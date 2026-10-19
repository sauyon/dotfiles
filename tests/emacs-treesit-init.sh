#!/usr/bin/env bash
# Cases for the tree-sitter setup in home/emacs: .ts/.tsx must reach their ts
# modes even when the rest of init dies.
#
# Why this gets a test: the failure is silent. init.el loads pref-init before
# mode-init, and pref-init calls `defhydra` at top level. A daemon started while
# ~/.emacs.d/elpa lacked hydra (it was never in package-selected-packages, only
# ever pulled in as a dependency) errored there, so mode-init -- which held the
# grammar path -- never ran. treesit-extra-load-path stayed nil, Emacs's own
# typescript-ts-mode-maybe found no grammar, and every .ts file opened in
# fundamental-mode for a week with nothing but a *Warnings* line to show for it.
#
# Each case loads the repo's init.el into a throwaway HOME with an EMPTY elpa
# (so defhydra is void, exactly as on that boot), wrapping the load in
# condition-case the way startup does, then asserts on the surviving state.
#
#   ./tests/emacs-treesit-init.sh            # builds this host's emacs + grammars
#   ./tests/emacs-treesit-init.sh EMACS LIB  # tests an emacs binary and grammar lib/ you have
set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
if [[ $# -ge 2 ]]; then
  emacs=$1 grammars=$2
else
  cfg="$repo#homeConfigurations.$(cat /etc/hostname).config"
  emacs=$(nix build --no-link --print-out-paths "$cfg.services.emacs.package")/bin/emacs \
    || { echo "could not build emacs" >&2; exit 1; }
  # The grammars are an anonymous home.packages entry; pick it out by name.
  grammars=$(nix build --no-link --print-out-paths --impure --expr "
    let ps = (builtins.getFlake \"git+file://$repo\").homeConfigurations.\"$(cat /etc/hostname)\".config.home.packages;
    in builtins.head (builtins.filter (p: (p.name or \"\") == \"emacs-treesit-grammars\") ps)")/lib \
    || { echo "could not build emacs-treesit-grammars" >&2; exit 1; }
  echo "testing $emacs with $grammars"
fi
[[ -e $grammars/libtree-sitter-typescript.so ]] \
  || { echo "no typescript grammar in $grammars" >&2; exit 1; }

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/.emacs.d/lisp" "$tmp/.local/state/home-manager/gcroots/current-home/home-path"
cp "$repo/home/emacs/init.el" "$tmp/.emacs.d/"
cp "$repo"/home/emacs/lisp/*.el "$tmp/.emacs.d/lisp/"
ln -s "$grammars" "$tmp/.local/state/home-manager/gcroots/current-home/home-path/lib"
printf 'const x: number = 1;\n' > "$tmp/a.ts"
printf 'const x = <div/>;\n' > "$tmp/a.tsx"

out=$(HOME=$tmp "$emacs" --batch --eval '
(progn
  (condition-case e (load (expand-file-name "~/.emacs.d/init.el") nil t)
    (error (princ (format "init-error %S\n" e))))
  (dolist (f (list "~/a.ts" "~/a.tsx"))
    (with-current-buffer (find-file-noselect f)
      (princ (format "mode %s %s\n" (file-name-extension f) major-mode))))
  (princ (format "hydra-selected %s\n" (and (memq (quote hydra) package-selected-packages) t))))' 2>/dev/null)

fail=0
check() { # name expected-line
  if grep -qxF "$2" <<<"$out"; then echo "ok   $1"; else echo "FAIL $1 (want: $2)"; fail=1; fi
}
# The scenario must actually be the broken-elpa one, or the other cases prove nothing.
if grep -q '^init-error .*defhydra' <<<"$out"; then echo "ok   init dies at defhydra (scenario reproduced)"
else echo "FAIL init did not die at defhydra; scenario not reproduced"; fail=1; fi
check ".ts opens in typescript-ts-mode despite init error" "mode ts typescript-ts-mode"
check ".tsx opens in tsx-ts-mode despite init error" "mode tsx tsx-ts-mode"
check "hydra is in package-selected-packages" "hydra-selected t"
[[ $fail = 0 ]] || { printf -- '--- emacs output\n%s\n' "$out"; exit 1; }
