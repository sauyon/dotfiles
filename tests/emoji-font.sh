#!/usr/bin/env bash
# Cases for the one font package that makes emoji render: Noto Color Emoji.
#
# Why this needs its own test. home.nix installs a lot of Noto -- the UI font is
# NotoSans Nerd Font, and the google-fonts subset names NotoSans, NotoSerif and
# NotoSansMono -- so "we have the Noto fonts" feels true while every emoji
# codepoint still draws as tofu. None of those families carry a single emoji
# glyph; Google ships emoji as a separate font, and nixpkgs as a separate
# package. The gap is invisible in the build and invisible in fc-list unless you
# grep for it, and it degrades silently: `fc-match emoji` with no emoji font
# installed does not error, it returns whatever Noto sorted first (here,
# Znamenny Musical Notation) and the text renders as boxes.
#
# What is actually being protected, in two halves:
#
#   The package. Every host gets it. Not gated on isDesktop, matching the
#   google-fonts set alongside it -- the output is ~10 MiB, and a headless host
#   that renders a mail or a PDF wants the glyphs too.
#
#   The family name. fontconfig's own 60-generic.conf is what binds the `emoji`
#   generic to a real font, and it does so by family name: it prefers exactly
#   "Noto Color Emoji". So installing the package is sufficient *only* while the
#   package ships that family. A repackage that renamed it, or that left only the
#   monochrome variant, would install fine and still leave every app in tofu --
#   so the last case reads the family out of the built font rather than trusting
#   the attribute name.
#
#   ./tests/emoji-font.sh            # tests this checkout
#   ./tests/emoji-font.sh /path/to/flake
set -u

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

# The family name fontconfig's 60-generic.conf prefers for the `emoji` generic.
FAMILY="Noto Color Emoji"

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

# Evaluate one option out of a host's config. nix eval's stderr is pure noise on
# a healthy box and the entire story on a broken one, so it is held back and
# printed only when the eval fails.
eval_option() { # eval_option <host> <option path> [extra nix eval args...]
  local host=$1 path=$2; shift 2
  if ! nix eval --json "$FLAKE#homeConfigurations.$host.config.$path" "$@" 2>"$D/err"; then
    echo >&2; echo "nix eval failed for $host's $path:" >&2; cat "$D/err" >&2
    exit 1
  fi
}

# The derivation names of a host's home.packages, one per line.
package_names_for() {
  eval_option "$1" 'home.packages' --apply 'map (p: p.name or "")' | jq -r '.[]'
}

check() { # check <what> <expected> <actual>
  n=$((n+1))
  if [ "$2" = "$3" ]; then
    echo "ok   $1"
  else
    echo "FAIL $1: expected '$2', got '$3'"; fails=$((fails+1))
  fi
}

# --- the cases ---------------------------------------------------------------
# Every host, including the headless ones: see "The package" above.
for host in fujiwara kyuusaku mari setsuna shiori utsuho; do
  got=$(package_names_for "$host" | grep -c '^noto-fonts-color-emoji-')
  check "$host installs a color emoji font" "1" "$got"

  # A font on disk that fontconfig never scans is not a rendered glyph. Each
  # host enables fontconfig, which is what links ~/.nix-profile/share/fonts in.
  check "$host enables fontconfig" "true" "$(eval_option "$host" fonts.fontconfig.enable)"
done

# --- the teeth ---------------------------------------------------------------
# Every case above matches on the attribute name, so a repackage that kept the
# name and dropped the color font would pass having checked nothing. Build the
# font and read the family out of it -- that string, not the attribute, is what
# fontconfig matches `emoji` against.
n=$((n+1))
if ! out=$(nix build --no-link --print-out-paths \
      "$FLAKE#homeConfigurations.shiori.pkgs.noto-fonts-color-emoji" 2>"$D/err"); then
  echo "FAIL could not build noto-fonts-color-emoji:"; cat "$D/err" >&2
  fails=$((fails+1))
elif fc-query --format '%{family}\n' \
       "$(find "$out" -name '*.ttf' -print -quit)" 2>/dev/null | grep -qxF "$FAMILY"; then
  echo "ok   the built font declares family '$FAMILY'"
else
  echo "FAIL built font does not declare family '$FAMILY': fontconfig's"
  echo "     60-generic.conf will not find it for the \`emoji\` generic"
  fails=$((fails+1))
fi

echo
if [ "$fails" -eq 0 ]; then echo "all $n passed"; else echo "$fails of $n failed"; fi
exit $(( fails > 0 ))
