#!/usr/bin/env bash
# Cases for the claim that orca's sandbox resolves fonts the way the host does
# -- the claim orca.nix's extraBwrapArgs make.
#
# Why this needs a test. Orca runs in appimageTools' bwrap FHS env, and
# buildFHSEnv bind-mounts the *host's* /etc/fonts into it, on the NixOS
# assumption that its conf.d links point into /nix/store. On Arch they point
# into /usr/share/fontconfig, and inside the sandbox /usr is the env's own, so
# all of them dangle: no sans/sans-serif/Arial/system-ui aliases at all. Every
# family Chromium asks for then falls to the first font in sort order --
# Montserrat Thin, from the google-fonts set -- and the whole UI rendered in a
# thin display face. Only the sandbox shows it; the host's fontconfig is fine,
# which is why nothing caught it.
#
# Modelling the sandbox's mounts by hand is how the first version of this test
# went wrong (it passed against the broken build). So this runs fc-match
# through orca's real launcher: the same bwrap script, with only its final
# command -- the container init that would start the AppImage -- swapped for
# the rootfs's own fc-match. Patterns are escaped (`sans\-serif`): in
# fontconfig's pattern syntax a bare hyphen starts a size.
#
# Usage:
#   tests/sandbox-fonts.sh               build this host's orca and test it
#   tests/sandbox-fonts.sh <orca-path>   test a given build, e.g. the live one:
#                                        $(dirname $(dirname $(readlink -f ~/.nix-profile/bin/orca-ide-app)))

set -u
set -o pipefail

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }

ORCA="${1:-}"
if [ -z "$ORCA" ]; then
  FLAKE=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
  drv=$(nix eval --raw "$FLAKE#homeConfigurations.$(cat /etc/hostname 2>/dev/null || uname -n).config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: builtins.match ".*orca-[0-9.]+" (p.name or "") != null) ps)).drvPath') \
    || { echo "orca is not in home.packages here" >&2; exit 1; }
  ORCA=$(nix-store --realise "$drv" | tail -1) || { echo "could not build orca" >&2; exit 1; }
fi
ORCA=$(readlink -f "$ORCA")
# The nixGL wrapper execs the inner wrapper, which execs the bwrap launcher.
launcher=$(nix-store -qR "$ORCA" | while read -r p; do
  [ -f "$p/bin/.orca-ide-unwrapped" ] && echo "$p/bin/.orca-ide-unwrapped"; done | head -1)
[ -n "$launcher" ] || { echo "no .orca-ide-unwrapped in the closure of $ORCA" >&2; exit 1; }
echo "testing $launcher"

T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
# The launcher's last bwrap argument is the container init plus "$@". Drop the
# init so "$@" is the whole command; everything that sets up mounts is untouched.
sed -E 's|^(  )/nix/store/[a-z0-9]{32}-container-init "\$@"$|\1"$@"|' "$launcher" > "$T/launch"
grep -qE '^  "\$@"$' "$T/launch" \
  || { echo "could not find the container-init line in $launcher" >&2; exit 1; }
chmod +x "$T/launch"

sandbox_match() { # sandbox_match <pattern>
  "$T/launch" /usr/bin/fc-match -f '%{family[0]}' "$1" 2>/dev/null
}

got=$(sandbox_match 'Noto Sans')
[ "$got" = "Noto Sans" ] \
  || { echo "the sandbox cannot even see Noto Sans (got '$got'); harness is broken" >&2; exit 1; }

for c in 'sans\-serif:Noto Sans' 'sans:Noto Sans' 'Arial:' \
         'monospace:Noto Sans Mono'; do
  pat=${c%%:*}; want=${c#*:}
  got=$(sandbox_match "$pat")
  host=$(fc-match -f '%{family[0]}' "$pat" 2>/dev/null)
  # An empty want means "whatever the host says": metric aliases like Arial ->
  # Liberation Sans are the host's call, and the point is that they apply here.
  [ -n "$want" ] || want=$host
  if [ "$got" = "$want" ]; then
    pass "$pat -> $want in the sandbox"
  else
    fail "$pat" "resolves to '${got:-nothing}' in the sandbox, want '$want' (host: '$host')"
  fi
done

# Exact families, not matches. Chromium takes a named family only when
# fontconfig has it (a weak alias is refused), so "fc-match X" coming back with
# some other mono font is no comfort: orca's terminal then renders in a
# proportional fallback laid out on a monospace grid. DejaVu Sans Mono is orca's
# Linux default terminalFontFamily, a single family with no fallback.
for fam in 'DejaVu Sans Mono'; do
  got=$("$T/launch" /usr/bin/fc-list -f '%{family[0]}\n' "$fam" 2>/dev/null | sort -u | head -1)
  if [ "$got" = "$fam" ]; then
    pass "'$fam' exists in the sandbox (orca's default terminal font)"
  else
    fail "$fam" "not installed in the sandbox; Chromium falls back to a proportional font"
  fi
done

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
