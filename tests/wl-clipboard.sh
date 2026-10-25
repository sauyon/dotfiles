#!/usr/bin/env bash
# Cases for wl-copy/wl-paste on every Wayland host.
#
# It was missing from the profile while still appearing to be there:
# hyprshot and elephant both depend on wl-clipboard at their store paths, so a
# shell started from one of their environments found wl-copy, and a normal login
# shell got pacman's "may be found in extra/wl-clipboard". tridactyl's
# externalclipboardcmd runs it too. So the check is "wl-clipboard is in
# home.packages", not "something on PATH resolves wl-copy".
#
# Hosts are read out of the flake: every host with hyprland enabled is a
# Wayland host and is covered the day it lands.
#
#   ./tests/wl-clipboard.sh            # tests this checkout
#   ./tests/wl-clipboard.sh /path/to/flake
set -u
set -o pipefail

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

PKG=wl-clipboard

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }

# --max-jobs 0: this is an eval, and must stay one (see tests/hmeval.sh).
wayland=$(nix eval --max-jobs 0 --json "$FLAKE#homeConfigurations" \
  --apply 'hs: builtins.mapAttrs (_: h: h.config.wayland.windowManager.hyprland.enable) hs' 2>/dev/null) \
  || { echo "could not list homeConfigurations" >&2; exit 1; }
hosts=$(jq -r 'to_entries[] | select(.value) | .key' <<<"$wayland")
[ -n "$hosts" ] || { echo "this flake has no hyprland hosts" >&2; exit 1; }

for h in $hosts; do
  out=$(nix eval --max-jobs 0 --raw "$FLAKE#homeConfigurations.$h.config.home.packages" \
    --apply "ps: let m = builtins.filter (p: (p.pname or \"\") == \"$PKG\") ps;
             in if m == [] then \"\" else (builtins.head m).outPath" 2>/dev/null) \
    || { fail "$h" "home.packages did not evaluate"; continue; }
  if [ -z "$out" ]; then
    fail "$h" "$PKG is not in home.packages"
  else
    pass "$h: $PKG in home.packages"
  fi
done

echo "$((n-fails))/$n passed"
[ "$fails" -eq 0 ]
