#!/usr/bin/env bash
# Cases for STEAM_FORCE_DESKTOPUI_SCALING, the one lever that makes the Steam
# client legible on a host whose compositor scale it cannot see.
#
# Steam's desktop UI is CEF, and it is an X11 client. hyprland.nix sets
# xwayland.force_zero_scaling, so on a scaled panel every X11 client gets the
# full pixel grid and no DPI hint -- the documented "crisp and small over blurry"
# trade. Steam ignores Xft.dpi and GDK_DPI_SCALE, so the only way back to a
# correct size is to hand CEF its own scale factor, which it lays out at rather
# than bitmap-upscaling into.
#
# What is actually being protected here, in two halves:
#
#   The value. It must equal the scale Hyprland gives eDP-1, because that is
#   exactly what it compensates for: the compositor scale the client is never
#   told about. So no case below hardcodes 2 -- each reads the host's real
#   monitor rule and expects the env var to agree. A literal that drifted from
#   laptopScale would size the UI wrongly the next time a panel changes, and the
#   failure is silent: Steam renders happily at any factor you give it.
#
#   The gate. It must be unset wherever the panel runs at scale 1. Those hosts
#   scale apps instead (hidpi.scale), so their Steam is already sized by the
#   toolkit path; forcing a second factor on top would multiply, which is the
#   double-scaling home.nix's "keep at most one of the two off 1 per host" rule
#   exists to prevent.
#
#   ./tests/steam-ui-scaling.sh            # tests this checkout
#   ./tests/steam-ui-scaling.sh /path/to/flake
set -u

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

VAR=STEAM_FORCE_DESKTOPUI_SCALING
D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

# Evaluate one option out of a host's config. nix eval's stderr is pure noise on
# a healthy box (substituter warnings, several lines per call) and the entire
# story on a broken one, so it is held back and printed only when the eval fails
# -- swallowing it outright would turn a broken checkout into a baffling
# "<unset>" that reads like a real missing value.
eval_option() { # eval_option <host> <option path>
  if ! nix eval --json "$FLAKE#homeConfigurations.$1.config.$2" 2>"$D/err"; then
    echo >&2; echo "nix eval failed for $1's $2:" >&2; cat "$D/err" >&2
    exit 1
  fi
}

# Read the var out of a host's evaluated session environment. Prints "<unset>"
# when the host does not define it, so absence is an ordinary expected value and
# not an eval failure a case has to special-case.
scaling_for() {
  eval_option "$1" home.sessionVariables \
    | jq -r --arg v "$VAR" '.[$v] // "<unset>"'
}

# The scale Hyprland gives the internal panel. hyprland.nix emits an eDP-1 rule
# only on a host whose panel wants scaling, so "no rule" is scale 1 -- the
# catch-all's value, and what that host's Steam already renders at correctly.
panel_scale_for() {
  eval_option "$1" wayland.windowManager.hyprland.settings.monitor \
    | jq -r 'map(select(.output == "eDP-1") | .scale) | (.[0] // 1) | tostring'
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
# shiori is the scaled-panel host (2880x1920 in 280x190mm, compositor at 2x);
# utsuho runs every output at 1, and setsuna scales apps rather than the
# compositor. Each host's expectation is derived, not written down.
for host in shiori utsuho setsuna; do
  scale=$(panel_scale_for "$host")
  if [ "$scale" = "1" ]; then
    check "$host (panel at 1x) leaves $VAR unset" "<unset>" "$(scaling_for "$host")"
  else
    check "$host (panel at ${scale}x) sets $VAR to match" "$scale" "$(scaling_for "$host")"
  fi
done

# --- the teeth ---------------------------------------------------------------
# Every expectation above is derived from the config, so a change that wiped
# laptopScale entirely would leave all three hosts at 1x, expect three unsets,
# and pass having checked nothing. Assert at least one scaled host still exists.
n=$((n+1))
if [ "$(panel_scale_for shiori)" != "1" ]; then
  echo "ok   at least one host still runs a scaled panel"
else
  echo "FAIL no host runs a scaled panel: every case above is now vacuous"
  fails=$((fails+1))
fi

echo
if [ "$fails" -eq 0 ]; then echo "all $n passed"; else echo "$fails of $n failed"; fi
exit $(( fails > 0 ))
