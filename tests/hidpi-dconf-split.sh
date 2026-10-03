#!/usr/bin/env bash
# Cases for the two-way split in how a host delivers its HiDPI font scale, and
# for the thing that split quietly took hostage.
#
# There are two ways to hand GTK a font scale, and a host must use exactly one:
#
#   text-scaling-factor, written to dconf. Read at runtime through a live dconf
#   D-Bus service, so it only works on a host that has one.
#
#   GDK_DPI_SCALE, an environment variable the toolkit reads directly. Works
#   anywhere, which is why it is the fallback for hosts with no dconf service.
#
# Setting both multiplies them -- the same double-scaling home.nix's "keep at
# most one of the two off 1 per host" rule exists to prevent, and just as silent:
# GTK renders happily at any product you give it.
#
# What is actually being protected here, in two halves:
#
#   The split itself. Exactly one mechanism on a host that scales apps, neither
#   on one that scales its compositor instead. No case hardcodes 1.25 or names a
#   host -- each derives the expected scale from QT_FONT_DPI, which home.nix
#   sets to floor(96*scale) precisely when the host scales apps. A literal that
#   drifted from hidpi.scale would mis-size fonts the next time a panel changes.
#
#   What the split dragged along. Choosing GDK_DPI_SCALE for a host must not
#   also switch off `dconf.enable`, because dconf.settings is NOT only this
#   repo's text-scaling-factor: home-manager's own gtk3 module contributes
#   color-scheme, gtk-theme, icon-theme, cursor-theme, cursor-size and font-name
#   into the same attrset, computed from the gtk.* options this config sets on
#   every desktop host. dconf.enable gates whether ANY of that is applied.
#   Gating it alongside a scaling decision throws out six keys to protect one,
#   and the failure is invisible on the surface that usually gets checked -- the
#   GTK ini files are still written and still correct, so gtk-3.0/settings.ini
#   reads dark while the XDG portal, which answers out of dconf, reports no
#   preference. Every portal-aware app then renders light.
#
#   ./tests/hidpi-dconf-split.sh            # tests this checkout
#   ./tests/hidpi-dconf-split.sh /path/to/flake
set -u

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

# Linux hosts only. mari is darwin: no dconf, no GTK, nothing here applies.
HOSTS=(utsuho kyuusaku setsuna shiori fujiwara)

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

# Evaluate one option out of a host's config. nix eval's stderr is pure noise on
# a healthy box (substituter warnings, several lines per call) and the entire
# story on a broken one, so it is held back and printed only when the eval fails
# -- swallowing it outright would turn a broken checkout into a baffling
# "<unset>" that reads like a real missing value.
eval_option() { # eval_option <host> <option path>
  if ! nix eval --max-jobs 0 --json "$FLAKE#homeConfigurations.$1.config.$2" 2>"$D/err"; then
    echo >&2; echo "nix eval failed for $1's $2:" >&2; cat "$D/err" >&2
    exit 1
  fi
}

# Cache each host's session environment and dconf block: every case below reads
# them two or three times, and a nix eval is seconds, not milliseconds.
load_host() { # load_host <host>
  [ -f "$D/$1.env" ] || eval_option "$1" home.sessionVariables > "$D/$1.env"
  [ -f "$D/$1.dconf" ] || eval_option "$1" dconf > "$D/$1.dconf"
}

session_var() { # session_var <host> <var>  -> value or "<unset>"
  jq -r --arg v "$2" '.[$v] // "<unset>"' < "$D/$1.env"
}

dconf_key() { # dconf_key <host> <key>  -> value or "<unset>"
  jq -r --arg k "$2" \
    '(.settings["org/gnome/desktop/interface"] // {})[$k] // "<unset>" | tostring' \
    < "$D/$1.dconf"
}

dconf_enabled() { jq -r '.enable | tostring' < "$D/$1.dconf"; }

# How many keys home-manager computed into the interface attrset for this host.
# Zero is a legitimate answer -- a headless host runs no GTK and declares none.
dconf_key_count() {
  jq -r '(.settings["org/gnome/desktop/interface"] // {}) | length' < "$D/$1.dconf"
}

# The app-side scale this host wants, derived rather than written down.
# home.nix sets QT_FONT_DPI to floor(96*scale) exactly when hidpi.enabled, so an
# unset var means "this host does not scale apps" and the quotient is the scale.
app_scale_for() {
  local dpi; dpi=$(session_var "$1" QT_FONT_DPI)
  if [ "$dpi" = "<unset>" ]; then echo 1; else
    awk -v d="$dpi" 'BEGIN { printf "%.6g", d/96 }'
  fi
}

check() { # check <what> <expected> <actual>
  n=$((n+1))
  if [ "$2" = "$3" ]; then
    echo "ok   $1"
  else
    echo "FAIL $1: expected '$2', got '$3'"; fails=$((fails+1))
  fi
}

for host in "${HOSTS[@]}"; do load_host "$host"; done

# --- the split ---------------------------------------------------------------
echo
for host in "${HOSTS[@]}"; do
  scale=$(app_scale_for "$host")
  gdk=$(session_var "$host" GDK_DPI_SCALE)
  tsf=$(dconf_key "$host" text-scaling-factor)

  if [ "$scale" = "1" ]; then
    check "$host (scales no apps) leaves GDK_DPI_SCALE unset" "<unset>" "$gdk"
    check "$host (scales no apps) writes no text-scaling-factor" "<unset>" "$tsf"
    continue
  fi

  # One mechanism, not two, not zero.
  set_count=0
  [ "$gdk" != "<unset>" ] && set_count=$((set_count+1))
  [ "$tsf" != "<unset>" ] && set_count=$((set_count+1))
  check "$host (scales apps ${scale}x) picks exactly one mechanism" 1 "$set_count"

  # And whichever it picked carries the real scale, not a drifted literal.
  picked=$gdk; [ "$picked" = "<unset>" ] && picked=$tsf
  [ "$picked" = "<unset>" ] || \
    check "$host's mechanism carries scale ${scale}" "$scale" \
      "$(awk -v v="$picked" 'BEGIN { printf "%.6g", v }')"
done

# --- what the split dragged along --------------------------------------------
# If home-manager computed any interface keys for a host, that host must apply
# them. This is the half that was broken: the keys were computed and discarded.
echo
for host in "${HOSTS[@]}"; do
  keys=$(dconf_key_count "$host")
  [ "$keys" -eq 0 ] && continue
  check "$host computed $keys dconf interface keys, so dconf is enabled" \
    true "$(dconf_enabled "$host")"
done

# The specific key this exists for. A desktop host declaring gtk.colorScheme
# must end up advertising it through the portal, which reads dconf.
echo
for host in "${HOSTS[@]}"; do
  cs=$(dconf_key "$host" color-scheme)
  [ "$cs" = "<unset>" ] && continue
  check "$host's color-scheme ($cs) is actually applied" \
    true "$(dconf_enabled "$host")"
done

# --- the teeth ---------------------------------------------------------------
# Every expectation above is conditional, so a change that emptied dconf.settings
# on every host -- or dropped gtk.colorScheme -- would skip each loop and pass
# having checked nothing. Pin that at least one host still reaches each branch.
echo
n=$((n+1))
if [ "$(dconf_key shiori color-scheme)" != "<unset>" ]; then
  echo "ok   at least one host still declares a dconf color-scheme"
else
  echo "FAIL no host declares a color-scheme: the cases above are now vacuous"
  fails=$((fails+1))
fi

n=$((n+1))
scaled=0
for host in "${HOSTS[@]}"; do
  [ "$(app_scale_for "$host")" = "1" ] || scaled=$((scaled+1))
done
if [ "$scaled" -gt 0 ]; then
  echo "ok   $scaled host(s) still scale apps"
else
  echo "FAIL no host scales apps: every split case above is now vacuous"
  fails=$((fails+1))
fi

echo
if [ "$fails" -eq 0 ]; then echo "all $n passed"; else echo "$fails of $n failed"; fi
exit $(( fails > 0 ))
