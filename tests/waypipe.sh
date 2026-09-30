#!/usr/bin/env bash
# Cases for which hosts get waypipe, the Wayland forwarding proxy in home.nix's
# home.packages.
#
# What is being protected here is a *pair*, and that is the whole point. waypipe
# is not a tool a host uses alone: `waypipe ssh <host> <app>` runs one waypipe
# locally, next to the compositor that will show the window, and a second one on
# the far end, next to the application. Neither half is any use without the
# other, so "which hosts have waypipe" is really "which hosts can be either end
# of a forward", and a gate that drifts to one host is a gate that has silently
# turned the feature off.
#
# Both halves also have to be the SAME waypipe. The 0.10 rewrite (C -> Rust)
# changed the wire format, and waypipe refuses a version mismatch rather than
# degrading -- so the two ends agreeing is a property of them coming from one
# flake.lock, which is exactly what the version case at the end asserts.
#
# The teeth at the end are about the cases themselves: a change that dropped
# waypipe from home.packages altogether would leave every "does not have it"
# case passing, which is the failure mode a gate test is most likely to decay
# into.
#
#   ./tests/waypipe.sh            # tests this checkout
#   ./tests/waypipe.sh /path/to/flake
set -u

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

PKG=waypipe
# The two ends of the forward. Kept as a list here rather than spelled into the
# loop so the "both ends, or neither" case below can count it.
PAIR=(shiori utsuho)

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

# The package names in one host's profile. `pname or name` because home.packages
# holds a mix -- nixpkgs derivations, this repo's writeShellScriptBin wrappers,
# flake inputs' packages -- and not all of them carry a pname.
#
# nix eval's stderr is noise on a healthy box and the whole story on a broken
# one, so it is held back and printed only on failure. Swallowing it would turn
# a broken checkout into a host that simply "does not have waypipe", which is a
# passing case for four of the six hosts here.
packages_for() { # packages_for <host>
  if ! nix eval --json "$FLAKE#homeConfigurations.$1.config.home.packages" \
    --apply 'ps: builtins.map (p: p.pname or p.name or "?") ps' 2>"$D/err"; then
    echo >&2; echo "nix eval failed for $1's home.packages:" >&2; cat "$D/err" >&2
    exit 1
  fi
}

has_pkg() { # has_pkg <host>
  packages_for "$1" | jq -e --arg p "$PKG" 'any(. == $p)' >/dev/null
}

# The store path of one host's waypipe, so the two ends can be compared. Empty
# if that host has none.
waypipe_drv() { # waypipe_drv <host>
  nix eval --raw "$FLAKE#homeConfigurations.$1.pkgs.$PKG" 2>/dev/null || true
}

check() { # check <desc> <expected: yes|no> <host>
  n=$((n + 1))
  local desc=$1 want=$2 host=$3 got=no
  has_pkg "$host" && got=yes
  if [ "$want" = "$got" ]; then
    echo "ok   $desc"
  else
    echo "FAIL $desc: expected $PKG present=$want, got present=$got"
    fails=$((fails + 1))
  fi
}

# --- the pair ----------------------------------------------------------------
# The laptop with the display and the desktop with the GPU. Each is the other's
# far end, which is why neither one alone would be worth installing.
for host in "${PAIR[@]}"; do
  check "$host (one end of the forward) has $PKG" yes "$host"
done

# --- the hosts outside it ----------------------------------------------------
# Not "cannot run it" -- waypipe would work on any of these. They are not in the
# pair, and the gate exists so the closure carries ffmpeg/vulkan/gbm only where
# something asks for it.
check "setsuna (not in the pair) does not have $PKG" no setsuna
check "fujiwara (headless, not in the pair) does not have $PKG" no fujiwara
check "kyuusaku (headless, not ours) does not have $PKG" no kyuusaku
check "mari (darwin) does not have $PKG" no mari

# --- both ends, or neither ---------------------------------------------------
# The case the pair-shaped gate exists for: half a forward is not a working
# forward, and a one-host gate is the plausible drift.
n=$((n + 1))
present=0
for host in "${PAIR[@]}"; do has_pkg "$host" && present=$((present + 1)); done
if [ "$present" -eq "${#PAIR[@]}" ] || [ "$present" -eq 0 ]; then
  echo "ok   the pair is all-or-nothing ($present/${#PAIR[@]} have $PKG)"
else
  echo "FAIL only $present of ${#PAIR[@]} pair hosts have $PKG: half a forward forwards nothing"
  fails=$((fails + 1))
fi

# --- the two ends agree ------------------------------------------------------
# waypipe rejects a wire-version mismatch outright. Same flake.lock, same
# x86_64-linux, so the two store paths must be identical; if they ever diverge
# the forward fails at connect time with a version error, not at build time.
n=$((n + 1))
a=$(waypipe_drv "${PAIR[0]}"); b=$(waypipe_drv "${PAIR[1]}")
if [ -n "$a" ] && [ "$a" = "$b" ]; then
  echo "ok   both ends are the same $PKG ($(basename "$a"))"
else
  echo "FAIL the two ends differ: ${PAIR[0]}=${a:-<none>} ${PAIR[1]}=${b:-<none>}"
  fails=$((fails + 1))
fi

# --- the teeth ---------------------------------------------------------------
# Both directions have to be represented, or the suite passes having checked
# nothing. Without the first, waypipe removed from home.nix entirely is a green
# run; without the second, waypipe handed to every host is a green run the other
# way.
n=$((n + 1))
if has_pkg shiori; then
  echo "ok   at least one host still has $PKG (cases are not vacuous)"
else
  echo "FAIL no host has $PKG: every 'does not have it' case above is vacuous"
  fails=$((fails + 1))
fi

n=$((n + 1))
if has_pkg mari; then
  echo "FAIL every host has $PKG: the host gate is gone, not merely wrong"
  fails=$((fails + 1))
else
  echo "ok   at least one host still lacks $PKG (the gate is doing something)"
fi

echo
if [ "$fails" -eq 0 ]; then echo "all $n passed"; else echo "$fails of $n failed"; fi
exit $((fails > 0))
