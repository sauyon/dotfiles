#!/usr/bin/env bash
# Cases for which hosts get paru, the AUR helper in home.nix's home.packages.
#
# What is actually being protected here is one predicate, `isArchHost`, and the
# reason it is not `!isDarwin`:
#
#   kyuusaku is a Linux host whose distribution is not ours -- system/deploy says
#   so in the comment above its own four-host allow-list, and refuses to converge
#   pacman there for exactly this reason. A pacman frontend in that profile is a
#   tool with no package manager under it: it evaluates, it builds, it installs,
#   and it fails at the moment somebody runs it. `!isDarwin` would put it there,
#   which is why the list is its own list and why this file exists.
#
#   mari is darwin and gets it for the more obvious reason.
#
# So every case below is about host membership, and the teeth at the end are
# about the cases themselves: a change that dropped paru from home.packages
# altogether would leave every "does not have it" case passing, which is the
# failure mode a gate test is most likely to decay into.
#
#   ./tests/aur-helper.sh            # tests this checkout
#   ./tests/aur-helper.sh /path/to/flake
set -u

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

PKG=paru
D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

# The package names in one host's profile. `pname or name` because home.packages
# holds a mix -- nixpkgs derivations, this repo's writeShellScriptBin wrappers,
# flake inputs' packages -- and not all of them carry a pname.
#
# nix eval's stderr is noise on a healthy box and the whole story on a broken
# one, so it is held back and printed only on failure. Swallowing it would turn a
# broken checkout into a host that simply "does not have paru", which is a
# passing case for five of the six hosts here.
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

# --- the Arch hosts ----------------------------------------------------------
# The same four system/deploy converges pacman on. They have a pacman for paru
# to drive, and they are the hosts the AUR is for.
for host in utsuho setsuna shiori fujiwara; do
  check "$host (ours, Arch) has $PKG" yes "$host"
done

# --- the hosts that must not have it -----------------------------------------
# kyuusaku is the case the predicate exists for: Linux, and not ours.
check "kyuusaku (Linux, not ours) does not have $PKG" no kyuusaku
check "mari (darwin) does not have $PKG" no mari

# --- the teeth ---------------------------------------------------------------
# Both directions have to be represented, or the suite passes having checked
# nothing. Without the first, paru removed from home.nix entirely is six green
# cases; without the second, paru handed to every host is six green cases the
# other way. The four/two split above already asserts both, so these two say it
# in the form that survives someone editing the host loops.
n=$((n + 1))
if has_pkg shiori; then
  echo "ok   at least one host still has $PKG (cases are not vacuous)"
else
  echo "FAIL no host has $PKG: every 'does not have it' case above is vacuous"
  fails=$((fails + 1))
fi

n=$((n + 1))
if has_pkg kyuusaku; then
  echo "FAIL every host has $PKG: the host gate is gone, not merely wrong"
  fails=$((fails + 1))
else
  echo "ok   at least one host still lacks $PKG (the gate is doing something)"
fi

echo
if [ "$fails" -eq 0 ]; then echo "all $n passed"; else echo "$fails of $n failed"; fi
exit $((fails > 0))
