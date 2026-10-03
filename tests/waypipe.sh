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

# The .drv of the waypipe actually in one host's profile, so the two ends can be
# compared. Empty if that host has none.
#
# Deliberately NOT `homeConfigurations.$1.pkgs.waypipe`: that is the bare nixpkgs
# package, which is identical across hosts no matter what the profile installs, so
# a wrap applied to one end only would still have compared equal. What has to
# match is what each end will actually exec.
waypipe_drv() { # waypipe_drv <host>
  nix eval --raw "$FLAKE#homeConfigurations.$1.config.home.packages" \
    --apply 'ps: let m = builtins.filter (p: (p.pname or "") == "waypipe") ps;
                 in if m == [ ] then "" else (builtins.head m).drvPath' 2>/dev/null || true
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
  echo "ok   both ends build the same $PKG ($(basename "$a"))"
else
  echo "FAIL the two ends differ: ${PAIR[0]}=${a:-<none>} ${PAIR[1]}=${b:-<none>}"
  fails=$((fails + 1))
fi

# --- the Vulkan ICD (the GPU path) -------------------------------------------
# waypipe 0.10+ rewrote DMABUF handling onto Vulkan (src/dmabuf.rs), so the
# `dmabuf: true` the binary advertises is only half the story: it also needs a
# Vulkan *driver* at runtime, and a nix-built loader on a non-NixOS host does not
# get one. The host's own manifest does not save it -- Arch's
# /usr/share/vulkan/icd.d/radeon_icd.json names the bare soname
# `libvulkan_radeon.so`, which a store-built waypipe has no path to resolve, so
# the loader finds a manifest and still comes up empty:
#
#   ERR waypipe-server src/dmabuf.rs:970:
#       Failed to create Vulkan instance: Unable to find a Vulkan driver
#
# That error is reported to the client, which drops the dmabuf protocols, and the
# far-end app then fails in GTK with "Failed to initialize GTK" or the even less
# helpful "Gtk: Failed to open display" -- no window, nothing naming Vulkan.
# `--no-gpu` papers over it by forcing shared memory. These cases are about the
# wrapper that fixes it instead, because the symptom points nowhere near the cause.
drv_for() { # drv_for <host>
  nix eval --raw "$FLAKE#homeConfigurations.$1.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: (p.pname or "") == "waypipe") ps)).drvPath' 2>"$D/err"
}

n=$((n + 1))
if drv=$(drv_for shiori) && store=$(nix-store --realise "$drv" 2>>"$D/err" | tail -1) && [ -n "$store" ]; then
  echo "ok   waypipe realises for shiori ($(basename "$store"))"
  BIN="$store/bin/waypipe"
else
  echo "FAIL could not realise shiori's waypipe:"; cat "$D/err" >&2
  fails=$((fails + 1)); BIN=""
fi

# The wrapper has to be a wrapper. Before this change $out/bin/waypipe is the
# bare ELF and every assertion below is about a string that is simply absent.
# Read the value the wrapper really produces, by running the wrapper with its
# final `exec` rewritten into a printenv. Everything above that line -- the
# prefix arithmetic makeWrapper emits, which is five lines of bash parameter
# expansion per entry -- runs verbatim, so this tests the resulting value
# including order and de-duplication. A regex over the script does not: the
# emitted code mentions VK_ICD_FILENAMES on lines that contain no path at all,
# and an earlier version of this test matched one of those and passed vacuously.
icd_value=""
n=$((n + 1))
if [ -n "$BIN" ]; then
  icd_value=$(sed 's#^exec .*#printenv VK_ICD_FILENAMES#' "$BIN" \
    | env -u VK_ICD_FILENAMES bash 2>/dev/null | tail -1)
fi
if [ -n "$icd_value" ]; then
  echo "ok   wrapper exports VK_ICD_FILENAMES when the environment had none"
else
  echo "FAIL wrapper exports no VK_ICD_FILENAMES: the Vulkan/DMABUF path cannot initialise"
  fails=$((fails + 1))
fi

# Every manifest named must exist. A path that 404s is the failure mode a string
# match cannot see: the var is set, the loader reads nothing, and the error is
# byte-identical to having no wrapper at all.
n=$((n + 1))
if [ -n "$icd_value" ]; then
  missing=""
  IFS=':' read -r -a icds <<< "$icd_value"
  for f in "${icds[@]}"; do
    case "$f" in /nix/store/*) [ -r "$f" ] || missing="$missing $f" ;; esac
  done
  if [ -z "$missing" ]; then
    echo "ok   every store ICD manifest named exists (${#icds[@]} entries)"
  else
    echo "FAIL ICD manifests named but absent:$missing"
    fails=$((fails + 1))
  fi
else
  echo "FAIL no ICD list to check"; fails=$((fails + 1))
fi

# Both GPUs of the pair, from ONE derivation. shiori is Intel (anv) and utsuho is
# AMD (radv), and the two ends must stay the same store path -- see the case
# above -- so the wrapper cannot be keyed on the host's gpu attr. Listing both is
# what lets one package serve both ends; the loader skips an ICD whose device is
# not present.
for want in intel_icd radeon_icd; do
  n=$((n + 1))
  case "$icd_value" in
    *"$want"*) echo "ok   ICD list covers $want" ;;
    *) echo "FAIL ICD list does not cover $want: one end of the pair has no driver"
       fails=$((fails + 1)) ;;
  esac
done

# The wrap must not have cost us the feature it exists to serve.
n=$((n + 1))
if [ -n "$BIN" ] && "$BIN" --version 2>/dev/null | grep -q "dmabuf: true"; then
  echo "ok   wrapped waypipe still reports dmabuf: true"
else
  echo "FAIL wrapped waypipe does not report dmabuf: true"
  fails=$((fails + 1))
fi

# --prefix, not --set: a host that grows a working system ICD must keep it. The
# store entries have to come first, though, or the broken host manifest is what
# the loader tries first.
n=$((n + 1))
if [ -n "$BIN" ]; then
  combined=$(sed 's#^exec .*#printenv VK_ICD_FILENAMES#' "$BIN" \
    | VK_ICD_FILENAMES=/sentinel/host_icd.json bash 2>/dev/null | tail -1)
  case "$combined" in
    /nix/store/*:*/sentinel/host_icd.json) echo "ok   pre-set VK_ICD_FILENAMES is kept, after ours" ;;
    *"/sentinel/host_icd.json") echo "FAIL ours do not come first: $combined"; fails=$((fails + 1)) ;;
    *) echo "FAIL a pre-set VK_ICD_FILENAMES was discarded: $combined"; fails=$((fails + 1)) ;;
  esac
else
  echo "FAIL no wrapper to check prefix behaviour"; fails=$((fails + 1))
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
