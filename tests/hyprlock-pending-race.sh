#!/usr/bin/env bash
# Guards patches/hyprlock-fix-lost-finished-event.patch, which fixes the race
# that freezes every cmd[update:N] label on the lock screen (hyprwm/hyprlock#1071).
#
# The patch is a workaround for an upstream bug, so the job here is not only
# "does it still apply" but "is it still needed". Case 1 reads the pinned
# nixpkgs hyprlock source and fails the moment upstream reorders those two
# statements itself -- that failure is the signal to delete the patch, its
# wiring in home.nix, and this file. Nothing else notices: a patch that has
# become a no-op still applies cleanly and still builds.
#
#   ./tests/hyprlock-pending-race.sh          # evaluates this host's hyprlock
#   HOST=utsuho ./tests/hyprlock-pending-race.sh
set -u

HOST="${HOST:-$(cat /etc/hostname)}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PATCHFILE="$ROOT/patches/hyprlock-fix-lost-finished-event.patch"
REL="src/renderer/AsyncResourceManager.cpp"

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

ok()   { n=$((n+1)); printf 'ok   %s\n' "$1"; }
bad()  { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %s\n     %s\n' "$1" "$2"; }

# The body of CAsyncResourceManager::enqueue, which is the whole subject here.
# Read from the file rather than the built binary: the ordering of two
# statements is a source property, and there is nothing to observe in the
# output of a compiler.
enqueue_body() { # $1 = source tree
  sed -n '/^void CAsyncResourceManager::enqueue(ResourceID/,/^}/p' "$1/$REL"
}

# Line number of the first match within the function body, or "" if absent.
lineof() { # $1 = body file, $2 = fixed string
  grep -nF -m1 "$2" "$1" | cut -d: -f1
}

echo "resolving hyprlock source for $HOST..."
drv=$(nix eval --raw ".#homeConfigurations.$HOST.pkgs.hyprlock.src.drvPath" 2>/dev/null) \
  || { echo "could not evaluate hyprlock.src for $HOST" >&2; exit 1; }
SRC=$(nix-store --realise "$drv" 2>/dev/null | tail -1) \
  || { echo "could not realise $drv" >&2; exit 1; }
[ -r "$SRC/$REL" ] || { echo "no $REL in $SRC -- did upstream move the file?" >&2; exit 1; }
echo "testing against $SRC"

# A writable copy, since patch(1) needs one and the store is read-only.
cp -r --no-preserve=mode,ownership "$SRC" "$D/src"

# 1. Is the patch still needed? Upstream hands the resource to the gatherer
#    thread on the first line and attaches the `finished` listener on the last,
#    so a resource that renders inside that window emits to nobody and the
#    widget stays pending forever.
enqueue_body "$D/src" > "$D/before"
g=$(lineof "$D/before" "m_gatherer.enqueue(resource);")
l=$(lineof "$D/before" "resource->m_events.finished.listenStatic(")
if [ -z "$g" ] || [ -z "$l" ]; then
  bad "upstream still has the bug" \
      "neither statement found in CAsyncResourceManager::enqueue -- upstream restructured it, re-read the patch by hand"
elif [ "$g" -lt "$l" ]; then
  ok "upstream still has the bug (enqueue at $g, listener at $l)"
else
  bad "upstream still has the bug" \
      "upstream now registers the listener first (listener at $l, enqueue at $g): hyprwm/hyprlock#1071 is fixed -- DROP $PATCHFILE, its entry in home.nix, and this test"
fi

# 2. The patch applies to that source. nixpkgs' patchPhase runs `patch -p1`, so
#    test with the same tool: `git apply` is stricter about context and would
#    fail on a patch the build would accept, and vice versa.
if [ ! -r "$PATCHFILE" ]; then
  bad "patch applies cleanly" "no such file: $PATCHFILE"
elif out=$(cd "$D/src" && patch -p1 --dry-run --force < "$PATCHFILE" 2>&1); then
  ok "patch applies cleanly"
  (cd "$D/src" && patch -p1 --force --silent < "$PATCHFILE" >/dev/null 2>&1)
else
  bad "patch applies cleanly" "$out"
fi

# 3. The patched order is the fixed one. Case 2 only says the hunk landed;
#    this says it landed the right way round.
enqueue_body "$D/src" > "$D/after"
g=$(lineof "$D/after" "m_gatherer.enqueue(resource);")
l=$(lineof "$D/after" "resource->m_events.finished.listenStatic(")
if [ -z "$g" ] || [ -z "$l" ]; then
  bad "patched enqueue registers the listener first" "statement missing after patching"
elif [ "$l" -lt "$g" ]; then
  ok "patched enqueue registers the listener first (listener at $l, enqueue at $g)"
else
  bad "patched enqueue registers the listener first" \
      "listener still at $l, gatherer enqueue at $g"
fi

# 4. The m_resources record is populated before the handoff too. onResourceFinished
#    drops any id it cannot find there, so a record written after the gatherer
#    could reintroduce the same permanent-pending stall by the other route.
r=$(lineof "$D/after" "m_resources[resourceID] = {resource, {widget}};")
if [ -z "$r" ]; then
  bad "resource is recorded before the gatherer sees it" "m_resources assignment missing after patching"
elif [ -n "$g" ] && [ "$r" -lt "$g" ]; then
  ok "resource is recorded before the gatherer sees it (record at $r, enqueue at $g)"
else
  bad "resource is recorded before the gatherer sees it" "record at ${r:-?}, gatherer enqueue at ${g:-?}"
fi

# 5. The patch is actually wired into the hyprlock this host installs. A patch
#    file that applies but is never listed builds a clean, unfixed hyprlock.
if wired=$(nix eval --json ".#homeConfigurations.$HOST.pkgs.hyprlock.patches" 2>/dev/null); then
  case "$wired" in
    *hyprlock-fix-lost-finished-event.patch*) ok "patch is wired into pkgs.hyprlock for $HOST" ;;
    *) bad "patch is wired into pkgs.hyprlock for $HOST" "patches = $wired" ;;
  esac
else
  bad "patch is wired into pkgs.hyprlock for $HOST" "could not evaluate pkgs.hyprlock.patches"
fi

echo
echo "$((n - fails))/$n passed"
[ "$fails" = 0 ]
