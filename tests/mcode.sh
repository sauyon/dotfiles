#!/usr/bin/env bash
# Cases for the `mcode` derivation in mcode.nix -- MiniMax Code, packaged with
# buildNpmPackage over the published npm tarball.
#
# Four things in that build can fail silently, producing a $out that installs
# clean and only dies in someone's hands:
#
#   1. The Node pin. On Node 24 this CLI aborts about a second into the TUI, at
#      "Starting server...", with
#
#        node::RemoveEnvironmentCleanupHook(...) at ../../src/api/hooks.cc:142
#        Assertion failed: (env) != nullptr
#
#      Node 24's node_object_wrap.h grew cleanup hooks, so ~ObjectWrap calls
#      RemoveEnvironmentCleanupHook(v8::Isolate::GetCurrent(), ...), which CHECKs
#      that an Environment is current -- and the call arrives from ObjectWrap's
#      own weak callback, which V8 runs with no context entered. better-sqlite3's
#      Statement derives from node::ObjectWrap, so the first collection that
#      reaps a dead prepared statement kills the process. The header is compiled
#      INTO the addon, so the pin has to cover the build, not just the runtime.
#   2. better-sqlite3 is declared `optional`. An `npm ci` that skipped its native
#      build still yields a $out with both binaries in it, and only fails when
#      someone opens a session -- the session store is drizzle over a static
#      import of it.
#   3. ripgrep. mcode prefers @vscode/ripgrep's bundled binary and falls back to
#      `rg` on PATH; the fallback is only a fallback if something is there to
#      find, so postInstall puts one in the closure rather than letting file
#      search depend on whatever the user happens to have installed.
#   4. There is no node on these hosts at all (not in pacman, not in the nix
#      profile), so the launcher must resolve its own interpreter out of the
#      store or nothing runs.
#
#   ./tests/mcode.sh                                 # builds the package, tests it
#   ./tests/mcode.sh /nix/store/...-mcode-0.6.2      # tests a given one
#
# The argument is a store path (the package root), not a binary, because most of
# the cases are about what is inside lib/node_modules rather than about the CLI.
#
# Resolved from homeConfigurations.shiori specifically rather than from
# $(cat /etc/hostname): the package is in every host's list and nothing here is
# host-specific, so there is no reason the cases should only run on one box. The
# flake ref comes from this script's own location rather than `.#`, so running it
# from a worktree tests THAT tree and not whichever one happens to be the cwd.
set -u

REPO=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

# Must match `version` in mcode.nix. Hardcoded on purpose: a bump that forgets to
# regenerate mcode-package-lock.json fails `npm ci`, but a bump that regenerates
# everything and silently builds the OLD tarball (a stale src.hash left in place)
# would pass every other case here.
WANT_VERSION=0.6.2

# Must match the `nodejs` pin in mcode.nix. This is case 1, asserted as a
# tripwire rather than a cage: moving off 22 is allowed, but only once the
# gc-stress case below still passes on the new runtime. Bump both together and
# deliberately, never this line alone.
WANT_NODE_MAJOR=22

PKG="${1:-}"
if [ -z "$PKG" ]; then
  drv=$(nix eval --raw "$REPO#homeConfigurations.shiori.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: p.pname or "" == "mcode") ps)).drvPath') \
    || { echo "could not evaluate mcode for shiori" >&2; exit 1; }
  PKG="$(nix-store --realise "$drv" | tail -1)"
fi
echo "testing $PKG"

BIN="$PKG/bin/mcode"
WRAPPED="$PKG/bin/.mcode-wrapped"
MOD="$PKG/lib/node_modules/@minimax-ai/code"
[ -x "$BIN" ] || { echo "not executable: $BIN" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0; skips=0

ok()   { n=$((n + 1)); printf '  ok       %s\n' "$1"; }
bad()  { n=$((n + 1)); fails=$((fails + 1)); printf '  FAILED   %s\n%s\n' "$1" "$2"; }
skip() { n=$((n + 1)); skips=$((skips + 1)); printf '  skipped  %s (%s)\n' "$1" "$2"; }

# Every CLI case runs with an empty environment and a PATH that resolves nothing.
# That is not paranoia about leakage -- it is case 4 above, made unskippable: a
# launcher still looking for `node` on PATH would fail here even on a box that
# happens to have node installed. HOME is a fresh empty dir for a second reason:
# the wrapper reads an API key out of ~/.config/local-auto-mode/api-key, and
# these cases must not depend on that file existing or go near its contents.
run() {
  env -i HOME="$D/home" PATH=/var/empty "$@"
}
mkdir -p "$D/home"

echo
echo "--- it runs with no node anywhere on PATH ----------------------------------"

out=$(run "$BIN" --version 2>&1); rc=$?
if [ $rc -eq 0 ] && [ "$out" = "$WANT_VERSION" ]; then
  ok "--version prints $WANT_VERSION under an empty PATH"
else
  bad "--version prints $WANT_VERSION under an empty PATH" \
    "    rc=$rc
    got: $(printf '%s' "$out" | sed 's/^/      /')"
fi

echo
echo "--- the Node pin, which is what keeps the TUI alive ------------------------"

# The interpreter the launcher actually execs, read out of the inner wrapper
# makeWrapper generated. Asserting it here rather than trusting mcode.nix means
# a bump that changes `nodejs` without changing this file gets caught.
node=$(grep -o '/nix/store/[^"]*-nodejs-[^/]*/bin/node' "$WRAPPED" 2>/dev/null | head -1)
if [ -z "$node" ]; then
  bad "the launcher execs a node out of the store" \
    "    no /nix/store/...-nodejs-.../bin/node in $WRAPPED
    without it the pin in mcode.nix is not reaching the runtime at all"
elif [ ! -x "$node" ]; then
  bad "the launcher execs a node out of the store" \
    "    not executable: $node"
else
  ok "the launcher execs a node out of the store"

  major=$(run "$node" -p 'process.versions.node.split(".")[0]' 2>&1)
  if [ "$major" = "$WANT_NODE_MAJOR" ]; then
    ok "that node is $WANT_NODE_MAJOR.x (the pin in mcode.nix)"
  else
    bad "that node is $WANT_NODE_MAJOR.x (the pin in mcode.nix)" \
      "    got major: $major
    Node 24+ reintroduces the ObjectWrap/GC abort described at the top of this
    file. If this bump is deliberate, confirm the gc-stress case below still
    passes on the new runtime, then move WANT_NODE_MAJOR with it."
  fi
fi

echo
echo "--- better-sqlite3: compiled, and it survives a collection -----------------"

# Located via package.json, not via the .node: node-gyp leaves a second copy of
# the addon under build/Release/obj.target/, and `find -quit` may return either.
bsq=$(dirname "$(find "$MOD" -path '*/better-sqlite3/package.json' -print -quit 2>/dev/null)" 2>/dev/null)
if [ -f "$bsq/package.json" ]; then
  ok "better-sqlite3 is in the closure, not skipped as optional"
else
  bad "better-sqlite3 is in the closure, not skipped as optional" \
    "    no better-sqlite3/package.json under $MOD
    it is declared optional, so a failed native build becomes a silent skip"
fi

addon=$(find "$MOD" -name better_sqlite3.node -print -quit 2>/dev/null)
if [ -n "$addon" ]; then
  ok "its native addon was built"
else
  bad "its native addon was built" \
    "    no better_sqlite3.node under $MOD
    node-gyp's rebuild is the only thing that produces it here; check the build log"
fi

echo
echo "--- the teeth: the abort that killed every TUI launch ----------------------"

# Present and loadable is not enough, and neither is `--version`: the abort fires
# only when a collection reaps a dead prepared Statement, which no short-lived
# invocation gets far enough to do. mcode-gc-stress.cjs is the same script
# mcode.nix runs in installCheckPhase, reused here so a $out that arrived from a
# binary cache -- never locally built, never install-checked -- is held to it too.
stress="$REPO/mcode-gc-stress.cjs"
if [ ! -f "$stress" ]; then
  skip "a collection that reaps a prepared statement does not abort" \
    "missing $stress"
elif [ -z "$node" ] || [ ! -x "$node" ]; then
  skip "a collection that reaps a prepared statement does not abort" \
    "could not read node from the wrapper"
elif [ ! -f "$bsq/package.json" ]; then
  skip "a collection that reaps a prepared statement does not abort" \
    "no better-sqlite3 to exercise"
else
  err=$(run "$node" "$stress" "$bsq" 2>&1); rc=$?
  if [ $rc -eq 0 ]; then
    ok "a collection that reaps a prepared statement does not abort"
  else
    bad "a collection that reaps a prepared statement does not abort" \
      "    rc=$rc (134 is the SIGABRT this file is about)
$(printf '%s\n' "$err" | sed 's/^/      /' | head -12)"
  fi
fi

echo
echo "--- ripgrep is in the closure, not borrowed from the user's PATH -----------"

rgpath=$(grep -o '/nix/store/[^"'"'"']*-ripgrep-[^/]*/bin' "$BIN" 2>/dev/null | head -1)
if [ -n "$rgpath" ] && [ -x "$rgpath/rg" ]; then
  ok "the wrapper puts a store ripgrep on PATH"
else
  bad "the wrapper puts a store ripgrep on PATH" \
    "    found: ${rgpath:-<none>} in $BIN
    without it mcode's \`rg\` fallback depends on an out-of-closure tool"
fi

echo
printf '%d case(s), %d failed, %d skipped\n' "$n" "$fails" "$skips"
[ "$fails" -eq 0 ]
