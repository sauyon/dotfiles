#!/usr/bin/env bash
# Cases for the `even-terminal` derivation in even-terminal.nix -- Even Realities'
# bridge from a terminal coding agent to their G2 glasses, packaged with
# buildNpmPackage over the published npm tarball.
#
# Three things in that build can fail silently, producing a $out that installs
# clean and only dies in someone's hands:
#
#   1. The package's own `build` script is `rm -rf dist && tsc`, and its `prepack`
#      hook runs it. buildNpmPackage's install phase shells out to `npm pack`, so
#      without npmPackFlags = [ "--ignore-scripts" ] the published, prebuilt dist/
#      is deleted mid-install and the tree is packed empty.
#   2. node-pty ships no linux prebuild, so on Linux it is compiled here by
#      node-gyp. even-terminal loads it lazily -- only when a session spawns an
#      agent -- so a missing addon is invisible to every cheap smoke test.
#   3. There is no node on these hosts at all (not in pacman, not in the nix
#      profile). The bin's `#!/usr/bin/env node` must have been rewritten to the
#      store's node or nothing runs.
#
#   ./tests/even-terminal.sh                    # builds the package, then tests it
#   ./tests/even-terminal.sh /nix/store/...-even-terminal-0.10.5  # tests a given one
#
# The argument is a store path (the package root), not a binary, because half the
# cases are about what is inside lib/node_modules rather than about the CLI.
#
# Resolved from homeConfigurations.shiori specifically rather than from
# $(cat /etc/hostname): the package is in every host's list, and nothing here is
# host-specific, so there is no reason the cases should only run on one box. The
# flake ref comes from this script's own location rather than `.#`, so running it
# from a worktree tests THAT tree and not whichever one happens to be the cwd.
set -u

REPO=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

# Must match `version` in even-terminal.nix. Hardcoded on purpose: a bump that
# forgets to regenerate even-terminal-package-lock.json fails `npm ci`, but a bump
# that regenerates everything and silently builds the OLD tarball (a stale
# src.hash left in place) would pass every other case here.
WANT_VERSION=0.10.5

PKG="${1:-}"
if [ -z "$PKG" ]; then
  drv=$(nix eval --raw "$REPO#homeConfigurations.shiori.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: p.pname or "" == "even-terminal") ps)).drvPath') \
    || { echo "could not evaluate even-terminal for shiori" >&2; exit 1; }
  PKG="$(nix-store --realise "$drv" | tail -1)"
  echo "testing $PKG"
fi

BIN="$PKG/bin/even-terminal"
MOD="$PKG/lib/node_modules/@evenrealities/even-terminal"
[ -x "$BIN" ] || { echo "not executable: $BIN" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0; skips=0

ok()  { n=$((n + 1)); printf '  ok       %s\n' "$1"; }
bad() { n=$((n + 1)); fails=$((fails + 1)); printf '  FAILED   %s\n%s\n' "$1" "$2"; }
skip() { n=$((n + 1)); skips=$((skips + 1)); printf '  skipped  %s (%s)\n' "$1" "$2"; }

# Every CLI case runs with an empty environment and a PATH that resolves nothing.
# That is not paranoia about leakage -- it is case 3 above, made unskippable: if
# the shebang were still `/usr/bin/env node` these would all fail here even on a
# box that happens to have node installed.
run() {
  env -i HOME="$D/home" PATH=/var/empty "$@"
}
mkdir -p "$D/home"

echo
echo "--- the teeth: it runs with no node anywhere on PATH -----------------------"

out=$(run "$BIN" --version 2>&1); rc=$?
if [ $rc -eq 0 ] && [ "$out" = "$WANT_VERSION" ]; then
  ok "--version prints $WANT_VERSION under an empty PATH"
else
  bad "--version prints $WANT_VERSION under an empty PATH" \
    "    rc=$rc
    got: $(printf '%s' "$out" | sed 's/^/      /')"
fi

# The mechanism behind the case above, asserted directly so a failure says which
# half broke. A shebang still naming `env` would be a package that works only on a
# machine with node already installed -- which is none of these.
shebang=$(head -1 "$MOD/bin/cli.js")
case "$shebang" in
  '#!/nix/store/'*/bin/node)
    ok "bin/cli.js shebang points into the store, not /usr/bin/env" ;;
  *)
    bad "bin/cli.js shebang points into the store, not /usr/bin/env" \
      "    got: $shebang" ;;
esac

echo
echo "--- dist/ survived the prepack trap ----------------------------------------"

# `npm pack` running the package's prepack hook would leave dist/ gone. --version
# is served from bin/cli.js and can answer without dist/ at all, so it does NOT
# cover this; --help forces the yargs command table, which is built in dist/.
help=$(run "$BIN" --help 2>&1) || true
missing=""
for cmd in start config claude codex; do
  printf '%s\n' "$help" | grep -q "even-terminal $cmd" || missing="$missing $cmd"
done
if [ -z "$missing" ]; then
  ok "--help lists the start/config/claude/codex subcommands"
else
  bad "--help lists the start/config/claude/codex subcommands" \
    "    missing:$missing
    got:
$(printf '%s\n' "$help" | sed 's/^/      /')"
fi

# The same fact from the other side: an emptied dist/ can still leave the
# directory present. Count real modules rather than testing -d.
count=$(find "$MOD/dist" -name '*.js' 2>/dev/null | wc -l)
if [ "$count" -ge 20 ]; then
  ok "dist/ holds the published modules ($count .js files)"
else
  bad "dist/ holds the published modules" \
    "    found $count .js files under $MOD/dist, expected the published tree (30+)"
fi

echo
echo "--- node-pty: compiled, and it actually loads ------------------------------"

addon=$(find "$MOD/node_modules/node-pty" -name pty.node -print -quit 2>/dev/null)
if [ -n "$addon" ]; then
  ok "node-pty's native addon is in the closure"
else
  bad "node-pty's native addon is in the closure" \
    "    no pty.node under $MOD/node_modules/node-pty
    node-gyp's rebuild is the only thing that produces it on Linux; check the build log"
fi

# Present is not loaded. The addon is dlopen'd, so a build against the wrong node
# ABI or a missing libstdc++ shows up here and nowhere else. Uses the package's
# own node -- the one the shebang was patched to -- rather than anything on PATH.
node=${shebang#\#!}
if [ ! -x "$node" ]; then
  skip "node-pty dlopens under the package's own node" "could not read node from the shebang"
elif [ -z "$addon" ]; then
  skip "node-pty dlopens under the package's own node" "no addon to load"
else
  err=$(cd "$MOD" && env -i HOME="$D/home" PATH=/var/empty \
    "$node" --input-type=commonjs -e 'require("node-pty").open' 2>&1); rc=$?
  if [ $rc -eq 0 ]; then
    ok "node-pty dlopens under the package's own node"
  else
    bad "node-pty dlopens under the package's own node" \
      "    rc=$rc
$(printf '%s\n' "$err" | sed 's/^/      /')"
  fi
fi

echo
echo "--- the agent it drives is in the closure ----------------------------------"

# even-terminal spawns Claude Code as a child process, and by default that is the
# copy vendored by @anthropic-ai/claude-agent-sdk rather than anything on PATH
# (`--claude-use-system-cli` opts out). If the platform binary were missing the
# package would build, install, and then fail the first time a session starts.
vendored=$(find "$MOD/node_modules/@anthropic-ai" -name claude -type f -print -quit 2>/dev/null)
if [ -n "$vendored" ] && [ -x "$vendored" ]; then
  ok "the vendored claude agent binary is present and executable"
else
  bad "the vendored claude agent binary is present and executable" \
    "    found: ${vendored:-<none>} under $MOD/node_modules/@anthropic-ai"
fi

echo
printf '%d case(s), %d failed, %d skipped\n' "$n" "$fails" "$skips"
[ "$fails" -eq 0 ]
