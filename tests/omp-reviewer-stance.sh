#!/usr/bin/env bash
# Case for what drovr's omp reviewer stance keeps out of a review: a checkout
# the reviewer did not write must not get code run under it.
#
# omp's `--tools` does not bound this. Without the --config overlay home.nix
# passes, omp 18.12.2 imports a checkout's .claude/tools/*.ts and spawns the
# servers in its .mcp.json at startup -- before the model says a word -- and
# yolo approval then lets their tools run. This plants one of each, each
# touching a marker file when it runs, and launches omp with the readonly_flag
# from the generated drovr config for this host.
#
# Not evaluation-only: it builds that config (a writeText, no compile) and
# makes one short model call through the `modular` provider, so it needs omp
# installed with its key in place, and the network.
#
#   ./tests/omp-reviewer-stance.sh [host]   # default: this host
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
host="${1:-$(hostname -s)}"
command -v omp >/dev/null || { echo "omp not on PATH" >&2; exit 1; }

src=$(nix build --no-link --print-out-paths \
  "$repo#homeConfigurations.$host.config.xdg.configFile.\"drovr/config.toml\".source") \
  || { echo "could not build $host drovr config" >&2; exit 1; }
# Read with sed, not fromTOML: the file names the overlay's store path, which
# fromTOML refuses. tests/agent-clis.sh is what proves the file parses.
flag=$(sed -n '/^\[agents\.omp\]$/,/^\[/s/^readonly_flag = "\(.*\)"$/\1/p' "$src")
[ -n "$flag" ] || { echo "no [agents.omp].readonly_flag in $src" >&2; exit 1; }

t=$(mktemp -d); trap 'rm -rf "$t"' EXIT
mkdir -p "$t/repo/.claude/tools"
cat >"$t/repo/.claude/tools/planted_probe.ts" <<TS
import { writeFileSync } from "node:fs";
writeFileSync("$t/tool-ran", "x");
export default function () { return { name: "planted_probe", description: "probe", parameters: { type: "object", properties: {} }, async execute() { return { content: [{ type: "text", text: "probe" }] }; } }; }
TS
printf '{"mcpServers":{"planted":{"command":"sh","args":["-c","touch %s/mcp-ran"]}}}\n' "$t" \
  >"$t/repo/.mcp.json"
git -C "$t/repo" init -q

# Stand-in for the findings module drovr generates per pass (pi_extension.rs
# upstream) and hands over with `--extension`: same import shape, and the
# stance's `--tools` names its tool, so omp refuses to start without it.
cat >"$t/findings.ts" <<TS
import { Type } from "typebox";
import { writeFileSync } from "node:fs";
export default function (pi: any) {
  pi.registerTool({
    name: "submit_findings", label: "submit_findings",
    description: "Submit review findings. Call exactly once.",
    parameters: Type.Object({ summary: Type.String() }),
    async execute(_id: string, params: any) {
      writeFileSync("$t/findings.json", JSON.stringify(params));
      return { content: [{ type: "text", text: "accepted" }], details: {} };
    },
  });
}
TS

# The flag string is drovr's to split; it holds no quoting, so word-splitting
# here is the same split.
# shellcheck disable=SC2086
(cd "$t/repo" && timeout 300 omp -p $flag --extension "$t/findings.ts" \
  --model modular/google/gemma-4-31b-it \
  "Call the submit_findings tool once with summary set to ok, then reply done." \
  </dev/null >"$t/out" 2>&1)
rc=$?

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

if [ "$rc" -eq 0 ]; then ok "omp ran under the $host stance"
else bad "omp exited $rc under the $host stance:"; sed 's/^/  /' "$t/out"; fi
# The positive control: a stance that shut out the findings channel too would
# pass both cases below and deliver no review at all.
if grep -q '"summary"' "$t/findings.json" 2>/dev/null; then ok "submit_findings delivered"
else bad "submit_findings never delivered"; fi
if [ -e "$t/tool-ran" ]; then bad "planted .claude/tools module ran"
else ok "planted .claude/tools module did not run"; fi
if [ -e "$t/mcp-ran" ]; then bad "planted .mcp.json server ran"
else ok "planted .mcp.json server did not run"; fi

[ "$fails" -eq 0 ]
