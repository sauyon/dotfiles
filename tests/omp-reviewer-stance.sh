#!/usr/bin/env bash
# Case for what drovr's omp reviewer stance keeps out of a review: a checkout
# the reviewer did not write must not get code run under it, or reach its
# environment.
#
# omp's `--tools` does not bound this. With the bare flags, omp 18.12.2
# imports a checkout's .claude/tools/*.ts and spawns the servers in its
# .mcp.json at startup -- before the model says a word -- and the bun it runs
# on executes a bunfig.toml `preload` and loads .env*. This plants each of
# those in a scratch checkout, displaces what drovr's readonly_displace names
# (as drovr does before a reviewer launches), and runs the reviewer exactly as
# the generated drovr config for this host composes it: `command`, then
# `readonly_flag`, then a stand-in for drovr's findings module.
#
# It also checks the overlay's disabledProviders against the discovery
# providers the omp in that config actually ships, so a new upstream provider
# (or one dropped from the list) fails here rather than loading quietly.
#
# Not evaluation-only: it builds that config and the reviewer wrapper (no
# compile) and makes one short model call through the `modular` provider, so
# it needs omp's key in place, and the network.
#
#   ./tests/omp-reviewer-stance.sh [host]   # default: this host
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
host="${1:-$(hostname -s)}"

src=$(nix build --no-link --print-out-paths \
  "$repo#homeConfigurations.$host.config.xdg.configFile.\"drovr/config.toml\".source") \
  || { echo "could not build $host drovr config" >&2; exit 1; }
# Read with sed, not fromTOML: the file names store paths, which fromTOML
# refuses. tests/agent-clis.sh is what proves the file parses.
key() { sed -n "/^\[agents\.omp\]\$/,/^\[/s/^$1 = \(.*\)\$/\1/p" "$src"; }
cmd=$(key command | tr -d '"')
flag=$(key readonly_flag | sed 's/^"//; s/"$//')
displace=$(key readonly_displace | tr -d '[]",')
[ -n "$cmd" ] && [ -n "$flag" ] || { echo "no [agents.omp] command/readonly_flag in $src" >&2; exit 1; }
# `command` is a store path the config build already realised; nix-store -r
# makes sure of it when the config came from a substituter.
nix-store -r "${cmd%/bin/*}" >/dev/null || { echo "could not realise $cmd" >&2; exit 1; }

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

# ── Overlay coverage ─────────────────────────────────────────────────────────
# Providers allowed to stay on: `native` is .omp (displaced), and the other two
# load instruction text, no code. Everything else omp ships must be disabled.
allowed="native agents-md claude-md"
overlay=$(printf '%s\n' "$flag" | grep -oE -- '--config /nix/store/[^ ]+' | cut -d' ' -f2)
cli=$(grep -oE '/nix/store/[^" ]+/dist/cli\.js' "$cmd" | head -n1)
disc="$(dirname "$(dirname "$cli")")/src/discovery"
if [ -f "$overlay" ] && [ -d "$disc" ]; then
  shipped=$(grep -rhoE 'PROVIDER_ID = "[a-z0-9-]+"' "$disc" | cut -d'"' -f2 | sort -u)
  disabled=$(jq -r '.disabledProviders[]' "$overlay" | sort -u)
  open=""
  for p in $shipped; do
    case " $allowed " in *" $p "*) continue ;; esac
    printf '%s\n' "$disabled" | grep -qx -- "$p" || open="$open $p"
  done
  # An empty grep would compare nothing and pass; omp 18.13.1 ships 19.
  if [ "$(printf '%s\n' "$shipped" | grep -c .)" -lt 10 ]; then
    bad "found only $(printf '%s\n' "$shipped" | grep -c .) providers in $disc; the grep no longer matches omp's source"
  elif [ -z "$open" ]; then ok "overlay disables every shipped provider but: $allowed"
  else bad "overlay leaves enabled:$open"; fi
  if [ "$(jq -r '.mcp.enableProjectConfig' "$overlay")" = false ]; then ok "overlay turns project MCP off"
  else bad "overlay leaves project MCP config on"; fi
else
  bad "cannot find overlay ($overlay) or omp discovery sources ($disc)"
fi

# ── Planted checkout ─────────────────────────────────────────────────────────
t=$(mktemp -d); trap 'rm -rf "$t"' EXIT
mkdir -p "$t/repo/.claude/tools"
cat >"$t/repo/.claude/tools/planted_probe.ts" <<TS
import { writeFileSync } from "node:fs";
writeFileSync("$t/tool-ran", "x");
export default function () { return { name: "planted_probe", description: "probe", parameters: { type: "object", properties: {} }, async execute() { return { content: [{ type: "text", text: "probe" }] }; } }; }
TS
printf '{"mcpServers":{"planted":{"command":"sh","args":["-c","touch %s/mcp-ran"]}}}\n' "$t" \
  >"$t/repo/.mcp.json"
printf 'import { writeFileSync } from "node:fs"; writeFileSync("%s/preload-ran", "x");\n' "$t" \
  >"$t/repo/preload.ts"
printf 'preload = ["./preload.ts"]\n' >"$t/repo/bunfig.toml"
# .env.local is bun's read only, so the wrapper keeps it out. (.env itself
# omp loads too, with no switch: an accepted residual risk, see home.nix, so
# it is not planted here.)
printf 'PLANTED_ENV_LOCAL=from-dotenv-local\n' >"$t/repo/.env.local"
git -C "$t/repo" init -q

# drovr renames each readonly_displace entry aside before a reviewer starts.
for d in $displace; do
  [ -e "$t/repo/$d" ] && mv "$t/repo/$d" "$t/repo/$d.drovr-displaced"
done

# Stand-in for the findings module drovr generates per pass (pi_extension.rs
# upstream) and hands over with `--extension`: same import shape, and the
# stance's `--tools` names its tool, so omp refuses to start without it. It
# also records what of the planted dotenv reached the reviewer's environment.
cat >"$t/findings.ts" <<TS
import { Type } from "typebox";
import { writeFileSync } from "node:fs";
export default function (pi: any) {
  writeFileSync("$t/env-seen", process.env.PLANTED_ENV_LOCAL ?? "");
  writeFileSync("$t/bun-options", process.env.BUN_OPTIONS ?? "");
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
(cd "$t/repo" && timeout 300 "$cmd" -p $flag --extension "$t/findings.ts" \
  --model modular/google/gemma-4-31b-it \
  "Call the submit_findings tool once with summary set to ok, then reply done." \
  </dev/null >"$t/out" 2>&1)
rc=$?

if [ "$rc" -eq 0 ]; then ok "reviewer ran under the $host stance"
else bad "reviewer exited $rc under the $host stance:"; sed 's/^/  /' "$t/out"; fi
# The positive control: a stance that shut out the findings channel too would
# pass every case below and deliver no review at all.
if grep -q '"summary"' "$t/findings.json" 2>/dev/null; then ok "submit_findings delivered"
else bad "submit_findings never delivered"; fi
for m in tool-ran:".claude/tools module" mcp-ran:".mcp.json server" preload-ran:"bunfig.toml preload"; do
  if [ -e "$t/${m%%:*}" ]; then bad "planted ${m#*:} ran"
  else ok "planted ${m#*:} did not run"; fi
done
seen=$(cat "$t/env-seen" 2>/dev/null)
if [ ! -e "$t/env-seen" ]; then bad "findings stand-in never loaded; dotenv unchecked"
elif [ -z "$seen" ]; then ok "planted .env.local stayed out of the environment"
else bad "planted dotenv reached the reviewer: $seen"; fi

# omp's worker processes are `bun cli.js <worker>` in the same checkout and
# get no flags of their own: only an inherited BUN_OPTIONS keeps them off
# bunfig.toml and .env*. A single -p run starts none, so check the variable.
opts=$(cat "$t/bun-options" 2>/dev/null)
case "$opts" in
  *--no-env-file*--config=/nix/store/*) ok "BUN_OPTIONS reaches the reviewer's workers" ;;
  *) bad "BUN_OPTIONS not set for workers: '${opts}'" ;;
esac

[ "$fails" -eq 0 ]
