#!/usr/bin/env bash
# Cases for Orca's Claude status hooks being declared in home-manager.
#
# Why this needs a test. Orca resumes a terminal agent after a restart from a
# record keyed by the agent's provider session id, and the only place that id
# comes from is the `session_id` in a hook payload. Orca installs those hooks by
# rewriting ~/.claude/settings.json -- here a read-only store symlink, so the
# install fails without a word and every Claude pane comes back from a reboot
# as a bare shell. The entries are therefore vendored (nix/orca-claude-hooks.json,
# generated from Orca's own source) and merged into claudeBaseSettings; this
# holds every host to them.
#
# Usage:
#   tests/orca-agent-hooks.sh    eval every host's Claude settings and test them

set -u
set -o pipefail

fails=0; n=0
pass() { n=$((n+1)); echo "ok   $1"; }
fail() { n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1)); }

REPO=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
VENDORED="$REPO/nix/orca-claude-hooks.json"

if [ -f "$VENDORED" ] && jq -e '.hooks | length > 0' "$VENDORED" >/dev/null 2>&1; then
  pass "nix/orca-claude-hooks.json carries hook entries"
else
  fail "vendored hooks" "$VENDORED is missing or has no .hooks"
  echo "$n cases, $fails failed"; exit 1
fi
# The record needs SessionStart (the only event a resumed, idle session emits)
# and the turn events that carry session_id while it works.
for ev in SessionStart UserPromptSubmit Stop; do
  if jq -e --arg e "$ev" '.hooks[$e]' "$VENDORED" >/dev/null; then pass "vendored hooks include $ev"
  else fail "vendored $ev" "absent"; fi
done

orca_on() { # orca_on <host> -> true | false
  # orca-cli, not the `orca` GUI: a serve host (fujiwara) runs agents under
  # `orca serve` with no desktop app, and its panes need the hooks just the same.
  # orca.nix installs orca-cli on exactly the hosts home.nix calls hasOrca.
  local v
  v=$(nix eval --raw "$REPO#homeConfigurations.$1.config.home.packages" \
    --apply 'ps: if builtins.any (p: (p.pname or "") == "orca-cli") ps then "true" else "false"') \
    || { echo "could not evaluate $1's packages" >&2; exit 1; }
  case "$v" in true|false) echo "$v" ;; *) echo "orca_on $1: got '$v'" >&2; exit 1 ;; esac
}

hosts=$(nix eval --json "$REPO#homeConfigurations" --apply builtins.attrNames) \
  || { echo "could not list homeConfigurations" >&2; exit 1; }
for h in $(jq -r '.[]' <<<"$hosts"); do
  orca=$(orca_on "$h") || exit 1
  hooks=$(nix eval --json "$REPO#homeConfigurations.$h.config.programs.claude-code.settings.hooks") \
    || { echo "could not evaluate $h's Claude hooks" >&2; exit 1; }
  orca_entries=$(jq '[.[][] | select(any(.hooks[]?; .command | test("\\.orca/agent-hooks/claude-hook\\.sh")))] | length' <<<"$hooks")
  if [ "$orca" = false ]; then
    if [ "$orca_entries" = 0 ]; then pass "$h: no Orca hooks without Orca"
    else fail "$h: Orca hooks without Orca" "$orca_entries entries"; fi
    continue
  fi
  # Exactly Orca's own entry, byte for byte: Orca counts its hooks present by
  # matching the generated form, and a near miss reads to it as missing.
  missing=$(jq -r --slurpfile v "$VENDORED" '
    . as $got | $v[0].hooks | to_entries[]
    | select(.value[0] as $want | ($got[.key] // []) | index($want) | not) | .key' <<<"$hooks")
  if [ -z "$missing" ]; then pass "$h: every Orca hook entry is in settings.hooks"
  else fail "$h: Orca hook entries missing" "$(paste -sd' ' <<<"$missing")"; fi
  # Merged, not replaced: the hosts' own SessionStart hooks (kube session,
  # herdr) still run.
  if jq -e '[.SessionStart[].hooks[]?] | any(.command | test("herdr-agent-state"))' <<<"$hooks" >/dev/null; then
    pass "$h: the existing SessionStart hooks survive the merge"
  else fail "$h: SessionStart merge" "herdr's hook is gone"; fi
done

echo "$n cases, $fails failed"
[ "$fails" -eq 0 ]
