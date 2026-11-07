#!/usr/bin/env bash
# Cases for Orca's Claude and Cursor status hooks being declared in home-manager.
#
# Why this needs a test. Orca resumes a terminal agent after a restart from a
# record keyed by the agent's provider session id, and the only place that id
# comes from is the `session_id` in a hook payload. Orca installs those hooks by
# rewriting ~/.claude/settings.json and ~/.cursor/hooks.json -- here read-only
# store symlinks, so the install fails without a word and every agent pane comes
# back from a reboot as a bare shell. The entries are therefore vendored
# (nix/orca-{claude,cursor}-hooks.json, generated from Orca's own source) and
# merged into claudeBaseSettings and the Cursor hooks.json; this holds every
# host to them.
#
# Usage:
#   tests/orca-agent-hooks.sh    eval every host's Claude and Cursor hooks and test them

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
  # The generated file is only right for the Orca it was generated from: a bump
  # that changes Orca's wrapper or event list leaves these entries counted as
  # missing. Same contract as the skills rev in orca.nix -- bump, regenerate.
  have=$(nix eval --raw "$REPO#homeConfigurations.$h.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: (p.pname or "") == "orca-cli") ps)).version') \
    || { echo "could not evaluate $h's orca-cli version" >&2; exit 1; }
  gen=$(jq -r '.orcaVersion' "$VENDORED")
  if [ "$gen" = "$have" ]; then pass "$h: hooks were generated from this Orca ($have)"
  else fail "$h: hooks generated from Orca $gen, installed is $have" \
    "regenerate with nix/orca-agent-hooks-gen.ts"; fi
  # Orca gates events on the Claude version too (PostToolBatch, SessionEnd): a
  # Claude older than the one generated for may reject an event it does not
  # know -- and with it the whole settings.json, permissions included.
  claude=$(nix eval --raw "$REPO#homeConfigurations.$h.config.programs.claude-code.package.version") \
    || { echo "could not evaluate $h's claude-code version" >&2; exit 1; }
  for_claude=$(jq -r '.claudeVersion' "$VENDORED")
  if [ "$for_claude" = "$claude" ]; then pass "$h: hooks were generated for this Claude ($claude)"
  else fail "$h: hooks generated for Claude $for_claude, installed is $claude" \
    "regenerate with nix/orca-agent-hooks-gen.ts"; fi
  # Each vendored entry, whole, in the host's list for its event.
  if missing=$(jq -r --slurpfile v "$VENDORED" '
    . as $got | $v[0].hooks | to_entries[]
    | select(.value[0] as $want | ($got[.key] // []) | index($want) | not) | .key' <<<"$hooks"); then
    if [ -z "$missing" ]; then pass "$h: every Orca hook entry is in settings.hooks"
    else fail "$h: Orca hook entries missing" "$(paste -sd' ' <<<"$missing")"; fi
  else fail "$h: Orca hook entries" "jq could not compare them"; fi
  # Merged, not replaced: the hosts' own SessionStart hooks (kube session,
  # herdr) still run.
  if jq -e '[.SessionStart[].hooks[]?] | any(.command | test("herdr-agent-state"))' <<<"$hooks" >/dev/null; then
    pass "$h: the existing SessionStart hooks survive the merge"
  else fail "$h: SessionStart merge" "herdr's hook is gone"; fi
done

# ── Cursor ──────────────────────────────────────────────────────────────────
# Same story for cursor-agent: Orca rewrites ~/.cursor/hooks.json, which is a
# home.file here. Its command embeds the script's absolute path, so the vendored
# file carries @HOME@ and each host's homeDirectory is substituted in.
CURSOR="$REPO/nix/orca-cursor-hooks.json"
if [ -f "$CURSOR" ] && jq -e '.hooks | length > 0' "$CURSOR" >/dev/null 2>&1; then
  pass "nix/orca-cursor-hooks.json carries hook entries"
else
  fail "vendored Cursor hooks" "$CURSOR is missing or has no .hooks"
  echo "$n cases, $fails failed"; exit 1
fi
# A real home in the vendored file is the generating machine's, wrong for every
# other host -- and a username in a public repo.
if grep -q '@HOME@/\.orca/agent-hooks/cursor-hook\.sh' "$CURSOR" && ! grep -q '/home/' "$CURSOR"; then
  pass "vendored Cursor hooks name the script under @HOME@, no real home"
else fail "vendored Cursor home" "expected @HOME@ and no /home/ path"; fi
gen=$(jq -r '.orcaVersion' "$CURSOR")
have=$(jq -r '.orcaVersion' "$VENDORED")
if [ "$gen" = "$have" ]; then pass "Cursor and Claude hooks come from the same Orca ($gen)"
else fail "Cursor hooks generated from Orca $gen" "Claude's from $have; regenerate both"; fi

cursor_orca_hosts=0
for h in $(jq -r '.[]' <<<"$hosts"); do
  has_cursor=$(nix eval --raw "$REPO#homeConfigurations.$h.config.home.file" \
    --apply 'fs: if builtins.any (f: f.target == ".cursor/hooks.json") (builtins.attrValues fs) then "true" else "false"') \
    || { echo "could not evaluate $h's home.file" >&2; exit 1; }
  [ "$has_cursor" = true ] || continue
  orca=$(orca_on "$h") || exit 1
  [ "$orca" = true ] && cursor_orca_hosts=$((cursor_orca_hosts+1))
  text=$(nix eval --raw "$REPO#homeConfigurations.$h.config.home.file.\".cursor/hooks.json\".text") \
    || { echo "could not evaluate $h's Cursor hooks.json" >&2; exit 1; }
  home=$(nix eval --raw "$REPO#homeConfigurations.$h.config.home.homeDirectory") \
    || { echo "could not evaluate $h's homeDirectory" >&2; exit 1; }
  orca_entries=$(jq '[.hooks[][] | select(.command // "" | test("\\.orca/agent-hooks/cursor-hook\\.sh"))] | length' <<<"$text")
  # Unreachable while utsuho is the only Cursor host (it has Orca); kept for
  # the day a Cursor host without Orca joins.
  if [ "$orca" = false ]; then
    if [ "$orca_entries" = 0 ]; then pass "$h: no Orca Cursor hooks without Orca"
    else fail "$h: Orca Cursor hooks without Orca" "$orca_entries entries"; fi
    continue
  fi
  if missing=$(jq -r --slurpfile v "$CURSOR" --arg home "$home" '
    . as $got | $v[0].hooks | to_entries[]
    | select((.value[0] | .command |= gsub("@HOME@"; $home)) as $want
             | ($got.hooks[.key] // []) | index($want) | not) | .key' <<<"$text"); then
    if [ -z "$missing" ]; then pass "$h: every Orca Cursor hook entry is in hooks.json, at $home"
    else fail "$h: Orca Cursor hook entries missing" "$(paste -sd' ' <<<"$missing")"; fi
  else fail "$h: Orca Cursor hook entries" "jq could not compare them"; fi
  if jq -e '[.hooks.sessionStart[]?] | any(.command | test("herdr-agent-state"))' <<<"$text" >/dev/null \
     && [ "$(jq '.version' <<<"$text")" = 1 ]; then
    pass "$h: herdr's Cursor sessionStart hook and version 1 survive the merge"
  else fail "$h: Cursor hooks.json merge" "herdr's hook or version gone"; fi
done
# Otherwise a renamed isCursorHost skips every host and the section tests nothing.
if [ "$cursor_orca_hosts" -gt 0 ]; then pass "Cursor cases ran on $cursor_orca_hosts Orca host(s)"
else fail "Cursor cases" "no host manages .cursor/hooks.json with Orca"; fi

echo "$n cases, $fails failed"
[ "$fails" -eq 0 ]
