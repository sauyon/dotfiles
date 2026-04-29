#!/usr/bin/env bash
# Gemini CLI hook to audit tool calls with Rampart.
set -euo pipefail

# Read input from stdin
input=$(cat)

# If input is empty or null, allow by default to avoid jq errors
if [ -z "$input" ] || [ "$input" = "null" ]; then
  echo '{"decision": "allow"}'
  exit 0
fi

# Use remote rampart server — URL read from sops-managed config.yaml
_rampart_cfg="${XDG_CONFIG_HOME:-$HOME/.config}/rampart/config.yaml"
RAMPART_URL=$(grep '^serve_url:' "$_rampart_cfg" 2>/dev/null | awk '{gsub(/["'"'"']/, "", $2); print $2}' || true)
RAMPART_TOKEN=$(cat ~/.rampart/remote-token 2>/dev/null || echo "")
TIMEOUT=10

tool_name=$(echo "$input" | jq -r '.tool_name // empty')

if [ -z "$tool_name" ]; then
  echo '{"decision": "allow"}'
  exit 0
fi

# Map Gemini tools to Rampart tool types
case "$tool_name" in
  "run_shell_command") tool_type="exec" ;;
  "read_file"|"list_directory"|"glob") tool_type="read" ;;
  "write_file"|"replace"|"write_file_chunk") tool_type="write" ;;
  "web_fetch"|"google_web_search") tool_type="fetch" ;;
  *) tool_type="exec" ;;
esac

params=$(echo "$input" | jq -c '.tool_input // {}')
session=$(echo "$input" | jq -r '.session_id // "default"')
transcript=$(echo "$input" | jq -r '.transcript_path // empty')

# Extract recent conversation context for Rampart's LLM verifiers
context=""
if [ -n "$transcript" ] && [ -f "$transcript" ]; then
  # Take last 5 messages, truncate content to 200 chars each
  context=$(jq -r '(.messages // [])[-5:] | map("[\(.role)] \(.content | if type == "array" then map(.text) | join(" ") else . end | .[0:200])") | join("\n")' "$transcript" 2>/dev/null || true)
fi

auth_header=""
if [ -n "$RAMPART_TOKEN" ]; then
  auth_header="Authorization: Bearer ${RAMPART_TOKEN}"
fi

payload=$(jq -n \
  --arg agent "gemini-cli" \
  --arg tool "$tool_type" \
  --arg session "$session" \
  --argjson params "$params" \
  --arg context "$context" \
  '{agent: $agent, tool: $tool, session: $session, params: $params, task_context: $context}')

response=$(curl -s --max-time "$TIMEOUT" \
  -X POST "${RAMPART_URL}/v1/tool/${tool_type}" \
  -H "Content-Type: application/json" \
  ${auth_header:+-H "$auth_header"} \
  -d "$payload" \
  2>/dev/null) || true

# Fail-closed if Rampart is unreachable
if [ -z "$response" ]; then
  echo '{"decision": "deny", "reason": "Rampart server unreachable (fail-closed)"}'
  exit 0
fi

decision=$(echo "$response" | jq -r '.decision // "allow"')

# Gemini hooks only support allow/deny.
# If Rampart returns "ask", we must deny since we can't trigger a native prompt from a hook.
if [[ "$decision" == "deny" || "$decision" == "ask" ]]; then
  reason=$(echo "$response" | jq -r '.reason // "Blocked by Rampart policy"')
  [ "$decision" == "ask" ] && reason="Rampart requires manual approval: ${reason}"
  
  policies=$(echo "$response" | jq -r '(.matched_policies // []) | join(", ") // empty')
  [ -n "$policies" ] && reason="${reason} (policies: ${policies})"
  
  echo "{\"decision\": \"deny\", \"reason\": \"${reason}\"}"
else
  echo '{"decision": "allow"}'
fi
