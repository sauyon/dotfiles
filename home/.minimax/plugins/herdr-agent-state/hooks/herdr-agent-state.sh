#!/bin/sh
# Vendored into ~/.minimax/plugins/herdr-agent-state/hooks/. Fires on every
# mcode lifecycle event declared in the same directory's hooks.json and,
# inside a herdr pane, publishes state/session RPCs to the local herdr
# socket. No-op unless HERDR_PANE_ID is set in mcode's environment, so
# inert outside a herdr pane.
#
# Action → state map (set by hooks.json's `command:` per event):
#   sessionstart → pane.report_agent_session (new identity, state=working)
#   working      → pane.report_agent        state=working
#   idle         → pane.report_agent        state=idle
#   blocked      → pane.report_agent        state=blocked
#
# mcode's CLAUDE-format hook emitter sends Claude-Code-shaped JSON on
# stdin: { hook_event_name, session_id, transcript_path, cwd, source, ... }.
# Verified empirically with `mcode exec --permission off` that
# SessionStart, UserPromptSubmit, PreToolUse, PostToolUse, Stop, and
# Notification all fire and carry the expected keys.
#
# mcode filters env when spawning hook subprocesses: only CLAUDE_*,
# MINIMAX_*, and a few basics (TERM, HOSTNAME, HOSTTYPE) are preserved;
# HERDR_* and anything else not on mcode's whitelist is dropped. We recover
# HERDR_* by reading the parent mcode process's /proc/<ppid>/environ —
# mcode itself inherits HERDR_* from the spawning herdr pane, so this
# round-trips through mcode's filter without modifying mcode.

set -eu

action="${1:-}"

# Recover HERDR_* from the parent (mcode)'s environ when not already in our
# own. Skip silently when running outside a herdr pane (no env to recover).
if [ -z "${HERDR_PANE_ID:-}" ] && [ -n "${PPID:-}" ] && [ -r "/proc/$PPID/environ" ]; then
  herdr_env="$(tr '\0' '\n' </proc/"$PPID"/environ 2>/dev/null \
    | grep -E '^HERDR_(ENV|SOCKET_PATH|PANE_ID|TAB_ID|WORKSPACE_ID)=' \
    || true)"
  if [ -n "$herdr_env" ]; then
    # shellcheck disable=SC2086
    eval "export $herdr_env"
  fi
fi

[ "${HERDR_ENV:-}" = "1" ] || exit 0
[ -n "${HERDR_SOCKET_PATH:-}" ] || exit 0
[ -n "${HERDR_PANE_ID:-}" ] || exit 0
command -v python3 >/dev/null 2>&1 || exit 0

state_for_action() {
  case "$1" in
    sessionstart) printf '%s' "working" ;;
    working)      printf '%s' "working" ;;
    idle)         printf '%s' "idle" ;;
    blocked)      printf '%s' "blocked" ;;
    *) exit 0 ;;
  esac
}

state="$(state_for_action "$action")"

HERDR_ACTION="$action" HERDR_STATE="$state" python3 - <<'PY'
import json
import os
import random
import socket
import sys
import time

pane_id = os.environ.get("HERDR_PANE_ID")
socket_path = os.environ.get("HERDR_SOCKET_PATH")
action = os.environ.get("HERDR_ACTION", "")
state = os.environ.get("HERDR_STATE", "")

if not pane_id or not socket_path or not state:
    raise SystemExit(0)

try:
    raw = sys.stdin.read()
    hook_input = json.loads(raw) if raw and raw.strip() else {}
except Exception:
    hook_input = {}


def first(*keys):
    for k in keys:
        v = hook_input.get(k)
        if isinstance(v, str) and v:
            return v
    return None


session_id = first("session_id", "conversation_id")
transcript_path = first("transcript_path")

# `source` is meaningful only on SessionStart (startup | resume | clear),
# where herdr uses it to distinguish a fresh session from a restored one.
session_start_source = None
if action == "sessionstart":
    src = hook_input.get("source")
    if isinstance(src, str) and src:
        session_start_source = src

source = "herdr:mcode"
agent = "mcode"
request_id = f"{source}:{int(time.time() * 1000)}:{random.randrange(1_000_000):06d}"
report_seq = time.time_ns()

params = {
    "pane_id": pane_id,
    "source": source,
    "agent": agent,
    "seq": report_seq,
    "state": state,
}
if session_id:
    params["agent_session_id"] = session_id
if transcript_path:
    params["agent_session_path"] = transcript_path
if session_start_source:
    params["session_start_source"] = session_start_source

# SessionStart is the canonical identity-carrying event — report the
# session via pane.report_agent_session first, so herdr binds the
# session_id for the pane before any working/idle transitions.
method = (
    "pane.report_agent_session" if action == "sessionstart" else "pane.report_agent"
)
request = {"id": request_id, "method": method, "params": params}

try:
    client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    client.settimeout(0.5)
    client.connect(socket_path)
    client.sendall((json.dumps(request) + "\n").encode())
    try:
        client.recv(4096)
    except Exception:
        pass
    client.close()
except Exception:
    pass
raise SystemExit(0)
PY
