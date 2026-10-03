#!/bin/sh
# Vendored into ~/.minimax/plugins/herdr-agent-state/hooks/. Fires on every
# mcode lifecycle event declared in the same directory's hooks.json and,
# inside a herdr pane, publishes state/session/release RPCs to the local
# herdr socket. No-op unless HERDR_PANE_ID is set in mcode's environment,
# so inert outside a herdr pane.
#
# Action → herdr-RPC map (set by hooks.json's `command:` per event):
#   sessionstart → pane.report_agent_session (state=working; new identity)
#   working      → pane.report_agent        state=working
#   idle         → pane.report_agent        state=idle
#   blocked      → pane.report_agent        state=blocked
#   sessionend   → pane.release_agent       (clears the pane's agent slot)
#
# mcode's CLAUDE-format hook emitter sends Claude-Code-shaped JSON on
# stdin: { hook_event_name, session_id, transcript_path, cwd, source, ... }.
# Verified empirically with `mcode exec --permission off --prompt-mode tui`
# against mcode 0.6.2 that SessionStart, UserPromptSubmit, PreToolUse,
# PostToolUse, Stop, and Notification all fire and carry the expected
# keys. SessionEnd is registered as a hook so mcode picks it up the day
# it grows one, but today mcode never fires it on a normal exit — see
# the comment on `source` below for why that still ends up clean.
#
# The `source` value we send is `herdr:mcode`, even though the herdr docs
# at herdr.dev/add-herdr-support say to avoid the `herdr:` prefix (treating
# it as reserved for herdr's own integrations). On herdr 0.9.1 — what this
# repo runs — a probe of the API socket shows bare sources are
# acknowledged with `{"type":"ok"}` but the state changes are silently
# dropped, while `herdr:`-prefixed sources actually update
# pane.agent_status. The same prefix is also what lets herdr's exit
# safety net ("clear the agent slot once the pane returns to its shell
# prompt") recognise and remove our entries on mcode quit. `agent` stays
# `mcode` (not `herdr:mcode`) because that field is the panel display
# name, not the integration tag.
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
#
# Loop instead of `eval "export $herdr_env"` — eval prepends `export` to line
# 1 only, so subsequent `HERDR_FOO=val` lines run as command names and the
# values never land. `export "KEY=val"` sets and exports a single variable
# in one POSIX-portable builtin.
if [ -z "${HERDR_PANE_ID:-}" ] && [ -n "${PPID:-}" ] && [ -r "/proc/$PPID/environ" ]; then
  herdr_env="$(tr '\0' '\n' </proc/"$PPID"/environ 2>/dev/null \
    | grep -E '^HERDR_(ENV|SOCKET_PATH|PANE_ID|TAB_ID|WORKSPACE_ID)=' \
    || true)"
  if [ -n "$herdr_env" ]; then
    OLD_IFS="${IFS}"
    IFS='
'
    for line in $herdr_env; do
      [ -n "$line" ] || continue
      export "$line" 2>/dev/null || true
    done
    IFS="${OLD_IFS}"
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
    idle         ) printf '%s' "idle" ;;
    blocked      ) printf '%s' "blocked" ;;
  esac
}

method_for_action() {
  case "$1" in
    sessionstart) printf '%s' "pane.report_agent_session" ;;
    sessionend  ) printf '%s' "pane.release_agent"         ;;
    working | idle | blocked) printf '%s' "pane.report_agent" ;;
  esac
}

state="$(state_for_action "$action" || true)"
method="$(method_for_action "$action" || true)"
[ -n "$method" ] || exit 0

HERDR_ACTION="$action" HERDR_STATE="$state" HERDR_METHOD="$method" python3 - <<'PY'
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
method = os.environ.get("HERDR_METHOD", "")

if not pane_id or not socket_path or not method:
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

# `source` identifies our integration to herdr. The 0.9.1 server honours
# `herdr:`-prefixed sources (claude, opencode, etc. all use one) and
# silently drops bare-name reports on the floor. Prefix ours so the panel
# state actually changes and the exit-time safety net can find us again.
source = "herdr:mcode"
agent = "mcode"
request_id = f"{source}:{int(time.time() * 1000)}:{random.randrange(1_000_000):06d}"
report_seq = time.time_ns()

# release_agent is the bare shutdown signal: name only, no state or session.
if method == "pane.release_agent":
    request = {
        "id": request_id,
        "method": method,
        "params": {
            "pane_id": pane_id,
            "source": source,
            "agent": agent,
            "seq": report_seq,
        },
    }
else:
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
