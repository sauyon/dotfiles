"""Rampart policy enforcement plugin for Hermes Agent.

Calls the Rampart preflight API before each tool invocation, blocking
the call when the policy engine returns deny.  Fails open if the
Rampart server is unreachable so that agent operation is not disrupted
by transient network issues.
"""

import json
import logging
import os
import urllib.request
import urllib.error

logger = logging.getLogger(__name__)

RAMPART_URL = os.environ.get("RAMPART_URL", "http://localhost:9090")
RAMPART_TOKEN = os.environ.get("RAMPART_TOKEN", "")
FAIL_OPEN = os.environ.get("RAMPART_FAIL_OPEN", "true").lower() in ("1", "true", "yes")
TIMEOUT = float(os.environ.get("RAMPART_TIMEOUT", "5"))


def _preflight(tool: str, params: dict) -> dict:
    """Call the Rampart preflight endpoint and return the parsed response."""
    url = f"{RAMPART_URL.rstrip('/')}/v1/preflight/{tool}"
    body = json.dumps({
        "agent": "hermes-agent",
        "session": os.environ.get("HERMES_SESSION_ID", "unknown"),
        "params": params,
    }).encode()

    headers = {"Content-Type": "application/json"}
    if RAMPART_TOKEN:
        headers["Authorization"] = f"Bearer {RAMPART_TOKEN}"

    req = urllib.request.Request(url, data=body, headers=headers, method="POST")
    try:
        with urllib.request.urlopen(req, timeout=TIMEOUT) as resp:
            return json.loads(resp.read())
    except (urllib.error.URLError, OSError, json.JSONDecodeError) as exc:
        logger.warning("rampart preflight failed (%s); fail_open=%s", exc, FAIL_OPEN)
        if FAIL_OPEN:
            return {"allowed": True, "action": "allow", "message": "fail-open: connection error"}
        return {"allowed": False, "action": "deny", "message": f"rampart unreachable: {exc}"}


def pre_tool_call(tool_name: str, tool_input: dict, **kwargs) -> dict | None:
    """Hermes pre_tool_call hook.

    Returns None to allow, or a dict with {"action": "block", "message": ...}
    to prevent the tool call.
    """
    # Map hermes tool names to rampart tool types
    tool_type = "exec"
    params = {}

    if tool_name in ("Bash", "bash", "shell", "exec", "terminal"):
        params = {"command": tool_input.get("command", "")}
    elif tool_name in ("Read", "read", "cat"):
        tool_type = "read"
        params = {"path": tool_input.get("file_path", tool_input.get("path", ""))}
    elif tool_name in ("Write", "write", "Edit", "edit"):
        tool_type = "write"
        params = {"path": tool_input.get("file_path", tool_input.get("path", ""))}
    elif tool_name in ("WebFetch", "web_fetch", "fetch"):
        tool_type = "fetch"
        params = {"url": tool_input.get("url", "")}
    else:
        # For unknown tools, send as exec with tool name as command
        params = {"command": f"{tool_name} {json.dumps(tool_input)}"}

    decision = _preflight(tool_type, params)

    if not decision.get("allowed", True):
        msg = decision.get("message", "blocked by rampart policy")
        logger.info("rampart denied %s: %s", tool_name, msg)
        return {"action": "block", "message": f"[rampart] {msg}"}

    return None
