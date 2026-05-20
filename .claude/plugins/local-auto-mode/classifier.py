#!/usr/bin/env python3
"""PreToolUse hook: local LLM-based auto mode classifier."""

from __future__ import annotations

import json
import os
import sys
import urllib.request
import urllib.error
from pathlib import Path

# home-manager symlinks each .py to its own nix store path, so Python's
# resolved sys.path[0] has no siblings. Add the unresolved script dir.
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from config import ENDPOINT, MODEL, TIMEOUT, get_api_key
from prompt import SYSTEM_PROMPT, build_user_prompt

# Tools that are always read-only — skip the LLM call.
ALWAYS_ALLOW = frozenset({
    "Read", "Glob", "Grep", "Agent", "AskUserQuestion",
    "TaskCreate", "TaskGet", "TaskList", "TaskUpdate", "TaskOutput",
    "WebSearch", "WebFetch",
})

# Patterns in Bash commands that should never be auto-approved.
HARD_DENY_PATTERNS = [
    "rm -rf /",
    "rm -rf ~",
    "mkfs.",
    "dd if=",
    "> /dev/sd",
    ":(){ :|:& };:",
]

FORCE_PUSH_PATTERNS = [
    "push --force",
    "push -f",
    "push --force-with-lease",  # still ask for main/master
]


def read_transcript_tail(path: str, max_chars: int = 2000) -> str:
    try:
        text = Path(path).read_text()
        return text[-max_chars:] if len(text) > max_chars else text
    except Exception:
        return ""


def fast_path(tool_name: str, tool_input: dict) -> str | None:
    """Return a decision string if we can skip the LLM, else None."""
    if tool_name in ALWAYS_ALLOW:
        return "allow"

    if tool_name == "Bash":
        cmd = tool_input.get("command", "")
        for pat in HARD_DENY_PATTERNS:
            if pat in cmd:
                return "deny"
        for pat in FORCE_PUSH_PATTERNS:
            if pat in cmd and ("main" in cmd or "master" in cmd):
                return "deny"

    return None


CLASSIFY_TOOL = {
    "type": "function",
    "function": {
        "name": "classify_result",
        "description": "Report the security classification result for the agent action",
        "parameters": {
            "type": "object",
            "properties": {
                "thinking": {
                    "type": "string",
                    "description": "Brief step-by-step reasoning.",
                },
                "shouldBlock": {
                    "type": "boolean",
                    "description": "Whether the action should be blocked (true) or allowed (false)",
                },
                "reason": {
                    "type": "string",
                    "description": "Brief explanation of the classification decision",
                },
            },
            "required": ["shouldBlock", "reason"],
        },
    },
}


REPRO_PATH = Path(os.path.expanduser("~/.cache/local-auto-mode/last-bad.json"))


def _looks_truncated(reason: str) -> bool:
    """Heuristic for responses that look like a leaked/cut special token."""
    if not reason:
        return True
    stripped = reason.strip()
    if len(stripped) < 10:
        return True
    if stripped.startswith(("<|", "<", "|", "```", "<bos>", "<start_of_turn>")):
        return True
    return False


def _save_repro(request_payload: dict, response_body: dict | str, note: str) -> None:
    try:
        REPRO_PATH.parent.mkdir(parents=True, exist_ok=True)
        # Strip the api key before saving the payload as a repro.
        REPRO_PATH.write_text(json.dumps({
            "note": note,
            "endpoint": f"{ENDPOINT}/chat/completions",
            "model": MODEL,
            "request": request_payload,
            "response": response_body,
        }, indent=2, default=str))
    except OSError:
        pass


def classify_with_llm(tool_name: str, tool_input: dict, cwd: str, transcript_tail: str) -> tuple[str, str]:
    """Call the local LLM and return (decision, reason)."""
    user_msg = build_user_prompt(tool_name, tool_input, cwd, transcript_tail)

    request_payload = {
        "model": MODEL,
        "messages": [
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": user_msg},
        ],
        "tools": [CLASSIFY_TOOL],
        "tool_choice": {"type": "function", "function": {"name": "classify_result"}},
        "max_tokens": 400,
        "temperature": 0,
    }
    payload = json.dumps(request_payload).encode()

    req = urllib.request.Request(
        f"{ENDPOINT}/chat/completions",
        data=payload,
        headers={
            "Content-Type": "application/json",
            "Authorization": f"Bearer {get_api_key()}",
        },
        method="POST",
    )

    resp = urllib.request.urlopen(req, timeout=TIMEOUT)
    raw = resp.read()
    try:
        body = json.loads(raw)
    except json.JSONDecodeError:
        _save_repro(request_payload, raw.decode("utf-8", errors="replace"), "non-JSON response body")
        return "ask", f"unparseable response: {raw[:80].decode('utf-8', errors='replace')}"

    msg = body["choices"][0]["message"]

    tool_calls = msg.get("tool_calls") or []
    if tool_calls:
        args = tool_calls[0].get("function", {}).get("arguments", "{}")
        try:
            parsed = json.loads(args) if isinstance(args, str) else args
            should_block = bool(parsed.get("shouldBlock"))
            reason = (parsed.get("reason") or "").strip()
            if _looks_truncated(reason):
                _save_repro(request_payload, body, f"suspicious reason field: {reason!r}")
            return ("ask" if should_block else "allow", reason or ("blocked" if should_block else "allowed"))
        except (json.JSONDecodeError, AttributeError):
            pass

    # Fallback: parse free-text shouldBlock if the model didn't honor tool_choice.
    text = (msg.get("content") or "").strip()
    import re
    m = re.search(r'"?shouldBlock"?\s*[:=]\s*(true|false)', text, re.IGNORECASE)
    if m:
        should_block = m.group(1).lower() == "true"
        reason_m = re.search(r'"?reason"?\s*[:=]\s*"?([^"\n]+)', text, re.IGNORECASE)
        reason = (reason_m.group(1).strip() if reason_m else "").rstrip('",') or ("blocked" if should_block else "allowed")
        return ("ask" if should_block else "allow", reason)

    _save_repro(request_payload, body, "no tool_call and no shouldBlock in content")
    return "ask", f"unparseable response: {text[:80]}"


def main() -> None:
    hook_input = json.loads(sys.stdin.read())
    tool_name = hook_input.get("tool_name", "")
    tool_input = hook_input.get("tool_input", {})
    cwd = hook_input.get("cwd", "")
    transcript_path = hook_input.get("transcript_path", "")

    # Fast path — no LLM needed.
    decision = fast_path(tool_name, tool_input)
    reason = "fast-path rule"

    warning = None
    if decision is None:
        try:
            transcript_tail = read_transcript_tail(transcript_path) if transcript_path else ""
            decision, reason = classify_with_llm(tool_name, tool_input, cwd, transcript_tail)
            if reason.startswith("unparseable response:"):
                warning = f"local classifier returned {reason}"
        except (urllib.error.URLError, TimeoutError, OSError) as exc:
            decision, reason = "ask", f"classifier unreachable: {exc}"
            warning = reason
        except Exception as exc:
            decision, reason = "ask", f"classifier error: {exc}"
            warning = reason

    if warning:
        print(f"local-auto-mode WARNING: {warning}", file=sys.stderr)
        if REPRO_PATH.exists():
            print(f"  repro saved to {REPRO_PATH}", file=sys.stderr)

    if decision == "deny":
        print(f"Blocked by local classifier: {reason}", file=sys.stderr)
        sys.exit(2)

    result = {
        "hookSpecificOutput": {
            "hookEventName": "PreToolUse",
            "permissionDecision": decision,
            "permissionDecisionReason": reason,
        },
    }

    json.dump(result, sys.stdout)
    sys.exit(0)


if __name__ == "__main__":
    main()
