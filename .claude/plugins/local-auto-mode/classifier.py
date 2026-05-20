#!/usr/bin/env python3
"""PreToolUse hook: local LLM-based auto mode classifier."""

from __future__ import annotations

import json
import sys
import urllib.request
import urllib.error
from pathlib import Path

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


def classify_with_llm(tool_name: str, tool_input: dict, cwd: str, transcript_tail: str) -> tuple[str, str]:
    """Call the local LLM and return (decision, reason)."""
    user_msg = build_user_prompt(tool_name, tool_input, cwd, transcript_tail)

    payload = json.dumps({
        "model": MODEL,
        "messages": [
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": user_msg},
        ],
        "max_tokens": 100,
        "temperature": 0,
    }).encode()

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
    body = json.loads(resp.read())
    text = body["choices"][0]["message"]["content"].strip()

    import re
    upper = text.upper()
    for token in ("ALLOW", "DENY", "ASK"):
        if re.search(rf'\b{token}\b', upper):
            reason = re.sub(rf'^.*?\b{token}\b[:\s]*', '', text, count=1).strip() or token.lower()
            return token.lower(), reason

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

    if decision is None:
        try:
            transcript_tail = read_transcript_tail(transcript_path) if transcript_path else ""
            decision, reason = classify_with_llm(tool_name, tool_input, cwd, transcript_tail)
        except (urllib.error.URLError, TimeoutError, OSError) as exc:
            decision, reason = "ask", f"classifier unreachable: {exc}"
        except Exception as exc:
            decision, reason = "ask", f"classifier error: {exc}"

    if decision == "deny":
        print(f"Blocked by local classifier: {reason}", file=sys.stderr)
        sys.exit(2)

    result = {
        "hookSpecificOutput": {
            "permissionDecision": decision,
        },
    }
    if decision == "ask":
        result["systemMessage"] = f"Local classifier deferred to user: {reason}"

    json.dump(result, sys.stdout)
    sys.exit(0)


if __name__ == "__main__":
    main()
