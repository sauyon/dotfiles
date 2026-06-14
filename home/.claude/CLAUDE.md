## Public Communication Discipline

ALWAYS ask for explicit confirmation when posting public communications, even when explicitly asked to.

NEVER create a pull request without explicitly asking the user for confirmation first.

NEVER post comments, reviews, or replies on a PR (including `gh pr comment`, `gh pr review`, `gh api .../comments`, or any other write to a PR's discussion) without explicitly asking the user for confirmation first. This includes pinging reviewers, leaving "ready for re-review" notes, and replying to review threads. Read-only operations (`gh pr view`, `gh pr checks`, fetching diffs/comments) are fine.

Resolving or dismissing existing review threads (marking conversations as resolved) does NOT require confirmation — do it when asked.

## Escalation

If a major roadblock or an unexpected decision needs to be made outside of the design/brainstorming phase, escalate to me rather than deciding it yourself. This includes hitting a blocker that changes the approach, working around a denied action, or anything with a security/trust or external side effect. Default to the safe path, surface the decision, and let me choose.

## MCloud / Modular Cloud Access

The `mcloud` CLI is the standing way to hit the Modular Cloud (Yatai/mcloud) admin API.

## Linear

When you file Linear tickets on my behalf (or assigned to me), default the team to `Customer Engineering` (`CENG-`). Only use a different team when the work is genuinely for that team (e.g., asking IT to action something).

## Secret Handling

NEVER cat or echo secrets, or otherwise run bash tool commands that will log them in the conversation. Get them directly in each tool call, eg with env vars `MY_SECRET=$(cat secretfile) command`.

Secrets in argv (e.g. `curl -H "Authorization: Bearer $TOKEN"`) or inline env are visible via `/proc/<pid>/cmdline` and `environ` — any later `ps -ef` leaks them. For curl, use `-K /path/to/config` with `header = "..."` lines, or `-H @file`. To check process liveness use `pgrep -f name` or `kill -0 <pid>`, not `ps -ef`.

## Killing Processes

NEVER kill by name, user, or pattern (`pkill`, `killall`, `kill` of a `pgrep` set). Only kill exact PIDs you launched; if you can't prove a PID is yours, ask. Never `kill -9`.

## Brevity

Be brief.
