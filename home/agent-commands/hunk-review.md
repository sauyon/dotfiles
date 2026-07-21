---
name: hunk-review
description: Drive a live Hunk diff review session via the `hunk session *` CLI — inspect focus, navigate files/hunks, reload contents, and leave inline review comments. Use when the user has a Hunk session running or wants to walk through / review a diff interactively.
---

# hunk-review

Read and follow `~/.claude/skills/hunk-review/SKILL.md`, then drive the user's live Hunk session over the current change.

Key rules from the skill:

1. **Never** run interactive `hunk diff` / `hunk show` yourself — the TUI belongs to the user. Control the live session only through `hunk session *` CLI commands.
2. Start with `hunk session list`, then `hunk session review --repo . --json` to understand file/hunk structure before reading raw patch text (`--include-patch` only when needed).
3. Navigate before commenting (`hunk session navigate ...`) so the user sees the code you're discussing.
4. Leave focused inline notes with `hunk session comment add` for one-offs, or batch several via `hunk session comment apply --stdin`.
5. If no session exists, ask the user to launch Hunk in their terminal first.

If the user passed arguments, treat them as scope/target for the review (e.g. specific files to focus on, or a `main...HEAD` range to `hunk session reload --repo . -- diff main...HEAD`): $ARGUMENTS
