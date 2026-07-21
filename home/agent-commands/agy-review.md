---
name: agy-review
description: Run an independent Gemini review of the current changes via the local Antigravity (`agy`) CLI and surface findings inline in Hunk. Use for "agy review", "gemini review", or a second-opinion / adversarial review of the working-tree diff. NOT for in-harness review — use /code-review for that.
---

# agy-review

Read and follow `~/.claude/skills/agy-review/SKILL.md`, then run its pipeline over the current change:

1. **Review** — pipe the working-tree diff into `agy` for an independent Gemini review:
   `git diff --no-ext-diff | agy --dangerously-skip-permissions -p "<review prompt from the skill>"`
2. **Transform** — map the JSON findings into a Hunk sidecar with the skill's `findings-to-agent-context.py`, writing `.gemini-review.agent.json` at the repo root.
3. **Render** — print the `hunk diff --agent-context .gemini-review.agent.json` command for the user to run. Do not launch the Hunk TUI yourself.

Relay Gemini's `verdict` faithfully (even if it contradicts your own assessment) and triage each finding before acting.

This deliberately sends the diff to the user's own Google/Gemini account — that is the intended purpose of this command.

If the user passed arguments, treat them as scope/target for the review (e.g. `--staged` for staged-only, a `main...HEAD` branch range, or specific files to focus on) and keep the stage-3 `hunk diff` target matching stage 1: $ARGUMENTS
