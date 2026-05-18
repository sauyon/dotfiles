---
name: linear-flow
description: Use in work repos (github.com/modular, modularml, bentoml) when starting substantive work ("help me X", "can you X", "add/create/implement/update/replace/fix Y"), when the user mentions Linear ("linear", "the ticket", "the issue", or an issue ID like MOD-1234), or before running `gh pr create`. Tracks the current Linear issue per Claude session in `~/.claude/linear-sessions/`, suggests existing issues or creates new ones, and injects Linear references into PR bodies with closing vs. non-closing magic words based on whether the PR fully resolves the issue. Skips personal repos (dotfiles, hyprland, emacs, AUR), status-only queries, and quick one-off commands.
---

# linear-flow

Tracks a Linear issue for each Claude Code session, manages the start/Linear/PR touch-points, and stays out of Linear's way the rest of the time. See `DESIGN.md` next to this file for full rationale.

## State

- **Session mapping file:** `~/.claude/linear-sessions/${CLAUDE_SESSION_ID}.json`
  - Schema: `{"issue": "MOD-1234", "team": "InfraPlatform", "cwd": "/...", "started": "ISO8601", "lastSeen": "ISO8601"}`
  - Or `{"skip": true, "cwd": "/..."}` if the user opted out for this session.
- **Config:** `~/.claude/linear-sessions/config.json`
  - Schema: `{"work_orgs": ["modular", "modularml", "bentoml"], "team_defaults": {"/home/sauyon/devel/mcloud": "InfraPlatform", ...}}`
  - Create with the defaults above if it doesn't exist.

Touch `lastSeen` on every activation. Lazily prune `*.json` whose `lastSeen` is older than 30 days.

## When activated

Decide which flow applies, then run it. The trigger comes from the skill description; this section is for execution.

### Flow A — starting work

Run when the user's request is a new piece of substantive work and there's no existing session mapping.

1. **Linear-tracked context check:**
   - **In a git repo:** read `git remote get-url origin` (fallback `upstream`). Extract the org segment.
     - If org is in `config.json` `work_orgs` → proceed.
     - Else → exit silently. Do NOT mention the skill to the user.
   - **Not in a git repo:** use judgment from the conversation. Work topics (mcloud, mammoth, bento, clusters, datadog dashboards, Linear-tracked services, Modular infra) → proceed. Personal topics (dotfiles, emacs, hyprland, AUR, ~/.config) → exit silently. Truly ambiguous → ask once: "Track this in Linear?" If no, write `{"skip": true, "cwd": "$PWD"}` to the mapping file and exit.

2. **Resolve the active issue** (in order — stop at first hit):
   1. Mapping file exists and is non-skip → use it.
   2. Branch name matches `<user>/<team>-<num>-*` (Linear convention) → fetch the issue with `mcp__claude_ai_Linear__get_issue`. Confirm with user: "Working on `MOD-1234: <title>`?" Default-yes. Write mapping.
   3. Scan `~/.claude/linear-sessions/*.json` for entries with `cwd == $PWD`. If the most recent (by `lastSeen`) is < 7 days old, surface: "Previous session here tracked `MOD-XYZ`. Continue with that issue?" If yes, copy issue/team into a fresh mapping file keyed by current `${CLAUDE_SESSION_ID}`.
   4. Search Linear: `mcp__claude_ai_Linear__list_issues` with `assignee: "me"`, `state: "started"` or `state: "unstarted"`, ordered by `updatedAt`, plus a `query` derived from the user's request. Show up to 3 candidates. Let the user pick one, create a new one, or skip.
   5. **Create new** (only with explicit user confirmation):
      - Infer team from cwd via `config.json` `team_defaults`. Confirm team before creating. If no match, ask.
      - Draft a title from the user's request. Show it for approval/edit before creating.
      - Use `mcp__claude_ai_Linear__save_issue` to create. Status: team default (don't auto-move to In Progress; Linear's git integration handles that on branch copy).
      - On success, update `team_defaults` in `config.json` so the inference improves.
   6. **Skip:** write `{"skip": true, "cwd": "$PWD"}` and exit.

3. Continue with the user's original request. The Linear interaction was a side-channel; the user came here to get work done.

### Flow B — explicit Linear mention

Run when the user references Linear directly: "linear", "the ticket", "the issue", an issue ID, "what's in MOD-X", "update the ticket", etc.

Handle the request directly via the Linear MCP. No prompt-then-act dance. If the user says "the issue" without specifying, default to the session's mapped issue (Flow A resolution).

### Flow C — PR creation

Run when about to call `gh pr create`, regardless of how that command was invoked.

1. **Resolve the issue** via the session mapping (same as Flow A resolution).
   - No mapping → ask: "No Linear issue linked to this session. Reference one for the PR? (issue ID / skip)"
   - Skip → no Linear reference in PR body.

2. **Decide the magic word** based on the conversation context. This is the judgment call the skill exists to encode:

   Use **closing** words (`Fixes`, `Closes`, `Resolves`) when:
   - The PR fully delivers the issue's stated scope.
   - The issue is bounded (a bug, a single feature, a discrete task).
   - There's no follow-up work explicitly planned.

   Use **non-closing** words (`Refs`, `Part of`, `Contributes to`, `Related to`) when:
   - The issue is an epic / parent and this PR is one of multiple.
   - The PR is partial work (infra-only, first half, behind-flag rollout, scaffolding).
   - The PR touches the area but doesn't resolve (adjacent refactor).
   - The user signaled more work to come ("first piece", "follow-up coming").

   When in doubt between closing and non-closing, prefer non-closing — a missed close is recoverable, an incorrect close requires manual issue reopening.

3. **Inject the reference** at the top of the PR body, before the `## Summary` section, as its own paragraph:

   ```
   Refs MOD-1234

   ## Summary
   ...
   ```

   Or `Fixes MOD-1234`, etc. Multiple refs are fine: `Fixes MOD-1234, MOD-5678`.

4. **Branch mismatch:** if `git branch --show-current` encodes a different issue than the session mapping, flag it: "Branch references `MOD-A` but session is tagged `MOD-B`. Which to put in the PR?" Branch-name issue wins by default — it's what Linear's GitHub integration reads.

5. **Do NOT call Linear MCP to move status.** Linear's GitHub integration handles transitions on PR open/merge. The skill's job here is the magic word, nothing else.

## Notes on judgment

- **Linking vs. closing:** Linear's GitHub integration auto-links PRs to issues by branch name OR PR title containing the issue ID — no body magic word required for the link. Body magic words exist to drive status transitions on merge. With Linear-style branch names, linking is free; the magic word is purely about "should this merge close the issue?"
- **Silent exits matter.** When the org check fails or the topic is personal, the skill must produce no user-visible output. Mentioning Linear in personal-repo work is noise.
- **Never auto-create issues.** All issue creates pass through user confirmation showing team + title.
- **Never alter Linear status from this skill.** That's Linear's GitHub integration's job.
