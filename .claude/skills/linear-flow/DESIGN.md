# linear-flow — Design

## Goal

A personal Claude Code skill that:

1. Prompts to find or create a Linear issue when starting substantive work.
2. Maintains a durable per-session→issue mapping that survives `--resume` and worktrees.
3. Auto-injects Linear references into PR bodies on `gh pr create`, exercising judgment on closing vs. non-closing magic words.
4. Handles explicit Linear requests directly via MCP.

Built fresh; no off-the-shelf solution covers all four pieces. The session→issue mapping is the genuinely novel piece — no community skill ships it.

## Triggers (when Claude self-activates the skill)

Skill `description` field signals invocation on:

- **Starting substantive work** — characteristic openers: "help me X", "can you X", or direct imperatives ("add X", "create X", "implement X", "update X", "replace X", "set up X"). Bug investigations after the fix scope is clear.
- **Explicit Linear mentions** — "linear", "the ticket", "the issue", any issue ID like `MOD-1234`.
- **PR creation** — about to call `gh pr create`.

**Does NOT trigger on:**

- Status/diagnostic-only requests with no fix scoped ("what's the status of X", "is X up", "why is X slow"). Once a fix is in motion, the next "help me" / "can you" triggers normally.
- Quick one-off commands (`reload hyprland`, `commit and push`, `pull`).
- Personal config (dotfiles, waybar, hyprland, emacs, ~/.config) — caught by the org check.
- Slash command invocations and meta-questions about Claude itself.

The skill's content encodes "is this Linear-worthy?" as judgment, not a regex match.

## Durable session→issue mapping

**Layout:**

- `~/.claude/linear-sessions/<session-uuid>.json` — `{issue, team, cwd, started, lastSeen, skip?}`. One file per tagged session.
- `~/.claude/linear-sessions/config.json` — `{work_orgs: ["modular", "modularml", "bentoml"], team_defaults: {...}}`. User-editable.

**Mechanism:**

Claude Code substitutes `${CLAUDE_SESSION_ID}` in SKILL.md content at load time. The skill instructions reference `~/.claude/linear-sessions/${CLAUDE_SESSION_ID}.json` directly — no SessionStart hook, no sentinel file, no cwd encoding layer.

**Resolution order on activation:**

1. Read `~/.claude/linear-sessions/${CLAUDE_SESSION_ID}.json`. If present, that's the active mapping.
2. Miss → check current branch for Linear pattern (`<user>/<team>-<num>-*`). If matched, fetch issue, confirm, write mapping.
3. Miss → scan `~/.claude/linear-sessions/*.json` for entries with `cwd == $PWD`, surface the most recent as "continue this?" suggestion.
4. Miss → search Linear via MCP / ask user / create new.
5. Result → write to `~/.claude/linear-sessions/${CLAUDE_SESSION_ID}.json`.

**What this gives us:**

- **Resume:** same UUID → same mapping. Free.
- **New session, same cwd:** different UUID, no mapping. Step 3 surfaces recent mapping for the same cwd as a suggestion.
- **Concurrent same-cwd sessions on different issues:** each has its own UUID-keyed mapping. No collision.
- **Worktrees:** different cwd, different UUID, fully isolated.
- **Cleanup:** lazy GC at activation — prune `*.json` whose `lastSeen` > 30 days.

## Starting-work flow

When the skill activates on a new-work trigger and the session has no mapping:

1. **Linear-tracked context check:**
   - In a git repo: `git remote get-url origin` (fallback `upstream`). Work orgs (`modular`, `modularml`, `bentoml`) → proceed. Other orgs → exit silently.
   - Not in a repo: use judgment from conversation context. Work topics (clusters, mcloud, mammoth, bento, datadog dashboards, Linear-tracked services) → proceed. Personal (dotfiles, emacs, hyprland, AUR) → exit. Ambiguous → single quick "Track in Linear?" prompt.

2. **Branch hint:** branch matches `<user>/<team>-<num>-*` → fetch that issue, confirm: "Working on `MOD-1234: Fix auth race`?" Default-yes.

3. **Search existing issues:** `list_issues` with `assignee: me`, recent first, plus a query derived from the user's request. Surface up to 3 candidates:

   > Found these open issues that might match:
   > - `MOD-1234: Fix auth race` (In Progress)
   > - `MOD-1198: Auth flakiness` (Todo)
   >
   > Pick one, create new, or skip Linear for this.

4. **Create new** (only with explicit user confirmation):
   - Team: infer from cwd via `config.json` `team_defaults` (e.g., `~/devel/mcloud` → InfraPlatform). Confirm before creating. Skill learns over time by appending to `team_defaults`.
   - Title: drafted from the request, shown for approval/edit before submit.
   - Status: leave at team default. Don't auto-move to In Progress — Linear's git integration handles transitions when the branch name is copied.

5. **Skip:** user picks skip → write `{skip: true}` to the mapping file so the skill doesn't re-prompt this session.

**Never auto-create silently.** All creates go through user confirmation.

## PR creation flow

When the skill is consulted before `gh pr create`:

1. **Resolve the issue** via the session mapping. No mapping → ask: "No Linear issue linked to this session. Reference one for the PR? (issue ID / skip)"

2. **Decide the magic word** — judgment based on conversation context:

   **Closing** (`Fixes`, `Closes`, `Resolves`) when:
   - PR fully delivers the issue's scope
   - Issue is concrete and bounded (bug, single feature, discrete task)
   - No follow-up explicitly planned

   **Non-closing** (`Refs`, `Part of`, `Contributes to`, `Related to`) when:
   - Issue is an epic/parent, PR is one of multiple
   - PR is partial work (infra-only, first half, behind-flag rollout)
   - PR touches the area but doesn't resolve (adjacent refactor)
   - User signaled more work to come ("first piece", "follow-up coming")

   **No reference** when the PR is genuinely unrelated (rare; user override).

3. **Inject reference** at the top of the PR body, before `## Summary`:

   ```
   Refs MOD-1234

   ## Summary
   ...
   ```

   Multiple refs allowed: `Fixes MOD-1234, MOD-5678`.

4. **No Linear MCP status calls.** Skill does not move issues to "In Review" — Linear's GitHub integration handles transitions on PR open/merge. Skill stays out of state-machine territory.

5. **Branch mismatch:** if branch encodes a different issue ID than the session mapping, flag and ask which to reference. Branch-name issue wins by default (it's what Linear's GitHub integration reads).

## Explicit Linear flow

When the user mentions Linear directly ("the ticket", "update the issue", an ID, etc.) — handle via Linear MCP directly. No prompt-then-act dance. Default issue when the user says "the issue" without specifying = the session's mapped issue.

## File layout

```
~/.claude/skills/linear-flow/
  SKILL.md          # the skill, with ${CLAUDE_SESSION_ID} substitution points
  DESIGN.md         # this doc

~/.claude/linear-sessions/
  <session-uuid>.json   # per-session mapping
  config.json           # work_orgs, team_defaults
```

## Why no SessionStart hook

The earlier version of this design used a `SessionStart` hook to write a cwd-keyed sentinel file containing the current session UUID. The discovery that `${CLAUDE_SESSION_ID}` is substituted directly into SKILL.md content (per official Claude Code docs) made the hook redundant. The skill reads its own UUID natively. One fewer moving part, no hook misconfiguration risk, no cwd encoding logic to maintain.

## Out of scope

- Auto-moving issues through Linear statuses (delegated to Linear's GitHub integration).
- Bidirectional sync (Linear changes → Claude context).
- Cross-session issue lifecycle reporting.
- Hook-driven boundary prompts at `SessionStart`/`SessionEnd`.
- Slash-command interface (`/linear`). The skill is description-triggered; no slash command planned.
