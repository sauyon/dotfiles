---
name: agy-review
description: Use when the user asks for an "agy review", a "gemini review", a second-opinion / adversarial review of the current changes from Gemini, or to run the local Antigravity (`agy`) CLI over a diff. Runs `agy` as an independent Gemini reviewer over the diff, then renders the findings as inline notes on the diff in the Hunk viewer (via an `--agent-context` sidecar). NOT for in-harness review (use /code-review for that) — this deliberately sends the diff to the user's own Google/Gemini account.
---

# agy-review

Run the user's local Antigravity CLI (binary `agy`, which replaced the EOL'd `gemini-cli`) as an independent Gemini-backed reviewer over the current change, then surface the findings as **inline notes in Hunk** so the user reviews them hunk-by-hunk.

## CRITICAL: agy is an autonomous agent, not a diff-review tool

`agy` is a full autonomous SWE agent. If you just pipe a diff into it
(`git diff | agy -p "review..."`) it **fails badly**: large stdin gets truncated,
so agy goes hunting for the real repo — it will clone it, tar-copy it (tens of GB),
run `mise trust` / `dev:setup` / the test suite, and report "tests pass" instead of
reviewing. `--sandbox` does not contain it. The **only** reliable way to get a clean
review is the recipe below: give agy the diff **as a file** and constrain it with an
**`AGENTS.md` rule**. Do not deviate from this. (See `[[agy_headless_diff_review]]`.)

## Pipeline

`$SKILL_DIR` is this skill's directory (`$HOME/.claude/skills/agy-review`). Run
the review in an isolated workspace, but write the sidecar to the **real repo root**.

### 1. Build an isolated review workspace

```bash
W=$(mktemp -d)
git diff --no-ext-diff > "$W/change.diff"      # staged: --cached ; branch: main...HEAD
mkdir -p "$W/.agents"
cat > "$W/.agents/AGENTS.md" <<'RULES'
# Rule: read-only diff review mode
You are acting strictly as a READ-ONLY code reviewer.
- Do NOT run any command, build, test, lint, mise, cargo, npm, pnpm, or git.
- Do NOT clone, copy, rsync, or access any file or repository outside this workspace.
- Do NOT verify by execution. Do NOT report build/test/integration status.
- Your ONLY job is to read `change.diff` in this workspace and review the code change.
RULES
cp "$W/.agents/AGENTS.md" "$W/AGENTS.md"
```

- The diff **must** be a file (`change.diff`), never stdin — agy reads it with
  `view_file`, so there's no truncation and no reason for it to go find the repo.
- The `AGENTS.md` rule is agy's real steering mechanism ("Rules" = markdown
  behavioral constraints at a customization root; workspace root is `.agents/`, and
  a root `AGENTS.md` is also read). It overrides agy's baked-in "always verify by
  building" bias. Prompt-only "don't run tools" instructions are NOT enough.

### 2. Review — get findings as JSON

```bash
( cd "$W" && agy --new-project --dangerously-skip-permissions -p "<review prompt, below>" ) > "$W/review.out"
```

- `--new-project` isolates from any persisted project context (else agy latches
  onto the last real project, e.g. quite-app).
- `--dangerously-skip-permissions` avoids the headless hang on approval prompts; the
  `AGENTS.md` rule (not this flag) is what keeps it read-only.

### 3. Transform — build the Hunk sidecar

```bash
python3 "$SKILL_DIR/findings-to-agent-context.py" .gemini-review.agent.json < "$W/review.out"
```

- Writes `.gemini-review.agent.json` at the **real repo root** (run this from there).
- **If it exits non-zero**, agy returned prose instead of JSON — fall back: relay the
  prose in `$W/review.out` in-harness. Do not fabricate a sidecar.
- `.gemini-review.agent.json` is a working artifact — globally gitignored already.

### 4. Render — tell the user to open Hunk

Do **not** launch the TUI yourself (Hunk's TUI is the user's). Print the command:

```bash
hunk diff --agent-context .gemini-review.agent.json
```

(Match stage 1's target: `hunk diff --staged …` or `hunk diff main...HEAD …`.)
Notes anchor to the lines they cite; findings without a line render at file level.

## Review prompt template

Tailor the bracketed parts. The output-format block is mandatory (the transform
depends on it). Point agy at `change.diff` explicitly.

```
Read the file change.diff in this workspace — it is the COMPLETE unified diff of a
proposed, unmerged change to <project, one line on stack>. <Top priority, e.g.
"Security is the #1 priority.">. <Context, e.g. "Pre-release, so deleting old code
paths is intended.">
Review it for (1) correctness/logic bugs, (2) security/trust-boundary weaknesses
[name the specific risky spots], (3) incomplete or behavior-breaking changes.
Skip style nits; prioritize highest-confidence findings.

Output ONLY a JSON object — no prose, no markdown fences — in exactly this shape:
{"verdict":"<one line: ship, or fix-first, and why>",
 "findings":[{"file":"<path exactly as in the diff>",
   "side":"new"|"old",
   "startLine":<int>,"endLine":<int>,
   "category":"correctness"|"security"|"incomplete"|"other",
   "confidence":"low"|"medium"|"high",
   "summary":"<one-line finding>","rationale":"<why / what to do>"}]}
startLine/endLine are the 1-based line numbers OF THE TARGET SOURCE FILE (read them
from the @@ hunk headers: new-side/`+` numbers for side "new", old-side/`-` numbers
for side "old"), NOT offsets within change.diff. If unsure, OMIT startLine/endLine
(the note becomes file-level) rather than guessing. Return findings:[] if nothing.
```

## After it runs

- Relay Gemini's `verdict` faithfully — including when it contradicts your own "looks good" assessment. Passing tests do not refute a behavior/correctness concern Gemini raises.
- Triage each finding (agree / disagree-with-reason) before acting; don't blindly implement. The sidecar (`.gemini-review.agent.json`) has the full list.
- `agy` occasionally hits transient `429 RESOURCE_EXHAUSTED` (model capacity) and retries with backoff — if it ultimately fails, just rerun.
- Clean up the workspace when done: `rm -rf "$W"`.

## Notes

- This sends repo source to the user's Google/Gemini account (external model API) via `agy`. That's the intended purpose and is allowed; do not use it for repos where that egress is not acceptable.
- For a review that stays entirely in-harness, use `/code-review` instead.
- Hunk rejects malformed annotation ranges (e.g. reversed tuples); `findings-to-agent-context.py` normalizes/omits bad ranges so the sidecar always loads.
