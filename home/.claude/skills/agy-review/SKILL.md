---
name: agy-review
description: Use when the user asks for an "agy review", a "gemini review", a second-opinion / adversarial review of the current changes from Gemini, or to run the local Antigravity (`agy`) CLI over a diff. Runs a panel of independent Gemini reviewers (correctness, security, error-handling, type-design) plus a verify pass over the diff, then renders the merged findings as inline notes in the Hunk viewer (via an `--agent-context` sidecar). NOT for in-harness review (use /code-review for that) — this deliberately sends the diff to the user's own Google/Gemini account.
---

# agy-review

Run the user's local Antigravity CLI (binary `agy`, which replaced the EOL'd `gemini-cli`) as an **independent panel of Gemini reviewers** over the current change — **correctness**, **security**, **error-handling**, **type-design** — then a **verify pass** to drop false positives, and surface the merged findings as **inline notes in Hunk**. Multiple focused angles beat one tuned prompt: a bland single prompt misses things each specialized pass catches.

## CRITICAL: agy is an autonomous agent, not a diff-review tool

`agy` is a full autonomous SWE agent. If you just pipe a diff into it
(`git diff | agy -p "review..."`) it **fails badly**: large stdin gets truncated,
so agy goes hunting for the real repo — it will clone it, tar-copy it (tens of GB),
run `mise trust` / `dev:setup` / the test suite, and report "tests pass" instead of
reviewing. `--sandbox` does not contain it. The **only** reliable way to get a clean
review is the recipe below: give agy the diff **as a file** and constrain it with an
**`AGENTS.md` rule**. Do not deviate from this. (See `[[agy_headless_diff_review]]`.)

Because each reviewer sees only `change.diff` in an isolated workspace, angles that
need repo context (git history, prior PRs, CLAUDE.md, existing test suite) are out of
scope here — those belong to `/code-review`. This skill covers **diff-only** angles.

## Pipeline

`$SKILL_DIR` is this skill's directory (`$HOME/.claude/skills/agy-review`). Run all
passes in one isolated workspace; write the sidecar to the **real repo root**.

### 1. Build the isolated review workspace

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
- Your ONLY job is to read the files in this workspace and review the code change.
RULES
cp "$W/.agents/AGENTS.md" "$W/AGENTS.md"
```

- The diff **must** be a file (`change.diff`), never stdin — agy reads it with
  `view_file`, so there's no truncation and no reason for it to go find the repo.
- The `AGENTS.md` rule is agy's real steering mechanism ("Rules" = markdown
  behavioral constraints at a customization root; workspace root is `.agents/`, and
  a root `AGENTS.md` is also read). It overrides agy's baked-in "always verify by
  building" bias. Prompt-only "don't run tools" instructions are NOT enough.

### 2. Run the four reviewers (sequential — avoid 429s)

For each angle, run (reusing `$W`); each is `<shared header>` + `<angle>` + `<output block>`:

```bash
for angle in correctness security errorhandling typedesign; do
  ( cd "$W" && agy --new-project --dangerously-skip-permissions -p "<prompt for $angle>" ) > "$W/review_$angle.out"
done
```

- `--new-project` isolates from persisted project context; `--dangerously-skip-permissions`
  avoids the headless hang (the `AGENTS.md` rule keeps it read-only, not this flag).
- Each ~2–3 min.

### 3. Merge → verify → sidecar

```bash
# a. merge the four reviewers into candidate findings with stable ids
python3 "$SKILL_DIR/findings-to-agent-context.py" --emit-candidates "$W/candidates.json" \
    correctness="$W/review_correctness.out" security="$W/review_security.out" \
    errorhandling="$W/review_errorhandling.out" typedesign="$W/review_typedesign.out"

# b. verify pass: agy scores each candidate 0-100 (reads candidates.json + change.diff)
( cd "$W" && agy --new-project --dangerously-skip-permissions -p "<VERIFY prompt>" ) > "$W/scores.out"

# c. finalize: fold verify scores in (demote low scores, don't delete), write sidecar
python3 "$SKILL_DIR/findings-to-agent-context.py" .gemini-review.agent.json \
    --candidates "$W/candidates.json" --scores "$W/scores.out" --min 50
```

- The helper tags each finding with its reviewer(s) (`source: "gemini:security"`,
  or `"gemini:security+errorhandling"` when reviewers agree) and collapses
  cross-reviewer duplicates.
- **Verify policy (`--scores`): demote, don't delete.** `--min` is a *dispute*
  threshold, not a delete threshold. Findings scored below `--min` are **kept** but
  flagged (`disputed` + `score:NN` tags, confidence lowered, a note appended) and
  sorted last, so a plausible high-severity finding always reaches the human. Only
  genuine noise is dropped: verifier score 0 **and** weak (low/none) reviewer
  confidence. Unscored findings are kept as-is.
- Writes `.gemini-review.agent.json` at the **real repo root** (run from there).
- **Fallback**: if the merge exits non-zero (nothing parsed), agy returned prose —
  relay `$W/review_*.out` in-harness; do not fabricate a sidecar. To skip the verify
  pass, omit steps (b)/(c) and run the helper directly with the four `label=path`
  args (writes the sidecar straight from the merge).
- `.gemini-review.agent.json` is a working artifact — globally gitignored already.

### 4. Render — tell the user to open Hunk

Do **not** launch the TUI yourself (Hunk's TUI is the user's). Print the command:

```bash
hunk diff --agent-context .gemini-review.agent.json
```

(Match stage 1's target: `hunk diff --staged …` or `hunk diff main...HEAD …`.) Notes
anchor to the lines they cite (the `source`/`score` tags show which reviewer flagged
each and its verify score); findings without a line render at file level.

## Reviewer prompt templates

All four share a context header and output block; only the **angle** differs. The
output-format block is mandatory (the merge depends on it). Point agy at `change.diff`.

**Shared header + output block** (prepend the angle between them):

```
Read the file change.diff in this workspace — it is the COMPLETE unified diff of a
proposed, unmerged change to <project, one line on stack>. <Context, e.g.
"Pre-release, so deleting old code paths is intended.">
[[ANGLE]]

Output ONLY a JSON object — no prose, no fences — exactly:
{"verdict":"<one line: ship, or fix-first, and why>",
 "findings":[{"file":"<path exactly as in the diff>","side":"new"|"old",
   "startLine":<int>,"endLine":<int>,
   "category":"correctness"|"security"|"incomplete"|"other",
   "confidence":"low"|"medium"|"high",
   "summary":"<one-line finding>","rationale":"<why / what to do>"}]}
startLine/endLine are 1-based TARGET SOURCE FILE lines from the @@ headers (new-side
for "new", old-side for "old"), NOT offsets within change.diff; OMIT if unsure.
Return findings:[] if nothing.
```

- **correctness:** `Review ONLY for correctness/logic bugs: wrong conditions, data-flow errors, (de)serialization/type mismatches, off-by-one, broken invariants, behavior-breaking or incomplete changes. Report only high-confidence real bugs. No style, no security policy.`
- **security:** `You are an adversarial security reviewer. Assume this change ships at least one real security weakness — find it. Attack the trust boundary: auth, untrusted input, injection, RCE, path traversal, unsafe deserialization, SSRF, secret handling, TOCTOU [name the risky spots]. Ignore style and non-security correctness.`
- **errorhandling:** `Review ONLY for silent failures and error handling: swallowed errors, ignored Results/return values, catch-all or empty fallbacks that hide failures, missing error propagation, defaults that mask outages, unwrap/panic on fallible paths. Report cases where a failure goes unnoticed or produces silently-wrong behavior.`
- **typedesign:** `Review ONLY for type & API design: primitive obsession, illegal states made representable, missing encapsulation/invariants, footgun signatures, Option/None where a real type belongs, breaking API/contract changes. Report design flaws that will cause bugs or misuse.`

**Verify prompt** (step 3b):

```
Read change.diff and candidates.json in this workspace. candidates.json lists
candidate review findings, each with an "id". For EACH finding, judge from the diff
whether it is a REAL issue a maintainer would act on, scoring 0-100 (0 = false
positive / not a real issue, 100 = definitely real and worth fixing). Be skeptical;
do not inflate. Output ONLY {"scores":[{"id":"F1","score":<0-100>}, ...]}.
```

## After it runs

- Relay the reviewers' verdicts faithfully — including when they contradict your own "looks good" assessment. Passing tests do not refute a behavior/correctness/security concern a reviewer raises.
- Triage each finding (agree / disagree-with-reason) before acting; don't blindly implement. The sidecar has the full list with `source` and `score:NN` tags.
- `agy` occasionally hits transient `429 RESOURCE_EXHAUSTED` (model capacity) and retries with backoff — if a reviewer ultimately fails, just rerun that one.
- Clean up the workspace when done: `rm -rf "$W"`.

## Notes

- This sends repo source to the user's Google/Gemini account (external model API) via `agy`. That's the intended purpose and is allowed; do not use it for repos where that egress is not acceptable.
- For a review that stays entirely in-harness, or that needs repo/history context, use `/code-review` instead.
- Hunk rejects malformed annotation ranges (e.g. reversed tuples); `findings-to-agent-context.py` normalizes/omits bad ranges so the sidecar always loads.
