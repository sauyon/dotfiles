---
name: agy-review
description: Use when the user asks for an "agy review", a "gemini review", a second-opinion / adversarial review of the current changes from Gemini, or to run the local Antigravity (`agy`) CLI over a diff. Runs a panel of independent Gemini reviewers (correctness + security) over the diff, then renders the merged findings as inline notes on the diff in the Hunk viewer (via an `--agent-context` sidecar). NOT for in-harness review (use /code-review for that) — this deliberately sends the diff to the user's own Google/Gemini account.
---

# agy-review

Run the user's local Antigravity CLI (binary `agy`, which replaced the EOL'd `gemini-cli`) as an **independent panel of Gemini reviewers** over the current change — one **correctness** reviewer and one **security** reviewer — then surface the merged findings as **inline notes in Hunk** so the user reviews them hunk-by-hunk. Two angles beat one tuned prompt: a bland single prompt misses things a focused correctness pass and an adversarial security pass each catch.

## CRITICAL: agy is an autonomous agent, not a diff-review tool

`agy` is a full autonomous SWE agent. If you just pipe a diff into it
(`git diff | agy -p "review..."`) it **fails badly**: large stdin gets truncated,
so agy goes hunting for the real repo — it will clone it, tar-copy it (tens of GB),
run `mise trust` / `dev:setup` / the test suite, and report "tests pass" instead of
reviewing. `--sandbox` does not contain it. The **only** reliable way to get a clean
review is the recipe below: give agy the diff **as a file** and constrain it with an
**`AGENTS.md` rule**. Do not deviate from this. (See `[[agy_headless_diff_review]]`.)

## Pipeline

`$SKILL_DIR` is this skill's directory (`$HOME/.claude/skills/agy-review`). Run the
reviewers in one isolated workspace; write the sidecar to the **real repo root**.

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

### 2. Run the two reviewers (sequential — avoid 429s)

```bash
( cd "$W" && agy --new-project --dangerously-skip-permissions -p "<CORRECTNESS prompt>" ) > "$W/review_correctness.out"
( cd "$W" && agy --new-project --dangerously-skip-permissions -p "<SECURITY prompt>"    ) > "$W/review_security.out"
```

- `--new-project` isolates from persisted project context; `--dangerously-skip-permissions`
  avoids the headless hang (the `AGENTS.md` rule keeps it read-only, not this flag).
- Each ~2–3 min. Reuse the same `$W` for both.

### 3. Merge → build the Hunk sidecar

```bash
python3 "$SKILL_DIR/findings-to-agent-context.py" .gemini-review.agent.json \
    correctness="$W/review_correctness.out" security="$W/review_security.out"
```

- The helper tags each finding with its reviewer (`source: "gemini:correctness"` /
  `"gemini:security"`), merges both into one sidecar at the **real repo root**, and
  collapses cross-reviewer duplicates (same file + overlapping range → one note
  tagged `gemini:correctness+security`).
- If **one** reviewer's output isn't valid JSON, the merge still uses the other.
- **If it exits non-zero** (neither parsed), agy returned prose — fall back: relay
  the prose in `$W/review_*.out` in-harness. Do not fabricate a sidecar.
- `.gemini-review.agent.json` is a working artifact — globally gitignored already.

### 4. Render — tell the user to open Hunk

Do **not** launch the TUI yourself (Hunk's TUI is the user's). Print the command:

```bash
hunk diff --agent-context .gemini-review.agent.json
```

(Match stage 1's target: `hunk diff --staged …` or `hunk diff main...HEAD …`.) Notes
anchor to the lines they cite (the `source` tag shows which reviewer flagged each);
findings without a line render at file level.

## Reviewer prompt templates

Both share the same context header and output block; only the **angle** differs.
Tailor the bracketed parts. The output-format block is mandatory (the merge depends
on it). Point agy at `change.diff` explicitly.

**Shared header + output block** (prepend to each angle):

```
Read the file change.diff in this workspace — it is the COMPLETE unified diff of a
proposed, unmerged change to <project, one line on stack>. <Context, e.g.
"Pre-release, so deleting old code paths is intended.">
[[ANGLE — see below]]

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

**Correctness angle:**

```
Review ONLY for correctness/logic bugs: wrong conditions, data-flow errors,
error-handling gaps, (de)serialization/type mismatches, resource leaks, off-by-one,
broken invariants, behavior-breaking or incomplete changes. Report only
high-confidence real bugs a maintainer would fix. No style, no security policy.
```

**Security angle:**

```
You are an adversarial security reviewer. Assume this change ships at least one real
security weakness — find it. Attack the trust boundary: authentication/authorization,
untrusted input, injection, remote code execution, path traversal, unsafe
deserialization, SSRF, secret handling, TOCTOU [name the specific risky spots].
Ignore style and non-security correctness.
```

## After it runs

- Relay both `verdict`s faithfully — including when they contradict your own "looks good" assessment. Passing tests do not refute a behavior/correctness/security concern a reviewer raises.
- Triage each finding (agree / disagree-with-reason) before acting; don't blindly implement. The sidecar (`.gemini-review.agent.json`) has the full list with `source` tags.
- `agy` occasionally hits transient `429 RESOURCE_EXHAUSTED` (model capacity) and retries with backoff — if it ultimately fails, just rerun that reviewer.
- Clean up the workspace when done: `rm -rf "$W"`.

## Notes

- This sends repo source to the user's Google/Gemini account (external model API) via `agy`. That's the intended purpose and is allowed; do not use it for repos where that egress is not acceptable.
- For a review that stays entirely in-harness, use `/code-review` instead.
- Hunk rejects malformed annotation ranges (e.g. reversed tuples); `findings-to-agent-context.py` normalizes/omits bad ranges so the sidecar always loads.
