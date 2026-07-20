---
name: gemini-review
description: Use when the user asks for a "gemini review", a second-opinion / adversarial review of the current changes from Gemini, or to run the local Antigravity (`agy`) CLI over a diff. Runs `agy` over the working-tree diff for an independent Gemini review, then renders the findings as inline notes on the diff in the Hunk viewer (via an `--agent-context` sidecar). NOT for in-harness review (use /code-review for that) — this deliberately sends the diff to the user's own Google/Gemini account.
---

# gemini-review

Run the user's local Antigravity CLI (binary `agy`, which replaced the EOL'd `gemini-cli`) as an independent Gemini-backed reviewer over the current change, then surface the findings as **inline notes in Hunk** so the user reviews them hunk-by-hunk. `agy` reads the piped diff AND can explore the working tree with its own tools, so it catches things the in-harness review and unit tests miss.

## Pipeline

Three stages, run from the repo root. `$SKILL_DIR` is this skill's directory (`$HOME/.claude/skills/gemini-review`).

### 1. Review — get findings as JSON

```bash
git diff --no-ext-diff | agy --dangerously-skip-permissions -p "<review prompt, see below>" > "${TMPDIR:-/tmp}/gemini-review.out"
```

- `--no-ext-diff` goes **after** `diff` (it's a `git diff` option, not a top-level git flag). Without it, a configured external differ (difftastic/delta) produces colorized non-patch output — or dies on the broken pipe.
- `agy -p` (alias `--print`/`--prompt`) runs a single prompt non-interactively and prints the response; it reads the piped diff on stdin.
- `--dangerously-skip-permissions` auto-approves `agy`'s tool permission requests, required for headless use — otherwise it blocks waiting for approval when it explores the working tree.
- Staged only: `git diff --cached --no-ext-diff`. Branch vs main: `git diff --no-ext-diff main...HEAD`. **Keep stage 3's `hunk diff` target matching** (`--staged`, or `main...HEAD`) so line numbers line up.

### 2. Transform — build the Hunk sidecar

```bash
python3 "$SKILL_DIR/findings-to-agent-context.py" .gemini-review.agent.json < "${TMPDIR:-/tmp}/gemini-review.out"
```

- The helper tolerantly extracts the JSON object from `agy`'s stdout (strips markdown fences / prose preamble), maps each finding to a Hunk annotation, and writes `.gemini-review.agent.json` at the repo root.
- **If it exits non-zero**, `agy` returned prose instead of JSON — fall back: read `${TMPDIR:-/tmp}/gemini-review.out` and relay the prose findings in-harness (the old behavior). Do not fabricate a sidecar.
- `.gemini-review.agent.json` is a working artifact — add it to the repo's `.gitignore` (or the user's global ignore) if it isn't already.

### 3. Render — tell the user to open Hunk

Do **not** launch the TUI yourself (Hunk's TUI is the user's). Print the exact command for them to run:

```bash
hunk diff --agent-context .gemini-review.agent.json
```

(Use the matching target from stage 1: `hunk diff --staged --agent-context …` or `hunk diff main...HEAD --agent-context …`.) Gemini's notes appear anchored to the lines they cite; findings without a line render at file level.

## Review prompt template

Tailor the bracketed parts to the repo and the specific change. The output-format block is mandatory and must be kept verbatim — the transform depends on it.

```
Review this uncommitted diff of <project, one line on stack>. <State the project's
top priority, e.g. "Security is the #1 priority — flag any regression."> <Any context,
e.g. "Unreleased, so deleting old code paths is correct, not a bug.">
Focus on: (1) correctness/logic bugs, (2) security regressions or weak fixes
[name the specific risky spots], (3) anything incomplete or behavior-breaking.
Skip style nits; prioritize highest-confidence findings.

Output ONLY a JSON object — no prose, no markdown fences — in exactly this shape:
{"verdict":"<one line: ship, or fix-first, and why>",
 "findings":[{"file":"<path exactly as it appears in the diff>",
   "side":"new"|"old",
   "startLine":<int>,"endLine":<int>,
   "category":"correctness"|"security"|"incomplete"|"other",
   "confidence":"low"|"medium"|"high",
   "summary":"<one-line finding>","rationale":"<why / what to do>"}]}
Line numbers are 1-based file lines read from the @@ hunk headers: use new-side (+)
numbers with side "new", old-side (-) numbers with side "old". If unsure of the exact
line, OMIT startLine/endLine (the note becomes file-level) rather than guessing.
Return {"verdict":"<...>","findings":[]} if you find nothing.
```

## After it runs

- Relay Gemini's `verdict` faithfully — including when it contradicts your own "looks good" assessment. Passing tests do not refute a behavior/correctness concern Gemini raises.
- Triage each finding (agree / disagree-with-reason) before acting; don't blindly implement. The sidecar (`.gemini-review.agent.json`) has the full list.
- `agy` occasionally hits transient `429 RESOURCE_EXHAUSTED` (model capacity) and retries with backoff — if it ultimately fails, just rerun.

## Notes

- This sends repo source to the user's Google/Gemini account (external model API) via `agy`. That's the intended purpose and is allowed; do not use it for repos where that egress is not acceptable.
- For a review that stays entirely in-harness, use `/code-review` instead.
- Hunk rejects malformed annotation ranges (e.g. reversed tuples); `findings-to-agent-context.py` normalizes/omits bad ranges so the sidecar always loads.
