---
name: gemini-review
description: Use when the user asks for a "gemini review", a second-opinion / adversarial review of the current changes from Gemini, or to run the local Antigravity (`agy`) CLI over a diff. Pipes the working-tree diff into the locally-installed `agy` CLI for an independent code review from Gemini. NOT for in-harness review (use /code-review for that) — this deliberately sends the diff to the user's own Google/Gemini account.
---

# gemini-review

Run the user's local Antigravity CLI (binary `agy`, which replaced the EOL'd `gemini-cli`) as an independent Gemini-backed reviewer over the current change. `agy` reads the piped diff AND can explore the working tree with its own tools, so it catches things the in-harness review and unit tests miss.

## The command

Run exactly this from the repo root (both flags are required — see Gotchas):

```bash
git diff --no-ext-diff | agy --dangerously-skip-permissions -p "<review prompt>"
```

- `--no-ext-diff` goes **after** `diff` (it's a `git diff` option, not a top-level git flag). Without it, a configured external differ (difftastic/delta) produces colorized non-patch output — or dies on the broken pipe.
- `agy -p` (alias `--print`/`--prompt`) runs a single prompt non-interactively and prints the response; it reads the piped diff on stdin.
- `--dangerously-skip-permissions` auto-approves `agy`'s tool permission requests, which is required for headless use — otherwise it blocks waiting for approval when it tries to explore the working tree.
- To review only staged changes use `git diff --cached --no-ext-diff`; for a branch vs main use `git diff --no-ext-diff main...HEAD`.

## Review prompt template

Tailor the bracketed parts to the repo and the specific change:

```
Review this uncommitted diff of <project, one line on stack>. <State the project's
top priority, e.g. "Security is the #1 priority — flag any regression."> <Any context,
e.g. "Unreleased, so deleting old code paths is correct, not a bug.">
Focus on: (1) correctness/logic bugs, (2) security regressions or weak fixes
[name the specific risky spots], (3) anything incomplete or behavior-breaking.
Cite file + specific concern, skip style nits, prioritize highest-confidence
findings, and end with a short ship / fix-first verdict.
```

## After it runs

- Relay Gemini's findings faithfully — including when they contradict your own "looks good" assessment. Passing tests do not refute a behavior/correctness concern Gemini raises.
- Triage each finding (agree / disagree-with-reason) before acting; don't blindly implement.
- `agy` occasionally hits transient `429 RESOURCE_EXHAUSTED` (model capacity) and retries with backoff — if it ultimately fails, just rerun.

## Notes

- This sends repo source to the user's Google/Gemini account (external model API) via `agy`. That's the intended purpose and is allowed; do not use it for repos where that egress is not acceptable.
- For a review that stays entirely in-harness, use `/code-review` instead.
