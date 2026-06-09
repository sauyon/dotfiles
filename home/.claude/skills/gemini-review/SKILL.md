---
name: gemini-review
description: Use when the user asks for a "gemini review", a second-opinion / adversarial review of the current changes from Gemini, or to run the local `gemini` CLI over a diff. Pipes the working-tree diff into the locally-installed `gemini` CLI for an independent code review. NOT for in-harness review (use /code-review for that) — this deliberately sends the diff to the user's own Gemini account.
---

# gemini-review

Run the user's local `gemini` CLI as an independent reviewer over the current change. Gemini reads the piped diff AND can explore the working tree with its own tools, so it catches things the in-harness review and unit tests miss.

## The command

Run exactly this from the repo root (both flags are required — see Gotchas):

```bash
git diff --no-ext-diff | gemini --skip-trust -p "<review prompt>"
```

- `--no-ext-diff` goes **after** `diff` (it's a `git diff` option, not a top-level git flag). Without it, a configured external differ (difftastic/delta) produces colorized non-patch output — or dies on the broken pipe.
- `gemini --skip-trust` is required in non-interactive/headless use; otherwise Gemini refuses with "not running in a trusted directory." (`GEMINI_CLI_TRUST_WORKSPACE=true` env works too.)
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
- Gemini occasionally hits transient `429 RESOURCE_EXHAUSTED` (model capacity) and retries with backoff — if it ultimately fails, just rerun.

## Notes

- This sends repo source to the user's Gemini account (external model API). That's the intended purpose and is allowed; do not use it for repos where that egress is not acceptable.
- For a review that stays entirely in-harness, use `/code-review` instead.
