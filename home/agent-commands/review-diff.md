---
name: review-diff
description: Use when the user wants a rich, annotated review of a code change, diff, branch, or PR — the diff itself organized, annotated, and shown with surrounding codebase context. Complements explain-diff (which teaches); this one is for reviewing.
---

<!--
Companion to explain-diff. Where explain-diff produces a teaching narrative
(background / intuition / quiz), review-diff produces a reviewer's artifact: the
actual diff, reorganized around the logic of the change, annotated inline, and
surrounded by the codebase context a reviewer needs but can't see in the diff.
Presentation ideas drawn from Devin Review (Cognition) and Greptile.
-->

# Review Diff

Please make me a rich, annotated review of the specified code change — something a
reviewer reads to actually review the PR, not a tutorial. If the change isn't
specified, ask what to review (working tree, staged, a commit range, a branch, or a
PR) before starting.

First figure out the diff, then **explore the surrounding codebase** — this is the
whole point. The diff alone is not enough to review well; you need the context that
GitHub's file-by-file view hides.

## Sections

- **Summary + table of contents**: One short paragraph on what the change does and
  why, then a clickable TOC of the sections below. One long page, section headers,
  no top-level tabs.

- **Annotated changes**: This is the body. Do NOT present the diff file-by-file in
  alphabetical order the way GitHub does — the structure of a diff rarely matches the
  structure of the work. Instead:
  - Group changes that are **logically connected** into sections, and order the
    sections so the reader can review top-to-bottom in the order the change makes
    sense. Give each section a heading and a 1–2 sentence summary of what it does and
    why.
  - Show the **actual diff hunks**, with added/removed lines colored (accessible
    green/red, and don't rely on color alone — keep the `+`/`-`).
  - When code was **moved or renamed**, show it as a move, not as a full delete plus a
    full rewrite. Say "moved from X to Y" and only highlight what actually changed.
  - Attach **inline annotations** to the non-obvious hunks. Be precise and technical:
    explain *why* the change is the way it is, what it replaces, and what a reviewer
    should understand. Don't annotate the obvious. This is not a beginner tutorial.

- **Codebase context**: The differentiating feature — surface what the reviewer needs
  but can't see in the diff, gathered by actually exploring the repo. For the changed
  symbols, concretely include, where relevant:
  - **Call sites** — who calls the changed functions/uses the changed types, and
    whether they're affected (flag callers the diff *didn't* update).
  - **Related tests** — which tests cover this code, and whether they were updated.
  - **History / prior behavior** — why the existing code looked the way it did, and
    what invariant or edge case the current shape was protecting, if any.
  Attach these as callouts near the section they inform, not in one dump at the end.

- **Findings**: Things the reviewer should scrutinize, tagged by severity so they're
  scannable (scheme from Devin Review):
  - 🔴 **red** — probable bug / correctness or security issue.
  - 🟡 **yellow** — warning: risky edge case, missed caller, behavior change, unclear
    invariant.
  - ⚪ **gray** — FYI / nit / commentary.
  Prefer **precision over recall**: state your confidence, and do not pad the list
  with low-confidence guesses — a noisy review is a review people stop trusting. If
  you found nothing worth flagging in a section, say so plainly rather than inventing
  findings. Tie each finding to the specific hunk/line it's about.

## Format

- Output a single self-contained HTML file with inline CSS and JavaScript. One long
  page with section headers and a table of contents; no tabs for the top-level
  structure. Basic responsive styling for phone viewing is nice.
- Put the file in a global place outside the code repo, filename starting with
  today's date in `YYYY-MM-DD-` format so files stay time-sorted and out of version
  control. For example: `/tmp/2026-01-12-review-<slug>.html`
- Write annotations tersely and technically — this is reference material for someone
  who already knows the language and codebase, not a teaching narrative. (Use
  explain-diff instead if the goal is to teach the change to someone unfamiliar.)
- For code and diff blocks, always use `<pre>` tags. If you use a custom styled div
  instead, it **must** have `white-space: pre-wrap` in its CSS, or the browser will
  collapse all newlines into a single line. Before saving the file, scan each code
  block in the HTML source and confirm its CSS includes `white-space: pre` or
  `pre-wrap`.
- Don't use ASCII diagrams. Use simple HTML designs for any diagrams, HTML lists for
  lists, etc.
- Use callouts for codebase context and for severity-tagged findings so they stand
  apart from the diff. Making the context callouts collapsible keeps the page
  scannable on large changes.
- Default output is the local HTML file above. If the user would rather have a
  hosted, shareable page, the same self-contained HTML can be published via the
  Artifact tool instead.
