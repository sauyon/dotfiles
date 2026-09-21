---
name: explain-diff
description: Use when the user asks for a rich explanation of a code change, diff, branch, or PR.
---

<!--
Source: https://gist.github.com/geoffreylitt/a29df1b5f9865506e8952488eac3d524
Author: Geoffrey Litt (https://github.com/geoffreylitt), retrieved 2026-07-04.
The gist declares no license; redistributed here for personal use with
attribution. Lightly modified: renamed from explain-diff-html, frontmatter
description trimmed, and the delivery step retargeted at Claude Artifacts with
the original standalone HTML file kept as the fallback for Cursor/opencode.
-->

# Explain Diff

Please make me a rich, interactive explanation of the specified code change.

It should have these sections:

- Background: Explain the existing system relevant to this change. (You should broadly explore surrounding code for this.) We don't know how much the reader already knows, so include a deep background for beginners (note that it can be skipped if the reader is already familiar), and then a more narrow background directly relevant to the change.
- Intuition: Explain the core intuition for the code change. The focus here is to explain the essence, not the full details. Use concrete examples with toy data. Use figures and diagrams liberally.
- Code: Do a high-level walkthrough of the changes to the code. Group/order the changes in an understandable way.
- Quiz: Come up with five questions that test the reader's knowledge of this PR. This should be medium difficulty, difficult enough that you actually need to understand the substance of the PR to answer them, but not gotchas. The goal is to help the reader make sure that they've actually understood. These should be presented as interactive multiple-choice questions, and when the user clicks, it tells them whether they were correct and gives feedback.

Delivery — publish it as a Claude Artifact by default:

1. Load the `artifact-design` skill before writing any markup. It calibrates how much design work the page warrants.
2. Write the page to a file — your scratchpad directory if you have one, otherwise `/tmp/YYYY-MM-DD-explanation-<slug>.html` with today's date. The publisher wraps the file in `<!doctype html><head>…</head><body>`, so write the page content directly: no `<!DOCTYPE>`, `<html>`, `<head>`, `<body>`, or `<meta charset>` tags of your own. Give it a `<title>`.
3. Publish with the `Artifact` tool, passing an emoji `favicon` and a one-sentence `description`. Hand me the returned URL as the deliverable.
4. To revise, edit the same file and publish again — the same file path redeploys to the same URL instead of minting a new one.

If there is no `Artifact` tool (Cursor, opencode) or publishing fails, fall back to a single self-contained HTML file: a complete document this time, with `<meta charset="utf-8">` first in the `<head>` so em-dashes and arrows render even when served without a charset header. Put it outside the code repo, filename starting with today's date in `YYYY-MM-DD-` format so the files stay time-sorted and out of version control — for example `/tmp/2026-01-12-explanation-<slug>.html` — and return the absolute path.

Format:

- Make the whole thing one long page with section headers and a table of contents. Don't use tabs for the top-level structure.
- Inline all CSS and JavaScript, and embed any image as a `data:` URI. An artifact renders under a strict CSP that blocks every external request — CDNs, fonts, remote images, `fetch` — so the page has to be genuinely self-contained, not merely mostly so.
- Style for both light and dark, since an artifact renders in the viewer's theme. Use `@media (prefers-color-scheme: dark)` as the default signal, plus `:root[data-theme="dark"]` / `:root[data-theme="light"]` overrides, which the viewer's theme toggle sets and which must win in both directions.
- Responsive styling so it reads on a phone. Wide content — tables, diagrams, code blocks — scrolls inside its own `overflow-x: auto` container; the page body itself must never scroll sideways.
- Please write with the clarity and flow of Martin Kleppmann, making it engaging and written in classic style. Transitions between sections should be smooth.
- Some tips on diagrams. Ideally, you should pick a small number of diagram families that can be reused throughout the explanation to explain various cases. Some useful kinds of diagrams:
  - A very simplified version of the UI that the user sees in the app, to explain UI changes.
  - A system diagram showing data flow or communication between components. Make sure to include example data here!
- Don't use ASCII diagrams. Always use simple HTML designs for your diagrams, HTML lists for lists of things, etc.
  - For code blocks, always use `<pre>` tags. If you use a custom styled div instead, it **must** have
    `white-space: pre-wrap` in its CSS, or the browser will collapse all newlines into a single line.
    Before saving the file, scan each code block in the HTML source and confirm its CSS includes
    `white-space: pre` or `pre-wrap`.
- Use callouts for key concepts or definitions, important edge cases, etc.
