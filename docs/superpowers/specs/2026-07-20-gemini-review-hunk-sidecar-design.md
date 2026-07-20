# gemini-review → hunk agent-context sidecar

Date: 2026-07-20
Status: approved (implementing without further review, per user)

## Goal

Make the `gemini-review` skill deliver Gemini's findings as **inline notes on the
diff in Hunk**, instead of only relaying prose in-harness. The user reviews
Gemini's findings hunk-by-hunk in the Hunk TUI.

## Key decision

Use Hunk's first-class `--agent-context <path>` JSON sidecar. This requires **no
live Hunk session, no daemon, and no change to Hunk or the bundled `hunk-review`
skill**. Findings are authored headlessly into a file; the user opens
`hunk diff --agent-context <file>` when they want.

Rejected alternatives:
- **Live-session push** (`hunk session comment apply --stdin`): requires a Hunk
  session already running; not headless.
- **Modifying Hunk itself**: `hunk-review` SKILL.md is a read-only symlink into
  the nix store (upstream `github:modem-dev/hunk`); changing it needs a fork or
  flake overlay. Unnecessary — the sidecar already gives session-free persistence.

## Sidecar schema (authoritative, hunk 0.17.0 `src/core/agent.ts`)

```jsonc
{
  "version": 1,
  "summary": "optional top-level summary (we put the ship/fix verdict here)",
  "files": [
    {
      "path": "quited/quited/src/plugins.rs",   // required, non-empty; matched against a file's current OR previous path
      "summary": "optional per-file summary",
      "annotations": [
        {
          "summary": "required, non-empty — the note title",
          "rationale": "optional longer explanation",
          "oldRange": [start, end],   // optional; 1-based inclusive integer tuple, ordered start<=end
          "newRange": [start, end],   // optional; same rules; for the new/`+` side
          "markup": "<stml>",         // optional STML body
          "tags": ["security"],       // optional string[]
          "confidence": "low|medium|high",  // optional
          "source": "gemini",         // optional
          "author": "gemini",         // optional
          "id": "...",                // optional
          "createdAt": "ISO-8601"     // optional
        }
      ]
    }
  ]
}
```

Notes derived from the loader:
- Only `files[].path` and each `annotations[].summary` are required.
- Ranges are **optional**: an annotation with no valid range degrades to a
  file-level note rather than failing. Invalid ranges (non-integer, <1, end<start)
  throw, so the transform must emit a range only when it is clean, else omit it.

## Flow

1. **Review (headless).** Run from repo root:
   ```bash
   git diff --no-ext-diff | agy --dangerously-skip-permissions -p "<JSON-findings prompt>"
   ```
   The prompt instructs Gemini to output **strict JSON only**:
   ```jsonc
   { "verdict": "ship | fix-first — one line",
     "findings": [
       { "file": "path/as/in/diff",
         "side": "new" | "old",
         "startLine": 123, "endLine": 130,   // 1-based file line numbers from @@ headers / the +/- side; omit if unsure
         "category": "correctness|security|incomplete|...",
         "confidence": "low|medium|high",
         "summary": "one-line finding",
         "rationale": "why / what to do" } ] }
   ```
   Gemini is told to read line numbers off the `@@` hunk headers and prefer
   **omitting** a range over guessing.

2. **Transform → sidecar.** A committed helper
   `findings-to-agent-context.py` (in the skill dir) reads Gemini's raw stdout,
   tolerantly extracts the JSON object (strips ``` fences / prose preamble),
   and maps:
   - group `findings` by `file` → `files[]`
   - `side:"new"` + valid `startLine`/`endLine` → `newRange`; `side:"old"` → `oldRange`; else no range
   - `confidence` → `confidence`; `category` → `tags`
   - `verdict` → top-level `summary`; `source`/`author` → `"gemini"`
   Writes `.gemini-review.agent.json` at repo root. On parse failure it exits
   non-zero so the skill can fall back.

3. **Render (user-driven).** The skill prints the exact command for the *user*
   to run (it does not launch the TUI itself, per hunk's "the TUI is the user's"
   rule):
   ```bash
   hunk diff --agent-context .gemini-review.agent.json
   ```
   Staged/branch variants: `hunk diff --staged --agent-context …` (pair with
   `git diff --cached`), `hunk diff main...HEAD --agent-context …` (pair with
   `git diff main...HEAD`). The reviewed diff and the rendered diff must be the
   same set so line numbers align.

4. **In-harness.** Still relay the `verdict` and triage each finding
   (agree / disagree-with-reason) as the skill does today.

## Fallback

If `agy` returns prose rather than valid JSON (it sometimes does), the transform
exits non-zero; the skill falls back to today's behavior — relay Gemini's prose
findings in-harness. No capability is lost; the hunk sidecar is an enhancement.

## Defaults

- **Line accuracy:** LLMs miscount; since ranges are optional, an omitted/slightly
  off range degrades to a file-level note. Gemini is told to omit rather than guess.
- **Sidecar location:** repo-root `.gemini-review.agent.json` (copy-pasteable
  path). Add it to the repo's gitignore (or the user's global gitignore).

## Out of scope

- Changes to `hunk` or the `hunk-review` skill.
- Live-session comment injection.
- The separate skillopt prompt-tuning experiment (paused, diffs staged in scratch).
