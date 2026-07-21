#!/usr/bin/env python3
"""Transform Gemini review findings (JSON) into a Hunk --agent-context sidecar.

Reads Gemini's raw stdout on stdin, tolerantly extracts the JSON object
(strips markdown fences / prose preamble), and writes a Hunk agent-context
sidecar to stdout (or the path given as argv[1]).

Exits non-zero if no valid findings JSON can be parsed, so the calling skill
can fall back to relaying prose.

Input shape (what the review prompt asks Gemini for):
    { "verdict": "...",
      "findings": [ { "file", "side":"new"|"old", "startLine", "endLine",
                      "category", "confidence", "summary", "rationale" } ] }

Output shape: Hunk agent-context (see src/core/agent.ts in hunkdiff):
    { "version": 1, "summary": <verdict>,
      "files": [ { "path", "annotations": [ { "summary", "rationale"?,
                   "newRange"|"oldRange"?, "tags"?, "confidence"?,
                   "source", "author" } ] } ] }
"""
import json
import re
import sys


def extract_json_object(text):
    """Return the first top-level JSON object found in text, or None."""
    # Strip a fenced code block if present: ```json ... ``` or ``` ... ```
    fence = re.search(r"```(?:json)?\s*\n(.*?)```", text, re.DOTALL)
    if fence:
        text = fence.group(1)
    text = text.strip()
    # Fast path: the whole thing is JSON.
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        pass
    # Fallback: decode the first object starting at the first '{'.
    start = text.find("{")
    if start == -1:
        return None
    decoder = json.JSONDecoder()
    try:
        obj, _ = decoder.raw_decode(text[start:])
        return obj
    except json.JSONDecodeError:
        return None


def clean_range(finding):
    """Return (start, end) as a valid 1-based ordered tuple, or None."""
    start = finding.get("startLine")
    end = finding.get("endLine", start)
    if not isinstance(start, int):
        return None
    if not isinstance(end, int):
        end = start
    if start < 1 or end < 1:
        return None
    if end < start:
        start, end = end, start
    return [start, end]


def build(findings_doc):
    verdict = findings_doc.get("verdict")
    findings = findings_doc.get("findings")
    if not isinstance(findings, list):
        return None

    files = {}  # path -> file entry (dict), insertion-ordered
    for f in findings:
        if not isinstance(f, dict):
            continue
        path = f.get("file")
        summary = f.get("summary")
        if not isinstance(path, str) or not path:
            continue
        if not isinstance(summary, str) or not summary:
            continue

        ann = {"summary": summary, "source": "gemini", "author": "gemini"}
        rationale = f.get("rationale")
        if isinstance(rationale, str) and rationale:
            ann["rationale"] = rationale
        conf = f.get("confidence")
        if conf in ("low", "medium", "high"):
            ann["confidence"] = conf
        category = f.get("category")
        if isinstance(category, str) and category:
            ann["tags"] = [category]

        rng = clean_range(f)
        if rng is not None:
            side = f.get("side", "new")
            ann["oldRange" if side == "old" else "newRange"] = rng

        files.setdefault(path, {"path": path, "annotations": []})
        files[path]["annotations"].append(ann)

    if not files:
        return None

    doc = {"version": 1, "files": list(files.values())}
    if isinstance(verdict, str) and verdict:
        doc["summary"] = verdict
    return doc


def main():
    raw = sys.stdin.read()
    parsed = extract_json_object(raw)
    if parsed is None:
        print("findings-to-agent-context: no JSON object in input", file=sys.stderr)
        return 1
    sidecar = build(parsed)
    if sidecar is None:
        print("findings-to-agent-context: no usable findings", file=sys.stderr)
        return 1

    out = json.dumps(sidecar, indent=2)
    if len(sys.argv) > 1:
        with open(sys.argv[1], "w") as fh:
            fh.write(out + "\n")
    else:
        print(out)
    return 0


if __name__ == "__main__":
    sys.exit(main())
