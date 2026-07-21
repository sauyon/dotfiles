#!/usr/bin/env python3
"""Transform Gemini review findings (JSON) into a Hunk --agent-context sidecar.

Two modes:

1. Single reviewer (stdin):
       agy ... | findings-to-agent-context.py [OUT]
   Reads one findings doc on stdin; writes the sidecar to OUT (or stdout).

2. Panel merge (multiple source-tagged reviewers):
       findings-to-agent-context.py OUT correctness=review_c.out security=review_s.out
   Each `label=path` is one reviewer's raw output. Findings are tagged with their
   reviewer (source `gemini:<label>`, plus <label> in tags), merged into one
   sidecar, and near-duplicates across reviewers are collapsed.

Exits non-zero if no usable findings can be parsed from ANY input, so the calling
skill can fall back to relaying prose.

Input shape (what the review prompt asks Gemini for):
    { "verdict": "...",
      "findings": [ { "file", "side":"new"|"old", "startLine", "endLine",
                      "category", "confidence", "summary", "rationale" } ] }

Output shape: Hunk agent-context (see src/core/agent.ts in hunkdiff):
    { "version": 1, "summary": <verdict(s)>,
      "files": [ { "path", "annotations": [ { "summary", "rationale"?,
                   "newRange"|"oldRange"?, "tags"?, "confidence"?,
                   "source", "author" } ] } ] }
"""
import json
import re
import sys

_CONF_RANK = {"low": 1, "medium": 2, "high": 3}


def extract_json_object(text):
    """Return the first top-level JSON object found in text, or None."""
    fence = re.search(r"```(?:json)?\s*\n(.*?)```", text, re.DOTALL)
    if fence:
        text = fence.group(1)
    text = text.strip()
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        pass
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
    """Return [start, end] as a valid 1-based ordered tuple, or None."""
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


def _norm(s):
    return re.sub(r"\W+", " ", s.lower()).strip()


def normalize_findings(doc, label):
    """Yield internal finding records from one reviewer doc."""
    findings = doc.get("findings")
    if not isinstance(findings, list):
        return
    for f in findings:
        if not isinstance(f, dict):
            continue
        path = f.get("file")
        summary = f.get("summary")
        if not isinstance(path, str) or not path:
            continue
        if not isinstance(summary, str) or not summary:
            continue
        side = "old" if f.get("side") == "old" else "new"
        rec = {
            "path": path,
            "side": side,
            "range": clean_range(f),
            "summary": summary,
            "rationale": f.get("rationale") if isinstance(f.get("rationale"), str) else "",
            "confidence": f.get("confidence") if f.get("confidence") in _CONF_RANK else None,
            "tags": [f["category"]] if isinstance(f.get("category"), str) and f["category"] else [],
            "sources": [label] if label else [],
        }
        yield rec


def _same(a, b):
    """Heuristic: do two records describe the same issue?"""
    if a["path"] != b["path"]:
        return False
    ra, rb = a["range"], b["range"]
    if ra and rb and a["side"] == b["side"]:
        if ra[0] <= rb[1] and rb[0] <= ra[1]:  # overlapping ranges
            return True
    # No usable range on one side: fall back to summary similarity.
    na, nb = _norm(a["summary"]), _norm(b["summary"])
    if na and (na == nb or na in nb or nb in na):
        return True
    return False


def _merge_into(dst, src):
    """Fold src record into dst (dst wins on primary text; union metadata)."""
    dst["sources"] = list(dict.fromkeys(dst["sources"] + src["sources"]))
    dst["tags"] = list(dict.fromkeys(dst["tags"] + src["tags"]))
    if _CONF_RANK.get(src["confidence"], 0) > _CONF_RANK.get(dst["confidence"], 0):
        dst["confidence"] = src["confidence"]
    if len(src["rationale"]) > len(dst["rationale"]):
        dst["rationale"] = src["rationale"]
    if not dst["range"] and src["range"]:
        dst["range"], dst["side"] = src["range"], src["side"]


def dedupe(records):
    kept = []
    for r in records:
        for k in kept:
            if _same(k, r):
                _merge_into(k, r)
                break
        else:
            kept.append(r)
    return kept


def to_sidecar(records, verdicts):
    files = {}  # path -> entry, insertion-ordered
    for r in records:
        ann = {"summary": r["summary"], "author": "gemini"}
        src = "gemini"
        if r["sources"]:
            src = "gemini:" + "+".join(r["sources"])
        ann["source"] = src
        if r["rationale"]:
            ann["rationale"] = r["rationale"]
        if r["confidence"]:
            ann["confidence"] = r["confidence"]
        tags = list(dict.fromkeys(r["sources"] + r["tags"]))
        if tags:
            ann["tags"] = tags
        if r["range"]:
            ann["oldRange" if r["side"] == "old" else "newRange"] = r["range"]
        files.setdefault(r["path"], {"path": r["path"], "annotations": []})
        files[r["path"]]["annotations"].append(ann)

    if not files:
        return None
    doc = {"version": 1, "files": list(files.values())}
    summary = "; ".join(f"{lbl}: {v}" if lbl else v for lbl, v in verdicts if v)
    if summary:
        doc["summary"] = summary
    return doc


def load_source(label, text):
    doc = extract_json_object(text)
    if doc is None:
        return None, (label, None)
    verdict = doc.get("verdict") if isinstance(doc.get("verdict"), str) else None
    return list(normalize_findings(doc, label)), (label, verdict)


def main():
    args = sys.argv[1:]
    out_path = None
    sources = []  # (label, text)
    if args and "=" not in args[0]:
        out_path = args[0]
        args = args[1:]
    if args:  # panel mode: label=path ...
        for a in args:
            if "=" not in a:
                print(f"findings-to-agent-context: bad arg {a!r}", file=sys.stderr)
                return 2
            label, path = a.split("=", 1)
            with open(path) as fh:
                sources.append((label, fh.read()))
    else:  # single mode: read stdin, no label
        sources.append(("", sys.stdin.read()))

    records, verdicts, any_parsed = [], [], False
    for label, text in sources:
        recs, verd = load_source(label, text)
        if recs is None:
            print(f"findings-to-agent-context: no JSON object in {label or 'stdin'}", file=sys.stderr)
            continue
        any_parsed = True
        records.extend(recs)
        verdicts.append(verd)

    if not any_parsed:
        return 1
    sidecar = to_sidecar(dedupe(records), verdicts)
    if sidecar is None:
        print("findings-to-agent-context: no usable findings", file=sys.stderr)
        return 1

    text = json.dumps(sidecar, indent=2)
    if out_path:
        with open(out_path, "w") as fh:
            fh.write(text + "\n")
    else:
        print(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
