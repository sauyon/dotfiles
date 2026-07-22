#!/usr/bin/env python3
"""Transform Gemini review findings (JSON) into a Hunk --agent-context sidecar.

Modes:

1. Single reviewer (stdin):
       agy ... | findings-to-agent-context.py [OUT]
   Reads one findings doc on stdin; writes the sidecar to OUT (or stdout).

2. Panel merge (multiple source-tagged reviewers):
       findings-to-agent-context.py OUT correctness=review_c.out security=review_s.out
   Each `label=path` is one reviewer's raw output. Findings are tagged with their
   reviewer (source `gemini:<label>`), merged, and cross-reviewer duplicates
   collapsed. Writes the sidecar directly.

3. Panel merge + verify pass (two-phase, false-positive DEMOTION):
   a. Emit merged candidates (stable ids) for a verifier to score:
        findings-to-agent-context.py --emit-candidates cand.json c=rc.out s=rs.out ...
   b. (run a verify agy pass that scores each id 0-100 -> scores.out)
   c. Finalize: fold scores in, write sidecar:
        findings-to-agent-context.py OUT --candidates cand.json --scores scores.out --min 50

   Verify policy: findings scored below --min are KEPT but flagged `disputed`
   (score:N tag, confidence lowered, note appended) and sorted last — NEVER silently
   deleted. Only genuine noise is dropped: verifier score 0 AND weak (low/none)
   reviewer confidence. A plausible high-severity finding always reaches the human.

Exits non-zero if no usable findings can be parsed, so the caller can fall back to
relaying prose.

Reviewer input shape:
    { "verdict": "...", "findings": [ { "file","side","startLine","endLine",
      "category","confidence","summary","rationale" } ] }
Verifier scores shape (tolerant): {"scores":[{"id":"F1","score":85}]} or a bare list.
Sidecar output: Hunk agent-context (see src/core/agent.ts in hunkdiff).
"""
import json
import re
import sys

_CONF_RANK = {"low": 1, "medium": 2, "high": 3}


def extract_json(text):
    """Return the first top-level JSON value (object or array) in text, or None."""
    fence = re.search(r"```(?:json)?\s*\n(.*?)```", text, re.DOTALL)
    if fence:
        text = fence.group(1)
    text = text.strip()
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        pass
    start = min((i for i in (text.find("{"), text.find("[")) if i != -1), default=-1)
    if start == -1:
        return None
    try:
        obj, _ = json.JSONDecoder().raw_decode(text[start:])
        return obj
    except json.JSONDecodeError:
        return None


def clean_range(start, end):
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
    for f in doc.get("findings", []) if isinstance(doc, dict) else []:
        if not isinstance(f, dict):
            continue
        path, summary = f.get("file"), f.get("summary")
        if not (isinstance(path, str) and path and isinstance(summary, str) and summary):
            continue
        yield {
            "path": path,
            "side": "old" if f.get("side") == "old" else "new",
            "range": clean_range(f.get("startLine"), f.get("endLine", f.get("startLine"))),
            "summary": summary,
            "rationale": f.get("rationale") if isinstance(f.get("rationale"), str) else "",
            "confidence": f.get("confidence") if f.get("confidence") in _CONF_RANK else None,
            "tags": [f["category"]] if isinstance(f.get("category"), str) and f["category"] else [],
            "sources": [label] if label else [],
            "id": f.get("id") if isinstance(f.get("id"), str) else None,
            "score": None,
            "disputed": False,
        }


def _same(a, b):
    if a["path"] != b["path"]:
        return False
    ra, rb = a["range"], b["range"]
    if ra and rb and a["side"] == b["side"] and ra[0] <= rb[1] and rb[0] <= ra[1]:
        return True
    na, nb = _norm(a["summary"]), _norm(b["summary"])
    return bool(na) and (na == nb or na in nb or nb in na)


def _merge_into(dst, src):
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


def load_sources(pairs):
    """pairs: list of (label, text). Returns (records, verdicts, any_parsed)."""
    records, verdicts, any_parsed = [], [], False
    for label, text in pairs:
        doc = extract_json(text)
        if not isinstance(doc, dict):
            print(f"findings-to-agent-context: no JSON object in {label or 'stdin'}", file=sys.stderr)
            continue
        any_parsed = True
        records.extend(normalize_findings(doc, label))
        if isinstance(doc.get("verdict"), str):
            verdicts.append([label, doc["verdict"]])
    return records, verdicts, any_parsed


def assign_ids(records):
    for i, r in enumerate(records, 1):
        if not r["id"]:
            r["id"] = f"F{i}"
    return records


def candidates_json(records, verdicts):
    out = []
    for r in records:
        item = {"id": r["id"], "file": r["path"], "side": r["side"],
                "summary": r["summary"], "rationale": r["rationale"],
                "reviewers": r["sources"], "confidence": r["confidence"]}
        if r["range"]:
            item["startLine"], item["endLine"] = r["range"]
        if r["tags"]:
            item["category"] = r["tags"][0]
        out.append(item)
    return json.dumps({"verdicts": verdicts, "findings": out}, indent=2)


def records_from_candidates(doc):
    recs = list(normalize_findings(doc, None))
    for r, f in zip(recs, doc.get("findings", [])):
        r["sources"] = [s for s in f.get("reviewers", []) if isinstance(s, str)]
    verdicts = [v for v in doc.get("verdicts", []) if isinstance(v, list) and len(v) == 2]
    return recs, verdicts


def parse_scores(text):
    doc = extract_json(text)
    items = doc.get("scores") if isinstance(doc, dict) else doc
    scores = {}
    for it in items if isinstance(items, list) else []:
        if isinstance(it, dict) and isinstance(it.get("id"), str) and isinstance(it.get("score"), (int, float)):
            scores[it["id"]] = int(it["score"])
    if isinstance(doc, dict) and not scores:  # {"F1": 85} form
        for k, v in doc.items():
            if isinstance(v, (int, float)):
                scores[k] = int(v)
    return scores


def apply_scores(records, scores, min_score):
    """Fold verify scores in. Demote (flag disputed) below min; keep everything
    except genuine noise (score 0 AND weak reviewer confidence)."""
    kept = []
    for r in records:
        sc = scores.get(r["id"])
        r["score"] = sc
        r["disputed"] = sc is not None and sc < min_score
        if sc == 0 and r["confidence"] in (None, "low"):
            continue  # only clear noise is dropped
        kept.append(r)
    return kept


def _sort_key(r):
    sc = r["score"] if r["score"] is not None else 100
    return (1 if r["disputed"] else 0, -sc, -_CONF_RANK.get(r["confidence"], 0))


def to_sidecar(records, verdicts):
    files = {}
    for r in sorted(records, key=_sort_key):
        rationale, conf = r["rationale"], r["confidence"]
        tags = list(dict.fromkeys(r["sources"] + r["tags"]))
        if r["score"] is not None:
            tags.append(f"score:{r['score']}")
        if r["disputed"]:
            tags.append("disputed")
            conf = "low"
            note = f"[verify: DISPUTED — score {r['score']}/100; a reviewer flagged this but the verifier doubts it]"
            rationale = (rationale + " " + note).strip() if rationale else note
        elif r["score"] is not None:
            conf = "high" if r["score"] >= 75 else "medium" if r["score"] >= 50 else conf
        ann = {"summary": r["summary"], "author": "gemini"}
        ann["source"] = "gemini:" + "+".join(r["sources"]) if r["sources"] else "gemini"
        if rationale:
            ann["rationale"] = rationale
        if conf:
            ann["confidence"] = conf
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


def main():
    args = sys.argv[1:]
    out_path = emit_candidates = candidates_file = scores_file = None
    min_score = 50
    pairs = []
    i = 0
    while i < len(args):
        a = args[i]
        if a == "--emit-candidates":
            emit_candidates = args[i + 1]; i += 2
        elif a == "--candidates":
            candidates_file = args[i + 1]; i += 2
        elif a == "--scores":
            scores_file = args[i + 1]; i += 2
        elif a == "--min":
            min_score = int(args[i + 1]); i += 2
        elif "=" in a:
            label, path = a.split("=", 1)
            pairs.append((label, open(path).read())); i += 1
        elif out_path is None:
            out_path = a; i += 1
        else:
            print(f"findings-to-agent-context: bad arg {a!r}", file=sys.stderr); return 2

    if candidates_file:
        doc = extract_json(open(candidates_file).read())
        if not isinstance(doc, dict):
            return 1
        records, verdicts = records_from_candidates(doc)
    else:
        if not pairs:
            pairs = [("", sys.stdin.read())]
        records, verdicts, any_parsed = load_sources(pairs)
        if not any_parsed:
            return 1

    records = dedupe(assign_ids(records))

    if scores_file:
        records = apply_scores(records, parse_scores(open(scores_file).read()), min_score)

    if emit_candidates:
        with open(emit_candidates, "w") as fh:
            fh.write(candidates_json(records, verdicts) + "\n")
        return 0

    sidecar = to_sidecar(records, verdicts)
    if sidecar is None:
        print("findings-to-agent-context: no usable findings", file=sys.stderr)
        return 1
    text = json.dumps(sidecar, indent=2)
    if out_path:
        open(out_path, "w").write(text + "\n")
    else:
        print(text)
    return 0


if __name__ == "__main__":
    sys.exit(main())
