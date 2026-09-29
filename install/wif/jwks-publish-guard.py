#!/usr/bin/env python3
"""jwks-publish-guard: refuse a JWKS replacement that would drop a published key.

    jwks-publish-guard.py <proposed.json> <live.json> [--allow-drop KID]...
    jwks-publish-guard.py --selftest

`admin-setup.sh` publishes `jq -s '{keys: .}' "$@"` -- the key set is exactly the
JWK files on its command line, and the upload replaces the object outright. So
omitting a file is a silent de-authorisation: jq succeeds, the upload succeeds,
the bucket serves a valid JWKS, discovery still resolves, and the host whose JWK
was left out is simply no longer in the trust root. Item 3 runs that command once
per enrolled identity, so it is five more opportunities to leave one out.

`jwks-remove-kid.py` guards the path where removal is the *intent*. This guards
the path where removal is a *side effect*, which is the one nobody reads as a
removal.

Why this has to run before the upload rather than being caught afterwards: per
A6.5, an addition is live within ~15 s but a removal has no measured upper bound
and was still accepted 21.7 h after the write. A mistaken drop therefore produces
no prompt symptom at all -- the fleet keeps working on keys the published set no
longer lists, and the first evidence is a host failing to activate a day later,
long after anyone connects it to this command.

Refusals, all before anything is written:
  - a kid in the live set is absent from the proposed set, and was not named with
    --allow-drop
  - --allow-drop names a kid that is not in the live set (the operator's picture
    of the bucket is stale -- most likely someone else republished)
  - the proposed set has no usable key, has a duplicate kid, has an entry with no
    kid, or is not a JWKS at all
  - the live set cannot be read. This one matters most: treating an unreadable
    live set as "nothing is published" would make every drop invisible and wave
    through precisely the publish this exists to stop. A failed fetch is a reason
    to stop, not a reason to assume the bucket is empty.

Exit 0 = safe to publish. Exit 1 = refused, with the reason on stderr.
"""
import argparse, json, sys


def kids_of(doc, what):
    """Ordered kids of a JWKS. Raises ValueError naming what is wrong."""
    if not isinstance(doc, dict):
        raise ValueError(f"{what} is not a JSON object")
    keys = doc.get("keys")
    if not isinstance(keys, list):
        raise ValueError(f"{what} has no \"keys\" list -- not a JWKS")
    out = []
    for i, k in enumerate(keys):
        if not isinstance(k, dict):
            raise ValueError(f"{what} entry {i} is not an object")
        kid = k.get("kid")
        if not kid:
            raise ValueError(
                f"{what} entry {i} has no kid; a key set whose members cannot be "
                f"named cannot be compared against what is published")
        out.append(kid)
    return out


def check_publish(proposed, live, allow_drop):
    """(proposed, live, [kid]) -> [problem strings]. Empty list means safe."""
    problems = []
    prop = kids_of(proposed, "the proposed set")
    livek = kids_of(live, "the live set")

    if not prop:
        problems.append("the proposed set has no keys; publishing it would "
                        "de-authorise every host at once")
    dupes = sorted({k for k in prop if prop.count(k) > 1})
    if dupes:
        problems.append(f"the proposed set lists these kids more than once: {', '.join(dupes)}")

    allow = list(allow_drop or [])
    stale = [k for k in allow if k not in livek]
    if stale:
        problems.append(
            f"--allow-drop names {', '.join(stale)}, which {'is' if len(stale) == 1 else 'are'} "
            f"not in the live set. Your picture of the bucket is out of date -- re-fetch it "
            f"and look before replacing it.")

    dropped = [k for k in livek if k not in prop and k not in allow]
    if dropped:
        problems.append(
            f"this publish would REMOVE {len(dropped)} published "
            f"{'kid' if len(dropped) == 1 else 'kids'}: {', '.join(dropped)}\n"
            f"  The host holding each one loses access, and per A6.5 you will not see it "
            f"happen: removals have no measured upper bound, so the fleet keeps working for "
            f"an unknown period and fails later.\n"
            f"  If the removal is intended, name each kid with --allow-drop, or use "
            f"revoke-kid.sh which exists for that.")
    return problems


def _selftest():
    a = {"keys": [{"kid": "A", "x": "1", "y": "2"}]}
    ab = {"keys": [{"kid": "A", "x": "1", "y": "2"}, {"kid": "B", "x": "3", "y": "4"}]}
    assert check_publish(ab, a, []) == []
    assert check_publish(a, ab, []) != []
    assert check_publish(a, ab, ["B"]) == []
    return 0


def main(argv):
    ap = argparse.ArgumentParser(add_help=True)
    ap.add_argument("proposed", nargs="?")
    ap.add_argument("live", nargs="?")
    ap.add_argument("--allow-drop", action="append", default=[], metavar="KID")
    ap.add_argument("--selftest", action="store_true")
    args = ap.parse_args(argv[1:])

    if args.selftest:
        return _selftest()
    if not args.proposed or not args.live:
        print("usage: jwks-publish-guard.py <proposed.json> <live.json> [--allow-drop KID]...",
              file=sys.stderr)
        return 1

    docs = {}
    for label, path in (("the proposed set", args.proposed), ("the live set", args.live)):
        try:
            with open(path) as f:
                docs[label] = json.load(f)
        except OSError as e:
            # Not "assume empty": see the module docstring.
            print(f"cannot read {label} at {path}: {e.strerror}. Refusing -- an unreadable "
                  f"live set would hide every removal.", file=sys.stderr)
            return 1
        except ValueError as e:
            print(f"{label} at {path} is not valid JSON: {e}", file=sys.stderr)
            return 1

    try:
        problems = check_publish(docs["the proposed set"], docs["the live set"], args.allow_drop)
    except ValueError as e:
        print(f"refusing: {e}", file=sys.stderr)
        return 1

    if problems:
        print("refusing to publish:", file=sys.stderr)
        for p in problems:
            print(f"- {p}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
