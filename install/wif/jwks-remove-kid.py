#!/usr/bin/env python3
"""Remove one kid from a device JWKS, refusing every way that would lock hosts out.

Split out of revoke-kid.sh so it can be tested without touching the live trust
root (tests/wif-jwks.sh). The refusals are the whole point of the file: this is
the one operation in the WIF kit that can make every enrolled host fail to boot
its secrets at once, and it is a single argument away from doing so.

  jwks-remove-kid.py <kid> <in.json> <out.json>
      exit 0: out.json written, "before:"/"after:" kid lists on stdout
      exit 1: refused or malformed, reason on stderr, out.json NOT written
"""
import json, sys


def reduce_jwks(doc, kid):
    """Return the JWKS with <kid> removed, or raise ValueError with the reason."""
    if not isinstance(doc, dict):
        raise ValueError("this is not a JWKS document (expected a JSON object)")
    keys = doc.get("keys")
    if not isinstance(keys, list):
        raise ValueError('this is not a JWKS document (no "keys" array)')
    # A kid-less entry is not something to crash on: it is someone else's key
    # and it must survive untouched, but it also must not be silently counted as
    # the one being removed.
    kids = [k.get("kid") if isinstance(k, dict) else None for k in keys]
    if kid not in kids:
        raise ValueError(f"kid {kid} is not in the published JWKS; it holds {kids}")
    remaining = [k for k, this in zip(keys, kids) if this != kid]
    # The guarantee is about the RESULT, not the input. Checking len(keys) <= 1
    # beforehand passes a JWKS that lists the same kid twice -- which admin-setup.sh
    # can produce, since it is `jq -s '{keys: .}'` over whatever files it is handed
    # and deduplicates nothing -- and then publishes {"keys":[]}, which 401s every
    # enrolled host at its next activation and at boot.
    if not remaining:
        raise ValueError(
            f"refusing: removing {kid} would publish an empty JWKS "
            f"({kids.count(kid)} of the {len(kids)} published keys carry that kid), "
            "which locks out every enrolled host at once")
    out = dict(doc)
    out["keys"] = remaining
    return out


if __name__ == "__main__":
    if len(sys.argv) != 4:
        raise SystemExit(__doc__.strip().splitlines()[-4].strip())
    kid, src, dst = sys.argv[1:4]
    try:
        with open(src) as f:
            doc = json.load(f)
    except (OSError, ValueError) as e:
        raise SystemExit(f"could not read a JWKS from {src}: {e}")
    try:
        out = reduce_jwks(doc, kid)
    except ValueError as e:
        raise SystemExit(str(e))
    print("before:", [k.get("kid") if isinstance(k, dict) else None for k in doc["keys"]])
    print("after: ", [k.get("kid") for k in out["keys"]])
    with open(dst, "w") as f:
        json.dump(out, f, separators=(",", ":"))
