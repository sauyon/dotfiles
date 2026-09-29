#!/usr/bin/env python3
"""sts-classify: turn one STS token-exchange response into a verdict.

    sts-classify.py <http-code> <body-file>   -> "accepted" | "rejected <why>" | "void <why>"

Three verdicts, not two, and the third is the point.

A revocation measurement says "the key stopped working at t+N", and its only
evidence is STS refusing a token. But STS answers 400 for several reasons that
have nothing to do with the key: a stale audience is invalid_request or
invalid_target, an attribute condition the subject fails is unauthorized_client.
Counting any of those as a refusal turns a config typo into a measured
revocation time and writes it into the report as fact. Counting them as
acceptance is just as wrong in the other direction.

So anything that is not an authentication verdict about THIS key is `void`: not
a fact about the key, a failed trial, to be discarded rather than counted.

`rejected` is deliberately narrow -- 401, or 400 with error=invalid_grant, which
is what Google returns when it cannot verify the ID token signature, i.e. when
the kid is not in the key set it currently holds. That is the one response that
means the key set no longer authorises this key.

Exit status is always 0: the verdict is the output. A non-zero status here would
be read by a shell loop as a transport failure and retried, which is exactly the
confusion this file exists to remove.
"""
import json, sys


def classify(code, body_text):
    """(code, body) -> (verdict, reason). Pure; tests/wif-sts-classify.sh covers it."""
    try:
        code = int(code)
    except (TypeError, ValueError):
        return "void", f"uninterpretable http code {code!r}"

    if code == 200:
        return "accepted", "STS issued a token"

    # Parse before branching on 400: the error field is what separates a refusal
    # of the KEY from a refusal of the REQUEST, and an unparsable body cannot
    # tell us which, so it is void rather than assumed either way.
    err = None
    try:
        err = (json.loads(body_text) or {}).get("error")
    except (ValueError, AttributeError):
        err = None

    if code == 401:
        return "rejected", "401 from STS"
    if code == 400:
        if err == "invalid_grant":
            return "rejected", "400 invalid_grant -- the key set does not authorise this key"
        if err:
            return "void", f"400 {err} -- about the request, not the key (audience? subject? condition?)"
        return "void", "400 with no readable error field -- cannot tell a key refusal from a bad request"
    if code == 0:
        return "void", "no response -- transport"
    if 500 <= code <= 599:
        return "void", f"{code} -- server side, not a verdict about the key"
    return "void", f"{code}" + (f" {err}" if err else "") + " -- not an authentication verdict"


def main(argv):
    if len(argv) != 3:
        print("void usage: sts-classify.py <http-code> <body-file>")
        return 0
    code, path = argv[1], argv[2]
    try:
        with open(path) as f:
            body = f.read()
    except OSError:
        # A body we cannot read is not evidence. The HTTP code alone still
        # decides the unambiguous cases (200, 401, 5xx); a 400 without its body
        # stays void, which is what the missing-file case must not turn into a
        # crash in the middle of a timing loop.
        body = ""
    verdict, why = classify(code, body)
    print(f"{verdict} {why}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
