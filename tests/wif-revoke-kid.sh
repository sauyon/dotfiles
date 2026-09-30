#!/usr/bin/env bash
# Static contracts for install/wif/revoke-kid.sh -- the script that removes a kid
# from the published JWKS and times how long STS keeps honouring it.
#
# It cannot be exercised offline: it signs with a real device key, talks to STS,
# and writes the bucket through the admin host. So these are static checks on the
# properties that decide whether the number it prints is a measurement or a
# fabrication.
#
# The one that matters most: a revocation is only observed when STS refuses for
# an AUTHENTICATION reason -- 401, or 400 with error=invalid_grant ("Unable to
# verify the ID Token signature"). STS also answers 400 for a wrong audience
# (invalid_request / invalid_target) and for a subject the provider condition
# rejects (unauthorized_client). Counting those as the revocation landing prints
# "REJECTED between t+0s and t+10s" for what is actually a stale KO_WIF_AUDIENCE
# in someone's shell -- and that number goes into the report as the revocation
# latency of the trust root.
#
# This is not hypothetical. Review round 6 caught exactly that bug in
# tests/wif-tpm.sh check 8, and the commit that fixed it (eef94cb) asserted "this
# is the rule revoke-kid.sh's loop already had". It did not: its loop matched
# `400|401)` with no inspection of the error field, so the more dangerous of the
# two scripts kept the bug while the commit message recorded it as safe. Hence a
# check rather than a comment.
#
#   ./tests/wif-revoke-kid.sh
#
# No network.
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
prog="$repo/install/wif/revoke-kid.sh"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

[ -f "$prog" ] || { echo "FAIL 0 - $prog missing"; echo; echo "0 checks, 1 failed"; exit 1; }
src=$(cat "$prog")

# --- the classification must be the shared, tested one -------------------------
if printf '%s' "$src" | grep -q 'sts-classify.py'; then
  ok "the timing loop classifies through sts-classify.py"
else
  bad "the timing loop classifies through sts-classify.py"$'\n'\
"      A second inline copy of this logic is how the two scripts drifted apart:"$'\n'\
"      wif-tpm.sh was fixed to require invalid_grant, revoke-kid.sh was not."
fi

# --- and must not carry the bare 400 branch it replaced ------------------------
if printf '%s' "$src" | grep -qE '^\s*400\|401\)'; then
  bad "no bare '400|401)' branch survives"$'\n'\
"      That treats ANY 400 as the revocation landing. A stale KO_WIF_AUDIENCE"$'\n'\
"      returns 400 invalid_request and would be reported as a measured"$'\n'\
"      revocation time."
else
  ok "no bare '400|401)' branch survives"
fi

# --- a non-verdict must not be counted as one ----------------------------------
# `void` is sts-classify's third verdict: not a fact about the key. In a timing
# loop it must retry, never terminate -- terminating on it prints a bracket that
# describes a network blip.
if printf '%s' "$src" | grep -q 'void'; then
  ok "the loop handles the 'void' verdict explicitly"
else
  bad "the loop handles the 'void' verdict explicitly"$'\n'\
"      Without it, a transport error or a 5xx falls into whichever branch is"$'\n'\
"      last and can end the measurement early."
fi

# --- the preflight must still prove the key works before anything is removed ---
# The experiment runs once: after the kid is gone it cannot be put back to try
# again, so a baseline that was never accepted makes the whole run worthless.
if printf '%s' "$src" | grep -q 'baseline'; then
  ok "a baseline exchange is required before the removal"
else
  bad "a baseline exchange is required before the removal"
fi

# --- the read and the write must be the same object ----------------------------
# Reading one JWKS and overwriting a different one revokes every kid the first
# lacks, and every downstream check still passes because the reduced document is
# valid on its own.
if printf '%s' "$src" | grep -q 'bucket_derived'; then
  ok "the bucket object is derived from the issuer, and a disagreement is refused"
else
  bad "the bucket object is derived from the issuer"
fi

echo
if [ "$fails" = 0 ]; then echo "$n checks, all good"; else echo "$n checks, $fails failed"; fi
[ "$fails" = 0 ]
