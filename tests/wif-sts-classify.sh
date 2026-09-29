#!/usr/bin/env bash
# Cases for install/wif/sts-classify.py -- turning an STS response into one of
# three verdicts: accepted, rejected, void.
#
# This exists because the distinction is the whole experiment. A revocation
# measurement reports "the key stopped working at t+N", and the only evidence
# for that is an STS refusal. But STS answers 400 for several unrelated reasons:
# a wrong audience is invalid_request/invalid_target, and so is a subject the
# provider condition dislikes. Round 6 of task-2's review caught exactly this --
# a check that treated ANY 400 as revoked would have turned permanently green,
# declaring the migration finished, while the old key still minted tokens. The
# same trap swallows the opposite result here: a config typo would look like a
# successful revocation and get written into the report as a measured number.
#
# So there are three verdicts, not two. "void" is not a failure of the key; it
# is a failure of the trial, and a void trial must be discarded rather than
# counted in either direction.
#
#   ./tests/wif-sts-classify.sh
#
# No network. Every case is a crafted (code, body) pair.
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
prog="$repo/install/wif/sts-classify.py"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

# case <name> <code> <body> <expected verdict>
case_is() {
  local name="$1" code="$2" body="$3" want="$4" got
  printf '%s' "$body" > "$work/body.json"
  got=$(python3 "$prog" "$code" "$work/body.json" 2>&1)
  # The verdict is the first word; anything after it is the reason, which is for
  # a human and is deliberately not asserted on.
  got="${got%% *}"
  if [ "$got" = "$want" ]; then
    ok "$name -> $want"
  else
    bad "$name -> $want"$'\n'"      got: $got"
  fi
}

# --- accepted: the only code that means the key still works --------------------
case_is "200 with a token"        200 '{"access_token":"ya29.x","expires_in":3600}' accepted
case_is "200 with an empty body"  200 ''                                            accepted

# --- rejected: an AUTHENTICATION refusal, and only that ------------------------
# 401 is unambiguous. 400 counts only when the error is invalid_grant, which is
# what Google returns when it cannot verify the ID token signature -- i.e. when
# the kid is not in the key set it holds.
case_is "401" 401 '{"error":"unauthorized_client"}' rejected
case_is "400 invalid_grant (unknown kid)" 400 \
  '{"error":"invalid_grant","error_description":"Unable to verify the ID Token signature"}' rejected

# --- void: everything else. NOT evidence about the key -------------------------
# These are the ones that would be silently miscounted as a revocation.
case_is "400 invalid_request (bad audience)" 400 \
  '{"error":"invalid_request","error_description":"Invalid value for audience"}' void
case_is "400 invalid_target (wrong pool)" 400 \
  '{"error":"invalid_target","error_description":"no matching provider"}' void
case_is "400 unauthorized_client (condition)" 400 \
  '{"error":"unauthorized_client","error_description":"attribute condition"}' void
case_is "400 with no parsable body" 400 'not json at all'  void
case_is "400 with an empty body"    400 ''                 void
case_is "000 transport failure"     000 ''                 void
case_is "500"                       500 ''                 void
case_is "503"                       503 '{"error":"unavailable"}' void
case_is "403"                       403 '{"error":"forbidden"}'   void

# --- the reason is carried, because a void trial has to be diagnosable ---------
printf '%s' '{"error":"invalid_request","error_description":"Invalid value for audience"}' > "$work/body.json"
out=$(python3 "$prog" 400 "$work/body.json" 2>&1)
if printf '%s' "$out" | grep -q 'invalid_request'; then
  ok "a void verdict names the STS error that caused it"
else
  bad "a void verdict names the STS error that caused it"$'\n'"      got: $out"
fi

# --- a missing body file is void, not a crash ----------------------------------
out=$(python3 "$prog" 400 "$work/does-not-exist.json" 2>&1); rc=$?
if [ "${out%% *}" = void ] && [ "$rc" = 0 ]; then
  ok "a missing body file is void, not a traceback"
else
  bad "a missing body file is void, not a traceback"$'\n'"      rc=$rc out: $out"
fi

# --- a nonsense code is void, not accepted -------------------------------------
case_is "an unrecognised code" 418 '' void

echo
if [ "$fails" = 0 ]; then echo "$n checks, all good"; else echo "$n checks, $fails failed"; fi
[ "$fails" = 0 ]
