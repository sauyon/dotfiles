#!/usr/bin/env bash
# Cases for install/wif/jwks-remove-kid.py — the refusals that stand between one
# mistyped argument and every enrolled host failing to decrypt its secrets.
#
# Removing a kid from the published JWKS is the only operation in the WIF kit
# that is both irreversible in effect (a host cannot re-authorise itself; it has
# no Google credential) and instant across the whole fleet. GCS object versioning
# is the rollback, and it is not one anybody performs calmly. So the refusals get
# cases, and they get them here rather than against the live bucket: every case
# below runs on a crafted file and touches no network at all.
#
#   ./tests/wif-jwks.sh
#
# Run it from anywhere; paths resolve against the repo this file lives in.
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
prog="$repo/install/wif/jwks-remove-kid.py"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT
jwk() { printf '{"kty":"EC","crv":"P-256","kid":"%s","x":"x","y":"y"}' "$1"; }

# run <name> <json> <kid> <expect: ok|refuse> [expected kids, space separated]
# On "refuse" the output file must NOT be written: a refusal that still leaves a
# reduced JWKS on disk invites the operator to upload it by hand.
run() {
  local name="$1" json="$2" kid="$3" expect="$4" want="${5:-}" err rc got
  printf '%s' "$json" > "$work/in.json"
  rm -f "$work/out.json"
  err=$(python3 "$prog" "$kid" "$work/in.json" "$work/out.json" 2>&1 >/dev/null); rc=$?
  if [ "$expect" = refuse ]; then
    if [ "$rc" = 0 ]; then
      bad "$name: expected a refusal, got exit 0"$'\n'"      wrote: $(cat "$work/out.json" 2>/dev/null)"
    elif [ -e "$work/out.json" ]; then
      bad "$name: refused (good) but still wrote $work/out.json"$'\n'"      $(cat "$work/out.json")"
    elif printf '%s' "$err" | grep -qi 'traceback'; then
      bad "$name: refused with a traceback rather than a message"$'\n'"$(printf '%s' "$err" | tail -3 | sed 's/^/      /')"
    else
      ok "$name: refused — $(printf '%s' "$err" | head -1 | cut -c1-72)"
    fi
    return
  fi
  if [ "$rc" != 0 ]; then
    bad "$name: expected success, got exit $rc"$'\n'"      $err"; return
  fi
  # .get, not ["kid"]: a kid-less entry is a case below, and indexing here would
  # report the tool as having dropped it when it kept it.
  got=$(python3 -c 'import json,sys; print(" ".join((k.get("kid") if isinstance(k, dict) else None) or "-" for k in json.load(open(sys.argv[1]))["keys"]))' "$work/out.json" 2>/dev/null)
  if [ "$got" = "$want" ]; then ok "$name: kept [$got]"
  else bad "$name"$'\n'"      want keys: [$want]"$'\n'"      got keys:  [$got]"; fi
}

# ── the happy path ──────────────────────────────────────────────────────────
run "removes the named kid and keeps the rest" \
    "{\"keys\":[$(jwk AAA),$(jwk BBB),$(jwk CCC)]}" BBB ok "AAA CCC"

# ── the refusals ────────────────────────────────────────────────────────────
run "refuses a kid that is not published" \
    "{\"keys\":[$(jwk AAA),$(jwk BBB)]}" ZZZ refuse

run "refuses to empty a one-key JWKS" \
    "{\"keys\":[$(jwk AAA)]}" AAA refuse

# The case that made this file exist. A guard written as `len(keys) <= 1` before
# the removal passes here and then publishes {"keys":[]}. admin-setup.sh is
# `jq -s '{keys: .}' "$@"` and deduplicates nothing, while tpm-keygen.sh tells the
# operator to pass "EVERY current key" by name -- so naming one file twice, or a
# file and a copy of it, produces exactly this JWKS.
run "refuses when every remaining key carries the kid being removed (duplicates)" \
    "{\"keys\":[$(jwk AAA),$(jwk AAA)]}" AAA refuse

# ── malformed input fails closed, and says so in words ──────────────────────
run "refuses a JWKS with no keys array" '{"nope":1}' AAA refuse
run "refuses a keys value that is not a list" '{"keys":"AAA"}' AAA refuse
run "refuses a top-level array" '[{"kid":"AAA"}]' AAA refuse
run "refuses an empty keys array" '{"keys":[]}' AAA refuse
# A captive portal or a truncated transfer hands the caller HTML, not JSON.
run "refuses a body that is not JSON at all" '<html>Sign in to continue</html>' AAA refuse

# A key with no kid belongs to someone else and must survive untouched -- and
# must not crash the tool on the way past.
run "keeps a kid-less entry rather than crashing on it" \
    "{\"keys\":[{\"kty\":\"EC\"},$(jwk AAA),$(jwk BBB)]}" AAA ok "- BBB"

# Surviving entries must be USABLE keys, not merely present. Counting entries is
# not the invariant: a JWKS whose only survivor is not a JWK is as complete a
# lockout as an empty one, and Google will not tell you which it was.
#
# This exact shape is one operator slip away. admin-setup.sh is
# `jq -s '{keys: .}' "$@"`, the operator is told to name "EVERY current key", and
# out/jwks.json -- a nested JWKS, written by that same script -- sits in the same
# directory as out/<host>.jwk.json.
run "refuses when the only survivor is a nested JWKS, not a key" \
    "{\"keys\":[$(jwk AAA),{\"keys\":[$(jwk BBB)]}]}" AAA refuse

run "refuses when the only survivor is not an object at all" \
    "{\"keys\":[$(jwk AAA),\"junk\"]}" AAA refuse

run "refuses when the only survivor is an object with no kid" \
    "{\"keys\":[$(jwk AAA),{\"kty\":\"EC\"}]}" AAA refuse

# ...but a kid-less or junk entry alongside a REAL surviving key is fine: it is
# someone else's business and must pass through untouched.
run "keeps junk alongside a usable survivor" \
    "{\"keys\":[$(jwk AAA),\"junk\",$(jwk BBB)]}" AAA ok "- BBB"

# Fields outside "keys" are not ours to drop: the JWKS may grow siblings.
printf '{"keys":[%s,%s],"extra":"keep me"}' "$(jwk AAA)" "$(jwk BBB)" > "$work/in.json"
rm -f "$work/out.json"
if python3 "$prog" AAA "$work/in.json" "$work/out.json" >/dev/null 2>&1 \
   && [ "$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("extra"))' "$work/out.json")" = "keep me" ]; then
  ok "preserves unknown top-level fields"
else
  bad "preserves unknown top-level fields"
fi

echo
if [ "$fails" = 0 ]; then echo "$n checks, all good"; else echo "$n checks, $fails failed"; fi
[ "$fails" = 0 ]
