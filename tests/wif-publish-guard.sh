#!/usr/bin/env bash
# Cases for install/wif/jwks-publish-guard.py -- the preflight that stands between
# enrolling a host and de-authorising the fleet.
#
# `admin-setup.sh` builds the published key set with `jq -s '{keys: .}' "$@"` and
# uploads exactly that. The set is therefore whatever files happened to be on the
# command line: hand it the five new hosts' JWKs and shiori is gone, silently,
# with every visible signal green -- jq succeeds, the upload succeeds, the bucket
# serves a valid JWKS, and the discovery document still resolves. Item 3 runs
# this command once per enrolled identity, which is five more chances to type it
# with one file missing.
#
# The item-2 handoff named this as the thing item 3 multiplies: "admin-setup.sh
# *replaces* the key set with exactly what it is handed." `jwks-remove-kid.py`
# guards the deliberate removal path. Nothing guarded the replace path, which is
# the one nobody thinks of as a removal at all.
#
# The asymmetry measured in A6.5 decides how this guard has to behave. An
# addition is live in ~15 s; a removal has no measured bound and was still
# accepted 21.7 h later. So a mistaken drop does NOT lock anyone out promptly --
# it quietly leaves the fleet running on keys the published set no longer lists,
# and the operator finds out when a host reboots a day later. There is no fast
# feedback to catch this, which is exactly why it has to be caught before the
# upload rather than after.
#
#   ./tests/wif-publish-guard.sh
#
# No network. Every case is a crafted pair of key sets.
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
prog="$repo/install/wif/jwks-publish-guard.py"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

# Every "refuse" case below asserts a non-zero exit, and a program that does not
# exist also exits non-zero. Without this gate a missing or unrunnable guard
# would show up as a suite that mostly passes -- the refusals "working" for the
# one reason that means nothing is being checked at all.
if [ ! -f "$prog" ]; then
  echo "FAIL 0 - $prog does not exist; every refusal case below would pass vacuously" >&2
  echo; echo "0 checks, 1 failed"; exit 1
fi
if ! python3 "$prog" --selftest >/dev/null 2>&1; then
  echo "FAIL 0 - $prog is not runnable (python3 $prog --selftest failed)" >&2
  echo; echo "0 checks, 1 failed"; exit 1
fi

jwk() { printf '{"kty":"EC","crv":"P-256","kid":"%s","x":"x%s","y":"y%s"}' "$1" "$1" "$1"; }
set_of() { local out="" k; for k in "$@"; do out="$out${out:+,}$(jwk "$k")"; done; printf '{"keys":[%s]}' "$out"; }

# run <name> <proposed json> <live json> <expect ok|refuse> [extra args...]
run() {
  local name="$1" prop="$2" live="$3" expect="$4"; shift 4
  printf '%s' "$prop" > "$work/proposed.json"
  printf '%s' "$live" > "$work/live.json"
  local err rc
  err=$(python3 "$prog" "$work/proposed.json" "$work/live.json" "$@" 2>&1 >/dev/null); rc=$?
  if printf '%s' "$err" | grep -qi traceback; then
    bad "$name: traceback"$'\n'"      $err"; return
  fi
  if [ "$expect" = refuse ]; then
    if [ "$rc" = 0 ]; then bad "$name: expected a refusal, got exit 0"; else ok "$name"; fi
  else
    if [ "$rc" = 0 ]; then ok "$name"; else bad "$name: expected exit 0, got $rc"$'\n'"      $err"; fi
  fi
}

# --- the safe shapes ------------------------------------------------------------
run "publishing the same set is fine"          "$(set_of A B)" "$(set_of A B)" ok
run "adding a key alongside the live ones"     "$(set_of A B C)" "$(set_of A B)" ok
run "first publish, nothing live yet"          "$(set_of A)" '{"keys":[]}' ok

# --- the footgun this exists for ------------------------------------------------
run "dropping a live kid is refused"           "$(set_of B)" "$(set_of A B)" refuse
run "dropping every live kid is refused"       "$(set_of C)" "$(set_of A B)" refuse
# The realistic shape: enrolling five, forgetting the host already enrolled.
run "enrolling new hosts while omitting the incumbent" \
    "$(set_of N1 N2 N3 N4 N5)" "$(set_of SHIORI)" refuse

# --- a drop is allowed only when it is named ------------------------------------
run "a named drop is allowed"                  "$(set_of B)" "$(set_of A B)" ok --allow-drop A
run "naming one of two drops is still refused" "$(set_of C)" "$(set_of A B)" refuse --allow-drop A
run "naming both drops is allowed"             "$(set_of C)" "$(set_of A B)" ok --allow-drop A --allow-drop B
# Naming a kid that is not live means the operator's picture of the bucket is
# stale -- the most likely reason being that someone else republished. Refusing
# is the cheap way to make them look before a replace.
run "naming a drop that is not live is refused" "$(set_of A B)" "$(set_of A B)" refuse --allow-drop GONE

# --- malformed input is refused, never published --------------------------------
run "a proposed set with no usable key"        '{"keys":[]}' "$(set_of A)" refuse
run "a proposed set that is not a JWKS"        '{"nope":1}' "$(set_of A)" refuse
run "proposed junk that does not parse"        'not json'   "$(set_of A)" refuse
run "a duplicate kid in the proposed set"      "$(set_of A A)" "$(set_of A)" refuse
run "an entry with no kid in the proposed set" '{"keys":[{"kty":"EC","crv":"P-256","x":"x","y":"y"}]}' "$(set_of A)" refuse

# --- the refusal has to name the kids, or the operator cannot act on it ----------
printf '%s' "$(set_of B)" > "$work/proposed.json"
printf '%s' "$(set_of A B)" > "$work/live.json"
msg=$(python3 "$prog" "$work/proposed.json" "$work/live.json" 2>&1 >/dev/null)
if printf '%s' "$msg" | grep -q "A"; then
  ok "the refusal names the kid that would be dropped"
else
  bad "the refusal names the kid that would be dropped"$'\n'"      got: $msg"
fi

# --- a missing live file is NOT treated as "nothing is published" ----------------
# That is the dangerous default: if the fetch failed, an empty live set makes
# every drop invisible and the guard waves through exactly the publish it exists
# to stop.
rm -f "$work/live.json"
printf '%s' "$(set_of A)" > "$work/proposed.json"
if ! python3 "$prog" "$work/proposed.json" "$work/live.json" >/dev/null 2>&1; then
  ok "an unreadable live set is refused, not treated as empty"
else
  bad "an unreadable live set is refused, not treated as empty"
fi

echo
if [ "$fails" = 0 ]; then echo "$n checks, all good"; else echo "$n checks, $fails failed"; fi
[ "$fails" = 0 ]
