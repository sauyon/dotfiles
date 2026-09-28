#!/usr/bin/env bash
# Removes ONE kid from the published device JWKS and times how long Google STS
# keeps accepting a JWT signed by the key behind it (report A8, experiment
# 2a-removal).
#
#   ./install/wif/revoke-kid.sh <kid> [key-to-test-with]
#
# Run it from the DEVICE holding the key you want to time -- that is where the
# private half is, and it is the only place that can sign. The bucket write is
# still done on the admin host over ssh (KO_ADMIN_HOST), because Google
# credentials deliberately do not exist on the device hosts. Without a key
# argument it only publishes, which is the right mode when the key is gone.
#
# This is the step that makes a key migration a migration. Publishing a new key
# ADDS an identity; nothing is taken away until its predecessor's kid leaves the
# JWKS, because the JWKS is where authority lives. A private key whose kid is
# gone is inert wherever it sits -- and, conversely, deleting the key file while
# its kid is still published removes nothing at all.
#
# It is also the most dangerous command in this kit: it is instant across the
# whole fleet, and an enrolled host cannot re-authorise itself (it holds no
# Google credential). The refusals live in jwks-remove-kid.py, with cases in
# tests/wif-jwks.sh. GCS object versioning on the bucket is the real rollback;
# the pre-change JWKS is also saved locally, below, so you do not have to reach
# for it from memory.
set -uo pipefail

kid="${1:-}"
testkey="${2:-}"
admin="${KO_ADMIN_HOST:-10.0.7.100}"
bucket="${KO_JWKS_URI:-gs://ko-keys-sauyon/hosts/.well-known/jwks.json}"
issuer="${KO_WIF_ISSUER:-https://storage.googleapis.com/ko-keys-sauyon/hosts}"
audience="${KO_WIF_AUDIENCE:-//iam.googleapis.com/projects/484956590837/locations/global/workloadIdentityPools/ko-hosts/providers/bucket}"
subject="${KO_WIF_SUB:-device:$(uname -n | cut -d. -f1)}"
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
signer="$repo/home/scripts/ko-wif-token.py"
SSH=(ssh -o BatchMode=yes -o ConnectTimeout=10)

if [ -z "$kid" ]; then
  echo "usage: $0 <kid> [key-to-test-with]" >&2
  echo "  removes <kid> from $bucket" >&2
  echo "  with a key, also times how long STS keeps accepting that key" >&2
  exit 1
fi

# Everything checkable is checked BEFORE the irreversible act. Discovering the
# key path was mistyped after the upload costs the experiment outright: the kid
# you needed to time is already gone and cannot be put back to try again.
if [ -n "$testkey" ]; then
  [ -r "$testkey" ] || { echo "cannot read the test key: $testkey" >&2; exit 1; }
  if ! python3 "$signer" --key "$testkey" --jwk >/dev/null 2>&1; then
    echo "$testkey is not a key ko-wif-token can read; fix that before revoking anything" >&2
    exit 1
  fi
fi

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

curl -fsS --max-time 20 "$issuer/.well-known/jwks.json" -o "$work/before.json" \
  || { echo "could not fetch the current JWKS from $issuer" >&2; exit 1; }

# Keep the pre-change JWKS where a rollback can find it. out/ is gitignored.
mkdir -p "$here/out"
saved="$here/out/jwks-before-$(date -u '+%Y%m%dT%H%M%SZ').json"
cp "$work/before.json" "$saved"

# The refusals -- empty result, duplicate kids, absent kid, malformed body --
# all live here, tested by tests/wif-jwks.sh against crafted input.
python3 "$here/jwks-remove-kid.py" "$kid" "$work/before.json" "$work/after.json" || exit 1

published() { # what the bucket actually serves right now, as a kid list
  curl -fsS --max-time 20 "$issuer/.well-known/jwks.json" 2>/dev/null \
    | python3 -c 'import json,sys
try: print(" ".join(str(k.get("kid")) for k in json.load(sys.stdin)["keys"]))
except Exception as e: print(f"(unreadable: {e})")' 2>/dev/null \
    || echo "(could not fetch)"
}

echo "publishing the reduced JWKS via $admin ..."
rd=$("${SSH[@]}" "$admin" 'mktemp -d ~/.jwks-publish.XXXXXX') \
  || { echo "could not reach the admin host $admin" >&2; exit 1; }
# %q the remote path into the trap: it is interpolated now (it must be, to
# survive the variable going out of scope) and then run through a remote shell.
printf -v rdq '%q' "$rd"
# shellcheck disable=SC2064  # deliberate: expand $work and $rdq at trap-set time
trap "rm -rf '$work'; ${SSH[*]} '$admin' \"rm -rf $rdq\" >/dev/null 2>&1" EXIT
scp -q -o BatchMode=yes -o ConnectTimeout=10 "$work/after.json" "$admin:$rd/jwks.json" \
  || { echo "scp to $admin failed; the JWKS is unchanged" >&2; exit 1; }
if ! "${SSH[@]}" "$admin" "export PATH=/nix/var/nix/profiles/default/bin:\$PATH
  nix shell nixpkgs#google-cloud-sdk -c gcloud storage cp --content-type=application/json \
    --cache-control='public, max-age=60' '$rd/jwks.json' '$bucket'"; then
  # Do not claim "unchanged" -- gcloud can fail after the object has landed, and
  # that is the worst thing to be wrong about, because the operator stops here.
  echo "the upload reported failure. What the bucket serves NOW:" >&2
  echo "  $(published)" >&2
  exit 1
fi
start=$(date +%s)
echo "removed $kid at $(date -u '+%H:%M:%SZ'); the bucket now serves:"
echo "  $(published)"
echo "(pre-change JWKS saved at $saved; the bucket is versioned as well)"

[ -n "$testkey" ] || { echo "no test key given, so nothing to time. Done."; exit 0; }

sts() { # sts <token file> -> prints http code; body in $work/sts.json
  : > "$work/sts.json"   # never let a previous round's body be read as this one's
  curl -sS --max-time 20 -o "$work/sts.json" -w '%{http_code}' https://sts.googleapis.com/v1/token \
    --data-urlencode grant_type=urn:ietf:params:oauth:grant-type:token-exchange \
    --data-urlencode "audience=$audience" \
    --data-urlencode scope=https://www.googleapis.com/auth/cloud-platform \
    --data-urlencode requested_token_type=urn:ietf:params:oauth:token-type:access_token \
    --data-urlencode subject_token_type=urn:ietf:params:oauth:token-type:jwt \
    --data-urlencode "subject_token@$1"
}

echo "timing how long STS keeps accepting a JWT signed by $testkey ..."
for _ in $(seq 1 180); do
  # A fresh token each round: a cached one expires on its own and would read as
  # a revocation that never happened. printf %s, not the signer's trailing
  # newline -- `--data-urlencode name@file` sends the file RAW, so a newline goes
  # on the wire as %0A and STS may reject the token for that instead of for the
  # revocation, turning round 1 into a bogus "revoked after 0s".
  if ! tok=$(python3 "$signer" --key "$testkey" --iss "$issuer" --sub "$subject" --aud "$audience" 2>&1); then
    echo "could not sign with $testkey: $(printf '%s' "$tok" | tail -1)" >&2
    echo "the removal itself succeeded; the timing is unknown, not zero." >&2
    exit 2
  fi
  (umask 077; printf '%s' "$tok" > "$work/tok.jwt")
  code=$(sts "$work/tok.jwt")
  t=$(( $(date +%s) - start ))
  case "$code" in
    200) echo "  t+${t}s: still accepted" ;;
    400|401)
      # Only an auth refusal is the revocation landing. Anything else that is
      # not 200 is the network or Google having a bad minute, and reporting it
      # as a revocation would put a fabricated number in the report.
      why=$(python3 -c 'import json,sys
try:
    d = json.load(open(sys.argv[1])); print(d.get("error"), "-", str(d.get("error_description"))[:120])
except Exception: print("(no JSON body)")' "$work/sts.json" 2>/dev/null)
      echo "REJECTED after ${t}s (http $code): $why"
      exit 0 ;;
    *)
      echo "  t+${t}s: http $code -- transport or server error, not a revocation; retrying" >&2 ;;
  esac
  sleep 10
done
echo "still accepted after $(( $(date +%s) - start ))s -- longer than this script waits." >&2
exit 1
