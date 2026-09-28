#!/usr/bin/env bash
# Admin side. Removes ONE kid from the published device JWKS and times how long
# Google STS keeps accepting a JWT signed by the key behind it (report A8,
# experiment 2a-removal).
#
#   ./install/wif/revoke-kid.sh <kid> [key-to-test-with]
#
# This is the step that makes a key migration a migration. Publishing a new key
# adds an identity; nothing is taken away until its predecessor's kid leaves the
# JWKS, because the JWKS is where authority lives. A private key whose kid is
# gone is inert, wherever it is sitting.
#
# It refuses to remove the last key, and it refuses a kid that is not there. It
# does NOT touch any private key: revocation and deletion are separate acts, and
# keeping the old key file is what lets this script measure anything.
#
# With a key argument it signs with that key each round and reports the moment
# STS starts refusing -- that number is the experiment. Without one it publishes
# and stops, which is the right mode when the key is genuinely gone.
#
# Run from anywhere; the gcloud upload goes through the admin host over ssh,
# because Google credentials deliberately do not exist on the device hosts.
set -uo pipefail

kid="${1:-}"
testkey="${2:-}"
admin="${KO_ADMIN_HOST:-10.0.7.100}"
bucket="${KO_JWKS_URI:-gs://ko-keys-sauyon/hosts/.well-known/jwks.json}"
issuer="${KO_WIF_ISSUER:-https://storage.googleapis.com/ko-keys-sauyon/hosts}"
audience="${KO_WIF_AUDIENCE:-//iam.googleapis.com/projects/484956590837/locations/global/workloadIdentityPools/ko-hosts/providers/bucket}"
repo="$(cd "$(dirname "$0")/../.." && pwd)"
signer="$repo/home/scripts/ko-wif-token.py"

if [ -z "$kid" ]; then
  echo "usage: $0 <kid> [key-to-test-with]" >&2
  echo "  <kid> is removed from $bucket; the JWKS is fetched from $issuer first" >&2
  exit 1
fi

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

curl -fsS --max-time 20 "$issuer/.well-known/jwks.json" -o "$work/before.json" \
  || { echo "could not fetch the current JWKS from $issuer" >&2; exit 1; }

# Build the new set here and let python do the refusing: an empty JWKS locks
# every enrolled host out at once, and it is a one-line mistake to make.
if ! python3 - "$work/before.json" "$kid" "$work/after.json" <<'PY'
import json, sys
j = json.load(open(sys.argv[1]))
keys = j.get("keys", [])
kids = [k.get("kid") for k in keys]
if sys.argv[2] not in kids:
    sys.exit(f"kid {sys.argv[2]} is not in the published JWKS; it holds {kids}")
if len(keys) <= 1:
    sys.exit("refusing to remove the only key: that locks out every enrolled host")
j["keys"] = [k for k in keys if k.get("kid") != sys.argv[2]]
json.dump(j, open(sys.argv[3], "w"), separators=(",", ":"))
print("before:", kids)
print("after: ", [k["kid"] for k in j["keys"]])
PY
then exit 1; fi

echo "publishing the reduced JWKS via $admin ..."
rd=$(ssh -o BatchMode=yes "$admin" 'mktemp -d ~/.jwks-publish.XXXXXX') \
  || { echo "could not reach the admin host $admin" >&2; exit 1; }
# shellcheck disable=SC2064  # $rd must expand now: the trap has to survive it
trap "rm -rf '$work'; ssh -o BatchMode=yes '$admin' \"rm -rf '$rd'\" >/dev/null 2>&1" EXIT
scp -q "$work/after.json" "$admin:$rd/jwks.json" || { echo "scp failed" >&2; exit 1; }
ssh -o BatchMode=yes "$admin" "export PATH=/nix/var/nix/profiles/default/bin:\$PATH
  nix shell nixpkgs#google-cloud-sdk -c gcloud storage cp --content-type=application/json \
    --cache-control='public, max-age=60' '$rd/jwks.json' '$bucket'" || {
  echo "the upload failed; the JWKS is unchanged" >&2; exit 1; }
start=$(date +%s)
echo "removed $kid at $(date -u '+%H:%M:%SZ')"

if [ -z "$testkey" ]; then
  echo "no test key given, so nothing to time. Done."
  exit 0
fi
if [ ! -r "$testkey" ]; then
  echo "cannot read $testkey, so nothing to time. The removal itself succeeded." >&2
  exit 1
fi

sts() { # sts <token file> -> http code, body in $work/sts.json
  curl -sS -o "$work/sts.json" -w '%{http_code}' https://sts.googleapis.com/v1/token \
    --data-urlencode grant_type=urn:ietf:params:oauth:grant-type:token-exchange \
    --data-urlencode "audience=$audience" \
    --data-urlencode scope=https://www.googleapis.com/auth/cloud-platform \
    --data-urlencode requested_token_type=urn:ietf:params:oauth:token-type:access_token \
    --data-urlencode subject_token_type=urn:ietf:params:oauth:token-type:jwt \
    --data-urlencode "subject_token@$1"
}

echo "timing how long STS keeps accepting a JWT signed by $testkey ..."
for _ in $(seq 1 180); do
  # A fresh token each round: a cached one would expire on its own and read as a
  # revocation that never happened.
  (umask 077; python3 "$signer" --key "$testkey" --iss "$issuer" \
     --sub "device:$(uname -n | cut -d. -f1)" --aud "$audience" > "$work/tok.jwt" 2>/dev/null) \
    || { echo "could not sign with $testkey (clock? wrong key?)"; break; }
  code=$(sts "$work/tok.jwt")
  t=$(( $(date +%s) - start ))
  if [ "$code" != 200 ]; then
    echo "REJECTED after ${t}s (http $code): $(python3 -c 'import json,sys
d=json.load(open(sys.argv[1])); print(d.get("error"), "-", str(d.get("error_description"))[:100])' "$work/sts.json" 2>/dev/null)"
    exit 0
  fi
  echo "  t+${t}s: still accepted"
  sleep 10
done
echo "still accepted after $(( $(date +%s) - start ))s -- longer than this script waits."
exit 1
