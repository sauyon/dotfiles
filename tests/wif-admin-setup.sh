#!/usr/bin/env bash
# Static contracts for install/wif/admin-setup.sh.
#
# This is the script that publishes the trust root. It cannot be exercised
# offline -- every real path talks to GCS, IAM and KMS -- so these are static
# checks on the invariants that were each established by an incident, and whose
# removal would be silent. A static check is weak evidence that the script
# works and strong evidence that a specific mistake has not come back, which is
# what is wanted here.
#
#   ./tests/wif-admin-setup.sh
#
# No network.
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
prog="$repo/install/wif/admin-setup.sh"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

[ -f "$prog" ] || { echo "FAIL 0 - $prog missing"; echo; echo "0 checks, 1 failed"; exit 1; }
src=$(cat "$prog")

has() { printf '%s' "$src" | grep -q -- "$1"; }

# --- the replace footgun --------------------------------------------------------
# The published set is `jq -s '{keys: .}' "$@"`, so it is exactly the files on the
# command line and the upload replaces the object. Omitting one silently
# de-authorises that host, and per A6.5 removals have no measured upper bound, so
# nothing fails until a day later.
if has 'jwks-publish-guard.py'; then
  ok "the publish runs jwks-publish-guard.py"
else
  bad "the publish runs jwks-publish-guard.py"$'\n'\
"      without it, omitting a JWK silently drops that host from the trust root."
fi

# The guard is worthless if the upload can still happen after it refuses.
guard_line=$(printf '%s' "$src" | grep -n 'jwks-publish-guard.py' | head -1 | cut -d: -f1)
upload_line=$(printf '%s' "$src" | grep -n "storage cp .*jwks.json" | head -1 | cut -d: -f1)
if [ -n "$guard_line" ] && [ -n "$upload_line" ] && [ "$guard_line" -lt "$upload_line" ]; then
  ok "the guard runs BEFORE the jwks.json upload (line $guard_line < $upload_line)"
else
  bad "the guard runs BEFORE the jwks.json upload"$'\n'"      guard=$guard_line upload=$upload_line"
fi

# --- a failed fetch must not read as "nothing is published" ---------------------
# Treating an unreadable live set as empty makes every removal invisible and waves
# through exactly the publish the guard exists to stop. Only a genuine 404 is the
# first publish.
if has '404)' && has 'first publish'; then
  ok "a 404 is treated as the first publish, distinctly from a failed fetch"
else
  bad "a 404 is treated as the first publish, distinctly from a failed fetch"
fi
if printf '%s' "$src" | grep -A2 'could not read the live JWKS' | grep -q 'exit 1'; then
  ok "any other fetch failure refuses instead of assuming an empty live set"
else
  bad "any other fetch failure refuses instead of assuming an empty live set"
fi

# --- sops: two different failures, only one of which is about credentials -------
# .sops.yaml's only creation rule is `path_regex: secrets.yaml`, so encrypting a
# file under install/wif/out/ matches no rule and sops refuses BEFORE it ever
# reaches GCP -- "error loading config: no matching creation rules found".
# Passing --gcp-kms does not bypass that; only --config does. Observed live on
# 2026-09-30.
if printf '%s' "$src" | grep -q 'sops.*--config'; then
  ok "sops is invoked with --config, so a non-matching path is not a creation-rule error"
else
  bad "sops is invoked with --config"$'\n'\
"      Without it, encrypting install/wif/out/<f> dies with \"no matching creation"$'\n'\
"      rules found\" because .sops.yaml only matches secrets.yaml -- before GCP is"$'\n'\
"      ever contacted."
fi

# The warning must not send the operator to an ADC login for a failure that is not
# about credentials. A6.3: the refresh token that login leaves in ~/.config/gcloud
# is what Shai-Hulud wave 1 harvested, and A6.3 requires revoking it afterwards.
# Telling someone to run it unnecessarily has a real cost.
warn=$(printf '%s' "$src" | grep -i 'WARN.*sops' || true)
if printf '%s' "$warn" | grep -q 'application-default login' \
   && ! printf '%s' "$warn" | grep -qi 'creation rule'; then
  bad "the sops warning does not blame ADC for every failure"$'\n'\
"      It names \`gcloud auth application-default login\` unconditionally, but the"$'\n'\
"      failure actually seen was a creation-rule mismatch. A6.3 makes that login"$'\n'\
"      a thing to do deliberately, not on a misdiagnosis."$'\n'\
"      got: $warn"
else
  ok "the sops warning does not blame ADC for every failure"
fi

# --- the admin's restricted key must not be picked up silently ------------------
if has 'unset GOOGLE_APPLICATION_CREDENTIALS'; then
  ok "GOOGLE_APPLICATION_CREDENTIALS is unset before any gcloud call"
else
  bad "GOOGLE_APPLICATION_CREDENTIALS is unset before any gcloud call"
fi

echo
if [ "$fails" = 0 ]; then echo "$n checks, all good"; else echo "$n checks, $fails failed"; fi
[ "$fails" = 0 ]
