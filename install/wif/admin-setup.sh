#!/usr/bin/env bash
# ADMIN side. Run on a machine where `gcloud auth login` is done as the human owner of the
# GCP project holding nix-keyring. NEVER run this on the bare test host.
#
# Creates (idempotently): bucket with a static OIDC issuer under hosts/, WIF pool+provider
# trusting that issuer, KMS key host-key, IAM (decrypt for the pool, encrypt+decrypt for you),
# a KMS canary ciphertext, and a sops-encrypted test file. Writes everything the device
# needs into ./out/ (copy that directory back to the device).
#
# Usage: ./admin-setup.sh out/<host>.jwk.json [more.jwk.json ...]
set -euo pipefail
PROJECT="${PROJECT:-sauyon-dotfiles}"
BUCKET="${BUCKET:-ko-keys-sauyon}"           # bucket names are global; change if taken
LOCATION="${LOCATION:-us}"
POOL="${POOL:-ko-hosts}"
PROVIDER="${PROVIDER:-bucket}"
KEYRING="${KEYRING:-nix-keyring}"
KEY="${KEY:-host-key}"
[ $# -ge 1 ] || { echo "usage: $0 out/<host>.jwk.json [...]" >&2; exit 2; }
for f in "$@"; do [ -s "$f" ] || { echo "missing $f" >&2; exit 2; }; done
here="$(cd "$(dirname "$0")" && pwd)"
out="$here/out"; mkdir -p "$out"

# The admin's gcp-key.json (SA with nix-key only) must NOT be used here: it cannot create
# anything and, if GOOGLE_APPLICATION_CREDENTIALS points at it, sops/gcloud silently use it.
unset GOOGLE_APPLICATION_CREDENTIALS GOOGLE_CREDENTIALS
gcloud config set project "$PROJECT" >/dev/null
ACCOUNT="$(gcloud config get-value account 2>/dev/null)"
echo "project=$PROJECT account=$ACCOUNT"
gcloud services enable iam.googleapis.com iamcredentials.googleapis.com sts.googleapis.com cloudkms.googleapis.com storage.googleapis.com
PROJECT_NUMBER="$(gcloud projects describe "$PROJECT" --format='value(projectNumber)')"

# --- bucket + static issuer ---------------------------------------------------------------
if ! gcloud storage buckets describe "gs://$BUCKET" >/dev/null 2>&1; then
  gcloud storage buckets create "gs://$BUCKET" --location="$LOCATION" --uniform-bucket-level-access
fi
gcloud storage buckets update "gs://$BUCKET" --versioning >/dev/null
PUBLIC=1
if ! gcloud storage buckets add-iam-policy-binding "gs://$BUCKET" --member=allUsers --role=roles/storage.objectViewer >/dev/null 2>&1; then
  echo "WARN: could not make bucket public (org policy publicAccessPrevention?). Will upload JWKS to the provider directly instead (--jwk-json-path; max 8 keys, manual rotation)."
  PUBLIC=0
fi
ISSUER="https://storage.googleapis.com/$BUCKET/hosts"
jq -s '{keys: .}' "$@" > "$out/jwks.json"

# The set published below is EXACTLY the files on this command line, and the
# upload replaces the object outright. Leaving one out is a silent
# de-authorisation with every visible signal green, and per A6.5 it produces no
# prompt symptom either: removals have no measured upper bound, so the dropped
# host keeps working for an unknown period and fails to activate a day later,
# long after anyone connects it to this command. Item 3 runs this once per
# enrolled identity, so guard it every time.
#
# Fetch the live set first, and distinguish the two ways it can be absent: a
# genuine 404 is the first publish and there is nothing to drop; anything else
# is a failed fetch, and treating THAT as an empty set would hide every removal.
live="$out/jwks-live.json"
if [ "$PUBLIC" = 1 ]; then
  code=$(curl -sS --max-time 20 -o "$live" -w '%{http_code}' \
    "$ISSUER/.well-known/jwks.json?_=$(date +%s%N)" 2>/dev/null) || code=000
  case "$code" in
    200) : ;;
    404) echo "no JWKS published yet at $ISSUER -- treating this as the first publish"
         echo '{"keys":[]}' > "$live" ;;
    *)   echo "could not read the live JWKS (http $code); refusing to replace an object" >&2
         echo "whose current contents are unknown. Nothing has been changed." >&2
         exit 1 ;;
  esac
else
  # On the non-public path the key set lives on the PROVIDER (--jwk-json-path),
  # not in a fetchable object, so there is nothing to diff over HTTP. The replace
  # footgun is identical there and this guard does not cover it; read the current
  # set with `gcloud iam workload-identity-pools providers describe` before
  # re-running. Saying so is better than silently fetching nothing and calling
  # the publish guarded.
  echo "WARN: bucket is not public, so the key set is held on the provider and this" >&2
  echo "      publish is NOT guarded against dropping a kid. Check the provider's" >&2
  echo "      current key set by hand before continuing." >&2
  echo '{"keys":[]}' > "$live"
fi
# KO_ALLOW_DROP: space-separated kids this publish is permitted to remove. Empty
# by default, so a removal is never something that just happens.
gargs=(); for k in ${KO_ALLOW_DROP:-}; do gargs+=(--allow-drop "$k"); done
if ! python3 "$here/jwks-publish-guard.py" "$out/jwks.json" "$live" "${gargs[@]}"; then
  echo >&2
  echo "Nothing has been changed. Pass every JWK that should stay published --" >&2
  echo "including the hosts already enrolled -- or set KO_ALLOW_DROP='<kid>...'." >&2
  exit 1
fi

cat > "$out/openid-configuration" <<EOF
{"issuer":"$ISSUER","jwks_uri":"$ISSUER/.well-known/jwks.json","response_types_supported":["id_token"],"subject_types_supported":["public"],"id_token_signing_alg_values_supported":["ES256","RS256"],"claims_supported":["sub","aud","exp","iat","iss"]}
EOF
gcloud storage cp --content-type=application/json --cache-control='public, max-age=60' "$out/openid-configuration" "gs://$BUCKET/hosts/.well-known/openid-configuration"
gcloud storage cp --content-type=application/json --cache-control='public, max-age=60' "$out/jwks.json" "gs://$BUCKET/hosts/.well-known/jwks.json"
if [ "$PUBLIC" = 1 ]; then
  curl -fsS "$ISSUER/.well-known/openid-configuration" | jq -c . || { echo "discovery doc not publicly fetchable" >&2; exit 1; }
fi

# --- WIF pool + provider --------------------------------------------------------------------
gcloud iam workload-identity-pools describe "$POOL" --location=global >/dev/null 2>&1 || \
  gcloud iam workload-identity-pools create "$POOL" --location=global --display-name="ko dotfiles devices"
AUDIENCE="//iam.googleapis.com/projects/$PROJECT_NUMBER/locations/global/workloadIdentityPools/$POOL/providers/$PROVIDER"
pargs=(--location=global --workload-identity-pool="$POOL" --issuer-uri="$ISSUER"
       --attribute-mapping="google.subject=assertion.sub"
       --attribute-condition='assertion.sub.startsWith("device:")')
[ "$PUBLIC" = 1 ] || pargs+=(--jwk-json-path="$out/jwks.json")
if gcloud iam workload-identity-pools providers describe "$PROVIDER" --location=global --workload-identity-pool="$POOL" >/dev/null 2>&1; then
  gcloud iam workload-identity-pools providers update-oidc "$PROVIDER" "${pargs[@]}"
else
  gcloud iam workload-identity-pools providers create-oidc "$PROVIDER" "${pargs[@]}"
fi

# --- KMS key + IAM ---------------------------------------------------------------------------
KEYRES="projects/$PROJECT/locations/global/keyRings/$KEYRING/cryptoKeys/$KEY"
gcloud kms keys describe "$KEY" --keyring="$KEYRING" --location=global >/dev/null 2>&1 || \
  gcloud kms keys create "$KEY" --keyring="$KEYRING" --location=global --purpose=encryption
gcloud kms keys add-iam-policy-binding "$KEY" --keyring="$KEYRING" --location=global \
  --member="principalSet://iam.googleapis.com/projects/$PROJECT_NUMBER/locations/global/workloadIdentityPools/$POOL/*" \
  --role=roles/cloudkms.cryptoKeyDecrypter >/dev/null
gcloud kms keys add-iam-policy-binding "$KEY" --keyring="$KEYRING" --location=global \
  --member="user:$ACCOUNT" --role=roles/cloudkms.cryptoKeyEncrypterDecrypter >/dev/null

# --- canary + sops test file ----------------------------------------------------------------
printf 'wif-canary %s' "$(date -u +%FT%TZ)" | gcloud kms encrypt --key="$KEY" --keyring="$KEYRING" --location=global \
  --plaintext-file=- --ciphertext-file=- | base64 -w0 > "$out/canary.b64"
# Trapped the moment it exists, not just removed on the happy path. This lands in
# out/ -- the directory the admin collects JWKs from -- and `set -e` means any
# failing command between here and the rm leaves a file named *secrets*.plain.yaml
# sitting in it. out/ has already proved it collects things nobody meant to leave.
plain="$out/test-secrets.plain.yaml"
trap 'rm -f "$plain" "$out/.sops.err"' EXIT
printf 'canary: hello-from-host-key %s\n' "$(date -u +%FT%TZ)" > "$plain"
if command -v sops >/dev/null; then
  # --config /dev/null is load-bearing, not tidiness. The repo's .sops.yaml has one
  # creation rule, `path_regex: secrets.yaml`, and this file is under install/wif/out/,
  # so sops matches no rule and refuses with "error loading config: no matching
  # creation rules found" -- BEFORE it ever contacts GCP. Passing --gcp-kms does not
  # bypass that. Observed live on 2026-09-30, where it was then misreported as a
  # credentials problem.
  if ! sops --config /dev/null --encrypt --gcp-kms "$KEYRES" \
       "$out/test-secrets.plain.yaml" > "$out/test-secrets.yaml" 2>"$out/.sops.err"; then
    rm -f "$out/test-secrets.yaml"
    # Two unrelated failures reach here and they need different actions. Saying
    # "log in" for the wrong one is not free: A6.3 records that the refresh token
    # `gcloud auth application-default login` leaves in ~/.config/gcloud is what
    # Shai-Hulud wave 1 harvested, and that it must be revoked afterwards. Do not
    # send anyone there on a misdiagnosis.
    if grep -qi 'creation rule' "$out/.sops.err"; then
      echo "WARN: sops refused on config, NOT on credentials:" >&2
      sed 's/^/      /' "$out/.sops.err" >&2
      echo "      This is the .sops.yaml creation_rules not matching $out/. It is a" >&2
      echo "      script bug if you see it -- --config /dev/null should prevent it." >&2
      echo "      Do NOT run an ADC login for this." >&2
    elif grep -qiE 'credential|default credentials|ADC' "$out/.sops.err"; then
      echo "WARN: sops could not authenticate to GCP. This step needs ADC as the human:" >&2
      echo "      gcloud auth application-default login   # then REVOKE it, see report A6.3" >&2
    else
      echo "WARN: sops encrypt failed:" >&2
      sed 's/^/      /' "$out/.sops.err" >&2
    fi
    echo "      (this only skips the optional test-secrets file; the JWKS, provider" >&2
    echo "       and IAM above are already applied)" >&2
  fi
else
  # Deliberately does NOT name $plain: the trap removes it when this script exits,
  # so an instruction operating on that path is impossible to follow by the time
  # anyone reads it. Regenerate the one-line canary instead.
  echo "WARN: sops not installed here, so the optional test-secrets file was skipped." >&2
  echo "      To create it later, from this directory:" >&2
  printf '        %s\n' "printf 'canary: hello-from-host-key %s\\n' \"\$(date -u +%FT%TZ)\" \\" >&2
  echo "          | nix run nixpkgs#sops -- --config /dev/null --encrypt --gcp-kms $KEYRES \\" >&2
  echo "              --input-type yaml --output-type yaml /dev/stdin > $out/test-secrets.yaml" >&2
fi
rm -f "$plain"   # belt and braces; the EXIT trap above is what actually guarantees it

cat > "$out/env.sh" <<EOF
PROJECT=$PROJECT
PROJECT_NUMBER=$PROJECT_NUMBER
BUCKET=$BUCKET
ISSUER=$ISSUER
POOL=$POOL
PROVIDER=$PROVIDER
AUDIENCE=$AUDIENCE
KEYRES=$KEYRES
PUBLIC_BUCKET=$PUBLIC
EOF
echo; echo "Done. Copy $out/ back to the device, then run device-test.sh there."
echo "Break-glass hygiene: if you ran 'gcloud auth application-default login' for sops, now run: gcloud auth application-default revoke"
