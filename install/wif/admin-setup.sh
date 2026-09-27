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
out="$(cd "$(dirname "$0")" && pwd)/out"; mkdir -p "$out"

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
printf 'canary: hello-from-host-key %s\n' "$(date -u +%FT%TZ)" > "$out/test-secrets.plain.yaml"
if command -v sops >/dev/null; then
  # needs ADC as the human: gcloud auth application-default login (revoke afterwards, see report A6.3)
  sops --encrypt --gcp-kms "$KEYRES" "$out/test-secrets.plain.yaml" > "$out/test-secrets.yaml" || \
    echo "WARN: sops encrypt failed (run: gcloud auth application-default login; then re-run)"
else
  echo "WARN: sops not installed here; run: nix run nixpkgs#sops -- --encrypt --gcp-kms $KEYRES $out/test-secrets.plain.yaml > $out/test-secrets.yaml"
fi
rm -f "$out/test-secrets.plain.yaml"

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
