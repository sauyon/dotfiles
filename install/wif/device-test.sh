#!/usr/bin/env bash
# Device side. Runs report A8 experiments 1a-1d and 2b against out/env.sh from admin-setup.sh.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"; out="$here/out"
host="$(uname -n)"; host="${host%%.*}"
key="${XDG_CONFIG_HOME:-$HOME/.config}/ko/experiment/wif.pem"
. "$out/env.sh"
jwt="$here/../../home/scripts/ko-wif-token.py"
pass(){ echo "PASS $*"; }; fail(){ echo "FAIL $*"; }
sts() { # $1 = subject jwt -> prints access token or error json
  curl -sS https://sts.googleapis.com/v1/token \
    --data-urlencode grant_type=urn:ietf:params:oauth:grant-type:token-exchange \
    --data-urlencode "audience=$AUDIENCE" \
    --data-urlencode scope=https://www.googleapis.com/auth/cloud-platform \
    --data-urlencode requested_token_type=urn:ietf:params:oauth:token-type:access_token \
    --data-urlencode subject_token_type=urn:ietf:params:oauth:token-type:jwt \
    --data-urlencode "subject_token=$1"
}
echo "== 1a/1b: static issuer + ES256 (sub=device:$host)"
J=$(python3 "$jwt" --key "$key" --iss "$ISSUER" --sub "device:$host" --aud "$AUDIENCE") || { fail "1a: could not sign (clock?)"; exit 1; }
R=$(sts "$J"); AT=$(jq -r '.access_token // empty' <<<"$R")
if [ -n "$AT" ]; then pass "1a+1b: STS accepted self-signed ES256 JWT from static issuer"; else fail "1a/1b: $(jq -c . <<<"$R")"; fi
echo "== 1c: direct KMS decrypt with federated token (no SA impersonation)"
if [ -n "$AT" ]; then
  R=$(curl -sS -H "Authorization: Bearer $AT" -H 'Content-Type: application/json' \
       -d "{\"ciphertext\":\"$(cat "$out/canary.b64")\"}" "https://cloudkms.googleapis.com/v1/$KEYRES:decrypt")
  PT=$(jq -r '.plaintext // empty' <<<"$R" | base64 -d 2>/dev/null)
  if [ -n "$PT" ]; then pass "1c: KMS decrypt -> '$PT'"; else fail "1c: $(jq -c . <<<"$R")"; fi
fi
echo "== 2b: sub not bound to key? (sign with $host key, assert sub=device:someone-else)"
J2=$(python3 "$jwt" --key "$key" --iss "$ISSUER" --sub "device:someone-else" --aud "$AUDIENCE")
R2=$(sts "$J2")
if [ -n "$(jq -r '.access_token // empty' <<<"$R2")" ]; then echo "CONFIRMED 2b: Google accepted a forged sub -> tier-only identity (report A3)"; else echo "SURPRISE 2b: rejected: $(jq -c . <<<"$R2")"; fi
echo "== 1d: sops via external_account credential config"
cfg="$out/wif-hosts.json"
cat > "$cfg" <<EOF
{"type":"external_account","audience":"$AUDIENCE","subject_token_type":"urn:ietf:params:oauth:token-type:jwt","token_url":"https://sts.googleapis.com/v1/token",
 "credential_source":{"executable":{"command":"$(command -v python3) $jwt --key $key --iss $ISSUER --sub device:$host --aud $AUDIENCE --adc","timeout_millis":10000}}}
EOF
if [ -s "$out/test-secrets.yaml" ]; then
  echo "-- 1d(i): sops CLI, executable source"
  if GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES=1 GOOGLE_APPLICATION_CREDENTIALS="$cfg" env -u GOOGLE_CREDENTIALS -u GOOGLE_OAUTH_ACCESS_TOKEN \
       nix run nixpkgs#sops -- -d "$out/test-secrets.yaml"; then pass "1d(i)"; else fail "1d(i)"; fi
  echo "-- 1d(ii): sops CLI, file source"
  cfgf="$out/wif-hosts-file.json"; tokf="${XDG_RUNTIME_DIR:-/tmp}/ko-wif.jwt"
  python3 "$jwt" --key "$key" --iss "$ISSUER" --sub "device:$host" --aud "$AUDIENCE" > "$tokf"
  jq --arg f "$tokf" '.credential_source={file:$f}' "$cfg" > "$cfgf"
  if GOOGLE_APPLICATION_CREDENTIALS="$cfgf" env -u GOOGLE_CREDENTIALS nix run nixpkgs#sops -- -d "$out/test-secrets.yaml"; then pass "1d(ii)"; else fail "1d(ii)"; fi
  echo "-- 1d(iii): the real sops-install-secrets binary from the failing unit, executable source"
  bin=$(grep -o '/nix/store/[^ ]*sops-install-secrets[^ ]*/bin/sops-install-secrets' "$(systemctl --user cat sops-nix.service | grep -o '/nix/store/[^ ]*sops-nix-user' | head -1)" | head -1)
  tmp=$(mktemp -d); : > "$tmp/age-empty.txt"
  cat > "$tmp/manifest.json" <<EOF
{"ageKeyFile":"$tmp/age-empty.txt","ageSshKeyPaths":[],"gnupgHome":null,"keepGenerations":1,"logging":{"keyImport":true,"secretChanges":true},
 "placeholderBySecretName":{},"secrets":[{"format":"yaml","key":"canary","mode":"0600","name":"canary","path":"$tmp/canary.txt","sopsFile":"$out/test-secrets.yaml"}],
 "secretsMountPoint":"$tmp/secrets.d","sshKeyPaths":[],"symlinkPath":"$tmp/secrets","templates":[],"userMode":true}
EOF
  if GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES=1 GOOGLE_APPLICATION_CREDENTIALS="$cfg" env -u GOOGLE_CREDENTIALS "$bin" -ignore-passwd "$tmp/manifest.json" && [ -s "$tmp/canary.txt" ]; then
    pass "1d(iii): sops-install-secrets decrypted via WIF -> $(cat "$tmp/canary.txt")"
  else fail "1d(iii) (binary: $bin)"; fi
else
  echo "SKIP 1d: out/test-secrets.yaml missing (admin-setup could not run sops)"
fi
