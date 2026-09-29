#!/usr/bin/env bash
# Experiment: does presenting an UNKNOWN kid make Google re-fetch the issuer's
# JWKS, and thereby make an already-published removal take effect?
#
#   ./install/wif/jwks-refresh-probe.sh [trials] [delay-seconds]
#
# WHY THIS EXISTS
#
# Removing a kid from the published JWKS is the only per-device revocation this
# design has (report A3: `sub` is not bound to the signing key, experiment 2b, so
# every device in a tier is one KMS principal and nothing downstream of the kid
# can tell devices apart). It was measured not to work on any useful timescale:
# a removed kid was still accepted 38 min after the write, and the SAME kid was
# still accepted 21.7 h after it.
#
# task-2's handoff offered a mechanism for that: a negative cache. If Google
# re-fetches the key set when it meets a kid it does not know, but serves a
# cached set when it meets one it does, then a removal is invisible precisely
# because the removed key is still in the cached set and never triggers a
# refresh. The handoff's own reading of this was pessimistic -- "which if right
# means nothing we do makes a removal land sooner" -- but that is the opposite
# conclusion from the same premise, and it was never tested. If an unknown kid
# forces a refresh, then presenting one is a CLIENT-SIDE lever: a revocation
# accelerator that needs no GCP credential, no admin host and no IAM change.
#
# It costs three token exchanges per trial to find out. That is the entire
# experiment.
#
# THE PROTOCOL, per trial
#
#   1. R (removed kid)   -> baseline. Must be `accepted`, or there is nothing to
#                           accelerate and the trial is void.
#   2. U (unknown kid)   -> the trigger. Must be `rejected`; that is Google
#                           failing to resolve a kid, which is the event the
#                           hypothesis says forces a re-fetch.
#   3. sleep <delay>
#   4. R again           -> THE READING. `accepted` = no refresh happened.
#                           `rejected` = the removal landed, and step 2 is why.
#   5. P (published kid) -> the control. Must be `accepted`. Without it, a
#                           `rejected` at step 4 is indistinguishable from the
#                           exchange having broken for an unrelated reason --
#                           a dropped network, a disabled pool, an expired
#                           provider. This is the check that makes step 4 mean
#                           anything, so a trial whose control fails is void.
#
# Every response goes through install/wif/sts-classify.py, which is deliberately
# strict about what counts as a refusal: only 401, or 400 with
# error=invalid_grant. A wrong audience is also a 400, and counting it as a
# refusal would report a config typo as a measured revocation. Anything else is
# `void` -- a failed trial, discarded, not evidence in either direction.
#
# This writes nothing, changes nothing, and needs no Google credential. It only
# mints self-signed JWTs from keys this host already holds and posts them to the
# public STS endpoint.
set -uo pipefail

trials="${1:-5}"
delay="${2:-5}"

here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
signer="$repo/home/scripts/ko-wif-token.py"
classify="$here/sts-classify.py"
host="$(uname -n)"; host="${host%%.*}"

issuer="${KO_WIF_ISSUER:-https://storage.googleapis.com/ko-keys-sauyon/hosts}"
audience="${KO_WIF_AUDIENCE:-//iam.googleapis.com/projects/484956590837/locations/global/workloadIdentityPools/ko-hosts/providers/bucket}"
subject="${KO_WIF_SUB:-device:$host}"
removed_key="${KO_WIF_FILE_KEY:-$HOME/.config/ko/wif.pem}"   # R: kid removed from the JWKS
published_key="${KO_WIF_TPM_KEY:-$HOME/.config/ko/wif-tpm.pem}" # P: kid still published

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

# Same single-source rule as tests/wif-tpm.sh: openssl and the tpm2 provider must
# come from one build, or the provider refuses to load and says why in neither.
pick() {
  local p attr
  for attr in "${@:2}"; do
    for p in $(cd "$repo" && nix build --no-link --print-out-paths "$attr" 2>/dev/null); do
      [ -e "$p/$1" ] && { printf '%s\n' "$p"; return 0; }
    done
  done
  return 1
}
ossl_root=$(pick bin/openssl ".#homeConfigurations.$host.pkgs.openssl" "nixpkgs#openssl")
prov_root=$(pick lib/ossl-modules/tpm2.so ".#homeConfigurations.$host.pkgs.tpm2-openssl" "nixpkgs#tpm2-openssl")
[ -n "$ossl_root" ] && export KO_OPENSSL="$ossl_root/bin/openssl"
[ -n "$prov_root" ] && export OPENSSL_MODULES="$prov_root/lib/ossl-modules"
export TPM2OPENSSL_TCTI="${TPM2OPENSSL_TCTI:-device:/dev/tpmrm0}"

# U: a key that has never been published anywhere. Generated fresh per run so it
# cannot accidentally be one Google has ever seen.
unknown_key="$work/unknown.pem"
(umask 077; "${KO_OPENSSL:-openssl}" ecparam -name prime256v1 -genkey -noout -out "$unknown_key") 2>/dev/null \
  || { echo "could not generate the unknown-kid key" >&2; exit 1; }

kid_of() { python3 "$signer" --key "$1" --jwk 2>/dev/null \
  | python3 -c 'import json,sys; print(json.load(sys.stdin)["kid"])' 2>/dev/null; }

exchange() { # exchange <key> -> prints the verdict line from sts-classify
  local tok code
  # stderr to a file, never into the token: the TSS stack writes WARNING: lines
  # at this layer and one folded in makes a two-line "JWT" that STS 400s, which
  # would read as a revocation.
  tok=$(python3 "$signer" --key "$1" --iss "$issuer" --sub "$subject" --aud "$audience" 2>"$work/signerr") \
    || { echo "void could not sign with $1: $(tail -1 "$work/signerr" 2>/dev/null)"; return 0; }
  (umask 077; printf '%s' "$tok" > "$work/tok.jwt")
  : > "$work/sts.json"
  code=$(curl -sS --max-time 20 -o "$work/sts.json" -w '%{http_code}' https://sts.googleapis.com/v1/token \
    --data-urlencode grant_type=urn:ietf:params:oauth:grant-type:token-exchange \
    --data-urlencode "audience=$audience" \
    --data-urlencode scope=https://www.googleapis.com/auth/cloud-platform \
    --data-urlencode requested_token_type=urn:ietf:params:oauth:token-type:access_token \
    --data-urlencode subject_token_type=urn:ietf:params:oauth:token-type:jwt \
    --data-urlencode "subject_token@$work/tok.jwt" 2>"$work/curlerr") || code=000
  # curl's stderr is kept, not discarded: every transport failure lands here as a
  # `void` trial, and a void trial you cannot diagnose is the one outcome that
  # makes the whole run unusable -- you cannot tell a flaky link from a pool that
  # has been disabled underneath you, and both read as "nothing measured".
  if [ "$code" = 000 ]; then
    printf 'void transport: %s\n' "$(tr -d '\r' < "$work/curlerr" | tail -1)"
    return 0
  fi
  python3 "$classify" "$code" "$work/sts.json"
}

verdict() { printf '%s' "${1%% *}"; }

# --- provenance, so the output can be read months later ------------------------
gen=$(curl -fsSI "$issuer/.well-known/jwks.json" 2>/dev/null \
  | tr -d '\r' | sed -n 's/^x-goog-generation: //p')
echo "issuer:     $issuer"
echo "audience:   $audience"
echo "subject:    $subject"
if [ -n "$gen" ]; then
  # The CURRENT object's generation, which is the most recent write of any kind.
  # It is NOT when R's kid was removed unless that was the last write, and on a
  # bucket where kids come and go it usually is not -- GCS keeps the earlier
  # generations but the URL only serves the latest. Reading this number as R's
  # removal time understates the elapsed interval, which is the one direction
  # that would make the revocation look better than it is.
  python3 - "$gen" <<'PY'
import sys, time
g = int(sys.argv[1]) / 1e6
print("latest jwks write (ANY change, not necessarily R's removal): %s (t+%.1f h)"
      % (time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(g)), (time.time()-g)/3600))
PY
  echo "            -> for R's own elapsed time, use the generation of the write that removed it"
fi
echo "R (removed):   $removed_key  kid=$(kid_of "$removed_key")"
echo "P (published): $published_key  kid=$(kid_of "$published_key")"
echo "U (unknown):   generated this run  kid=$(kid_of "$unknown_key")"
echo "trials=$trials delay=${delay}s"
echo

valid=0; flipped=0; void=0
for i in $(seq 1 "$trials"); do
  before=$(exchange "$removed_key")
  trigger=$(exchange "$unknown_key")
  sleep "$delay"
  after=$(exchange "$removed_key")
  control=$(exchange "$published_key")

  printf 'trial %d: before=%s trigger=%s after=%s control=%s\n' \
    "$i" "$(verdict "$before")" "$(verdict "$trigger")" "$(verdict "$after")" "$(verdict "$control")"

  # A trial only measures the hypothesis if its preconditions held: the removed
  # key worked at the start, the unknown key was actually refused (that IS the
  # trigger), and the exchange still worked at the end.
  if [ "$(verdict "$before")" != accepted ]; then
    echo "         VOID: the removed key was not accepted at baseline -- $before"; void=$((void+1)); continue
  fi
  if [ "$(verdict "$trigger")" != rejected ]; then
    echo "         VOID: the unknown kid was not refused, so no refresh was triggered -- $trigger"; void=$((void+1)); continue
  fi
  if [ "$(verdict "$control")" != accepted ]; then
    echo "         VOID: the published control key stopped working -- $control"; void=$((void+1)); continue
  fi
  valid=$((valid+1))
  [ "$(verdict "$after")" = rejected ] && { flipped=$((flipped+1)); echo "         *** the removed key was REFUSED after the trigger: $after"; }
done

echo
echo "valid trials: $valid   void: $void   removed-key flipped to rejected: $flipped"
if [ "$valid" = 0 ]; then
  echo "RESULT: nothing measured -- every trial was void. This is not evidence either way."
  exit 2
fi
if [ "$flipped" = 0 ]; then
  echo "RESULT: the forced-refresh hypothesis is NOT supported. Presenting an unknown"
  echo "        kid did not make the published removal take effect, in $valid valid trials."
  echo "        There is no client-side revocation accelerator here; the removal's"
  echo "        latency is Google's alone."
  exit 0
fi
echo "RESULT: the removed key flipped to rejected in $flipped of $valid valid trials"
echo "        after an unknown kid was presented. That is a client-side lever worth"
echo "        re-running with a control for elapsed time (it may simply have converged)."
exit 0
