#!/usr/bin/env bash
# Cases for the TPM-resident device WIF key (report A1 #1, A6.5, Part A item 2).
#
# Moving the device key into the TPM changes one thing that matters and must not
# change another:
#
#   1. The private half stops being a file. `~/.config/ko/wif.pem` is a P-256
#      private key in plaintext on disk -- anyone who reads it once can mint
#      device JWTs forever, from anywhere, and the only evidence is a KMS access
#      log entry that looks exactly like shiori. The TPM key is a wrapped blob:
#      the file is useless without this machine's TPM. One check proves that
#      rather than asserting it, by pointing the TCTI at a device that is not a
#      TPM and requiring the signature to FAIL. A key that still signs there was
#      never in the TPM.
#   2. The JWK the signer publishes still describes the key that signs. The kid
#      is an RFC 7638 thumbprint over x/y, so a mismatch means the JWKS entry
#      authorises a key nobody holds and STS rejects every token -- a failure
#      that only shows up against Google, minutes later, as a 401.
#   3. The file-key path keeps working untouched. mari is darwin and has no TPM,
#      and the other four hosts are not enrolled yet, so the file path is still
#      the one most of the fleet boots on. It is exercised here with no provider
#      environment at all, which is how those hosts run it.
#
#   ./tests/wif-tpm.sh                        # default key path
#   ./tests/wif-tpm.sh ~/.config/ko/wif-tpm.pem
#
# Run it from anywhere; paths are resolved against the repo this file lives in.
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
signer="$repo/home/scripts/ko-wif-token.py"
host="$(uname -n)"; host="${host%%.*}"
key="${1:-${KO_WIF_TPM_KEY:-$HOME/.config/ko/wif-tpm.pem}}"
tcti="${TPM2OPENSSL_TCTI:-device:/dev/tpmrm0}"
issuer="https://storage.googleapis.com/ko-keys-sauyon/hosts"

fails=0; n=0
ok()   { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad()  { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }
skip() { n=$((n+1)); printf 'skip %d - %s\n' "$n" "$1"; }
is()   { if [ "$2" = "$3" ]; then ok "$1"; else bad "$1"$'\n'"      want: $3"$'\n'"      got:  $2"; fi; }

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

# openssl and the tpm2 provider have to come from ONE source: a provider built
# against a different OpenSSL than the binary loading it either refuses to load
# or -- worse -- loads and misbehaves. Prefer this flake's own pkgs, because that
# is the pair home.nix wires into ko-wif-token; fall back to plain nixpkgs for a
# host with no homeConfiguration (or a checkout that cannot evaluate).
pick() { # pick <relative path> <flake attr>... -> first out path that contains it
  local p attr
  for attr in "${@:2}"; do
    for p in $(nix build --no-link --print-out-paths "$attr" 2>/dev/null); do
      [ -e "$p/$1" ] && { printf '%s\n' "$p"; return 0; }
    done
  done
  return 1
}
ossl_root=$(pick bin/openssl ".#homeConfigurations.$host.pkgs.openssl" "nixpkgs#openssl")
prov_root=$(pick lib/ossl-modules/tpm2.so ".#homeConfigurations.$host.pkgs.tpm2-openssl" "nixpkgs#tpm2-openssl")
OSSL="${ossl_root:+$ossl_root/bin/openssl}"
[ -n "$OSSL" ] || OSSL=$(command -v openssl)
[ -n "$OSSL" ] || { echo "need openssl (nixpkgs#openssl)" >&2; exit 1; }

# The signer resolves openssl from KO_OPENSSL and the tpm2 provider from the
# ambient OPENSSL_MODULES, exactly as the home.nix wrapper sets them.
export KO_OPENSSL="$OSSL"
[ -n "$prov_root" ] && export OPENSSL_MODULES="$prov_root/lib/ossl-modules"
export TPM2OPENSSL_TCTI="$tcti"

# JWS ES256 carries the signature as raw r||s; openssl verifies DER. Converting
# it here rather than reusing the signer's own der_sig_to_raw is the point: a bug
# shared by signer and test would verify against itself and prove nothing.
raw2der() { # raw2der <base64url sig> -> DER on stdout
  python3 - "$1" <<'PY'
import sys, base64
s = sys.argv[1]
raw = base64.urlsafe_b64decode(s + "=" * (-len(s) % 4))
if len(raw) != 64:
    sys.exit(f"expected a 64-byte raw ECDSA signature, got {len(raw)}")
def i(b):
    b = b.lstrip(b"\0") or b"\0"
    if b[0] & 0x80:
        b = b"\0" + b
    return b"\x02" + bytes([len(b)]) + b
body = i(raw[:32]) + i(raw[32:])
sys.stdout.buffer.write(b"\x30" + bytes([len(body)]) + body)
PY
}

# Sign a JWT with <key>, then check it against the public half the signer itself
# published as a JWK. Silent on success; prints the reason and returns non-zero
# otherwise.
roundtrip() { # roundtrip <key> [openssl provider args...]
  local k="$1" jwt sig msg point jwkpoint
  jwt=$(python3 "$signer" --key "$k" --iss "$issuer" --sub "device:$host" \
          --aud test-aud --skip-clock-check 2>&1) || { echo "sign failed: $jwt"; return 1; }
  msg=${jwt%.*}; sig=${jwt##*.}
  [ "$msg" != "$jwt" ] || { echo "not a compact JWS: $jwt"; return 1; }
  raw2der "$sig" > "$work/sig.der" || return 1
  "$OSSL" pkey "${@:2}" -in "$k" -pubout -out "$work/pub.pem" 2>"$work/e" \
    || { echo "could not extract the public key: $(tail -1 "$work/e")"; return 1; }
  printf '%s' "$msg" > "$work/msg"
  "$OSSL" dgst -sha256 -verify "$work/pub.pem" -signature "$work/sig.der" "$work/msg" \
    >/dev/null 2>&1 || { echo "the signature did not verify against the key's own public half"; return 1; }
  # SubjectPublicKeyInfo for P-256 is 91 bytes ending in the 64-byte x||y point.
  point=$("$OSSL" pkey "${@:2}" -in "$k" -pubout -outform DER 2>/dev/null | tail -c 64 | base64 | tr -d '\n')
  jwkpoint=$(python3 "$signer" --key "$k" --jwk | python3 -c 'import json, sys, base64
j = json.load(sys.stdin)
u = lambda s: base64.urlsafe_b64decode(s + "==")
sys.stdout.write(base64.b64encode(u(j["x"]) + u(j["y"])).decode())')
  [ "$jwkpoint" = "$point" ] || { echo "the JWK x||y is not this key's public point"; return 1; }
  return 0
}

# ── the TPM key ─────────────────────────────────────────────────────────────
if [ ! -e "${tcti#device:}" ]; then
  skip "TPM key checks: no TPM at ${tcti#device:} (this host has none, or set TPM2OPENSSL_TCTI)"
elif [ ! -r "$key" ]; then
  bad "the device key exists and is TPM-resident"$'\n'"      $key is missing or unreadable; generate it with ./install/wif/tpm-keygen.sh"
else
  # A TSS2 PRIVATE KEY is the wrapped-blob format tpm2-openssl writes. A plain
  # "EC PRIVATE KEY" here would mean the key never left the filesystem.
  is "the device key is a TSS2 (TPM-wrapped) private key, not an EC file key" \
     "$(head -1 "$key")" "-----BEGIN TSS2 PRIVATE KEY-----"

  if [ -z "${OPENSSL_MODULES:-}" ]; then
    skip "TPM JWK/signature checks: no tpm2 provider available (nixpkgs#tpm2-openssl)"
  else
    j=$(python3 "$signer" --key "$key" --jwk 2>&1)
    if printf '%s' "$j" | python3 -c 'import json, sys
j = json.load(sys.stdin)
assert j["kty"] == "EC" and j["crv"] == "P-256" and j["alg"] == "ES256", j
assert len(j["kid"]) == 43 and len(j["x"]) == 43 and len(j["y"]) == 43, j' 2>/dev/null; then
      ok "ko-wif-token prints a well-formed P-256 JWK for the TPM key"
    else
      bad "ko-wif-token prints a well-formed P-256 JWK for the TPM key"$'\n'"$(printf '%s' "$j" | tail -3 | sed 's/^/      /')"
    fi

    if err=$(roundtrip "$key" -provider tpm2 -provider default); then
      ok "a JWT signed by the TPM key verifies against the JWK ko-wif-token publishes"
    else
      bad "a JWT signed by the TPM key verifies against the JWK ko-wif-token publishes"$'\n'"      $err"
    fi

    # The claim the whole item rests on. /dev/null is a character device that is
    # not a TPM, so the TCTI layer opens it and the command reaches nothing --
    # while a key whose private half were still in the file would not care.
    if TPM2OPENSSL_TCTI="device:/dev/null" python3 "$signer" --key "$key" \
         --iss "$issuer" --sub "device:$host" --aud test-aud --skip-clock-check \
         >/dev/null 2>&1; then
      bad "the TPM key cannot sign without the TPM"$'\n'"      it signed with TPM2OPENSSL_TCTI=device:/dev/null: the private half is not TPM-bound"
    else
      ok "the TPM key cannot sign without the TPM (TCTI=device:/dev/null is refused)"
    fi

    # `.sops.yaml` has an equivalent check for the paper key; this is the same
    # idea for the device tier. A key configured but not published is a host that
    # boots to an STS 401, and nothing local can tell you that.
    kid=$(printf '%s' "$j" | python3 -c 'import json, sys; print(json.load(sys.stdin)["kid"])' 2>/dev/null)
    if [ -z "$kid" ]; then
      skip "JWKS membership: no kid to look for (the JWK check above failed)"
    elif ! jwks=$(curl -fsS --max-time 15 "$issuer/.well-known/jwks.json" 2>/dev/null); then
      skip "JWKS membership: $issuer is unreachable"
    elif printf '%s' "$jwks" | grep -qF "$kid"; then
      ok "the TPM key's kid is published in the live JWKS"
    else
      bad "the TPM key's kid is published in the live JWKS"$'\n'"      $kid is not in $issuer/.well-known/jwks.json"$'\n'"      publish it from the admin host (install/wif/admin-setup.sh)"
    fi
  fi
fi

# ── the file-key path, which mari and the unenrolled hosts still use ────────
# No OPENSSL_MODULES, no TCTI, no provider arguments: the darwin environment,
# reproduced. If this ever needs the TPM environment to pass, the signer has
# grown a dependency the rest of the fleet cannot satisfy.
unset OPENSSL_MODULES TPM2OPENSSL_TCTI
filekey="$work/file.pem"
if (umask 077; "$OSSL" ecparam -name prime256v1 -genkey -noout -out "$filekey" 2>/dev/null); then
  if err=$(roundtrip "$filekey"); then
    ok "the plain file-key path still signs and verifies with no TPM environment"
  else
    bad "the plain file-key path still signs and verifies with no TPM environment"$'\n'"      $err"
  fi
else
  bad "could not generate a scratch P-256 file key with $OSSL"
fi

# ── home.nix wires this host to the TPM key ─────────────────────────────────
# The evaluated credential config, not the source text: this is the JSON Google's
# auth library actually reads, and the argv inside it is what runs under PATH="".
if ! command -v nix >/dev/null 2>&1; then
  skip "home.nix wiring: no nix on PATH"
else
  cfg=$(cd "$repo" && nix eval --raw ".#homeConfigurations.$host.config.sops.environment.GOOGLE_APPLICATION_CREDENTIALS" 2>/dev/null)
  # `nix eval` prints the path without realising it; build the closure that
  # contains it. That is the derivation CI builds, so it is normally warm.
  [ -n "$cfg" ] && [ ! -e "$cfg" ] && \
    (cd "$repo" && nix build --no-link ".#homeConfigurations.$host.activationPackage" 2>/dev/null)
  if [ -z "$cfg" ] || [ ! -e "$cfg" ]; then
    skip "home.nix wiring: no realisable credential config for $host (unenrolled host?)"
  else
    if grep -qF -- "--key $key " "$cfg"; then
      ok "$host's credential config signs with $key"
    else
      bad "$host's credential config signs with $key"$'\n'"      it names: $(grep -o -- '--key [^ \"]*' "$cfg" | head -1)"
    fi
    wrapper=$(grep -o '/nix/store/[^ "]*ko-wif-token[^ "]*/bin/ko-wif-token' "$cfg" | head -1)
    if [ -n "$wrapper" ] && [ -e "$wrapper" ] \
       && grep -q 'OPENSSL_MODULES=' "$wrapper" && grep -q 'TPM2OPENSSL_TCTI=' "$wrapper"; then
      ok "ko-wif-token exports OPENSSL_MODULES and TPM2OPENSSL_TCTI on $host"
    else
      bad "ko-wif-token exports OPENSSL_MODULES and TPM2OPENSSL_TCTI on $host"$'\n'"      wrapper: ${wrapper:-none referenced by $cfg}"
    fi
  fi
fi

echo
if [ "$fails" = 0 ]; then echo "$n checks, all good"; else echo "$n checks, $fails failed"; fi
[ "$fails" = 0 ]
