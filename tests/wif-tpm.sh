#!/usr/bin/env bash
# Cases for the TPM-resident device WIF key (report A1 #1, A6.5, Part A item 2).
#
# Moving the device key into the TPM changes three things that matter and must
# not change a fourth:
#
#   1. The private half stops being a file THAT STILL WORKS. `~/.config/ko/wif.pem`
#      is a P-256 private key in plaintext on disk -- anyone who reads it once can
#      mint device JWTs forever, from anywhere, and the only evidence is a KMS
#      access log entry that looks exactly like shiori. Generating a TPM key does
#      not fix that; only removing the old key's kid from the published JWKS does,
#      because the JWKS is what grants authority. So the last check here computes
#      the old file key's kid and requires it to be ABSENT from the live JWKS. It
#      is meant to stay red through the migration and go green when the old
#      identity is revoked -- the step it would otherwise be easy to call done.
#   2. The new key cannot sign without a working TPM connection. Pointing the TCTI
#      at a device that is not a TPM must make signing fail. Read that check for
#      exactly what it proves: it fails inside provider initialisation, so it
#      shows the TPM key's signing path goes through the chip, NOT that this blob
#      is bound to THIS chip specifically -- a blob wrapped to a different TPM
#      would fail here identically. Proving the latter needs a second TPM.
#   3. The JWK the signer publishes still describes the key that signs. The kid is
#      an RFC 7638 thumbprint over x/y, so a mismatch means the JWKS entry
#      authorises a key nobody holds and STS rejects every token -- a failure that
#      only shows up against Google, minutes later, as a 401. Both halves are
#      recomputed here independently of the signer: the point from openssl, the
#      thumbprint from hashlib.
#   4. The file-key path keeps working untouched. mari is darwin and has no TPM,
#      and the other four hosts are not enrolled yet, so the file path is still
#      the one most of the fleet boots on. It is exercised with no provider
#      environment at all, which is how those hosts run it.
#
# A skip is NOT a pass. On a host that has a TPM and a key, anything that stops
# this file from checking the TPM is a failure of the test, not an exemption:
# `needed` marks those, and the exit status counts them. Only genuinely external
# conditions (no TPM on this host, no network, no nix) stay soft skips.
#
#   ./tests/wif-tpm.sh                        # default key path
#   ./tests/wif-tpm.sh ~/.config/ko/wif-tpm.pem
#
# Run it from anywhere; every nix invocation below runs with the cwd inside the
# repo this file lives in, so `.#` resolves to THIS flake and not to whatever
# flake the caller happened to be standing in.
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
signer="$repo/home/scripts/ko-wif-token.py"
host="$(uname -n)"; host="${host%%.*}"
key="${1:-${KO_WIF_TPM_KEY:-$HOME/.config/ko/wif-tpm.pem}}"
oldkey="${KO_WIF_FILE_KEY:-$HOME/.config/ko/wif.pem}"
# Pinned, not derived. The kid is public, stable and already in this file's own
# failure message, and keying the revocation check on the key FILE made the check
# disappear the moment anyone moved the file -- which is the single most likely
# thing an operator does next. Deleting a private key revokes nothing: authority
# lives in the JWKS, and whoever copied the file first keeps a working identity.
# Override for another host; empty disables the check deliberately.
oldkid_pinned="${KO_WIF_OLD_KID-01HB4BTt8_vvHx6QA2OY2lhRkDsZcO6XaYGIOtd5sZs}"
tcti="${TPM2OPENSSL_TCTI:-device:/dev/tpmrm0}"
issuer="${KO_WIF_ISSUER:-https://storage.googleapis.com/ko-keys-sauyon/hosts}"

fails=0; n=0; skipped=0
ok()   { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad()  { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }
skip() { n=$((n+1)); skipped=$((skipped+1)); printf 'skip %d - %s\n' "$n" "$1"; }
# A condition that should be impossible on a host this file is meant to run on.
# Skipping here would mean reporting "all good" for a run that checked nothing.
needed() { bad "$1"$'\n'"      (${2:-this host has a TPM and a device key}, so this is a broken test run, not a skip)"; }
is()   { if [ "$2" = "$3" ]; then ok "$1"; else bad "$1"$'\n'"      want: $3"$'\n'"      got:  $2"; fi; }

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

# Fetch the JWKS and PROVE it is one before anyone draws a conclusion from it.
# `curl -f` only rejects HTTP >= 400, so a captive portal or an intercepting
# proxy answering 200 with HTML gets through it. That matters asymmetrically: a
# check that the kid is PRESENT fails safe on such a body, but the check that the
# kid is ABSENT would read it as "revoked" and go green with the old identity
# still live -- a green suite on hotel wifi.
fetch_jwks() { # fetch_jwks -> kid list on stdout, non-zero if it is not a JWKS
  local body
  body=$(curl -fsS --max-time 15 "$issuer/.well-known/jwks.json" 2>/dev/null) || return 1
  printf '%s' "$body" | python3 -c 'import json, sys
d = json.load(sys.stdin)
ks = d["keys"]
assert isinstance(ks, list) and ks, "empty or non-list keys"
print("\n".join(str(k.get("kid")) for k in ks if isinstance(k, dict)))' 2>/dev/null
}

# openssl and the tpm2 provider have to come from ONE source: a provider built
# against a different OpenSSL than the binary loading it may refuse to load, and
# the error names neither. Prefer this flake's own pkgs, because that is the pair
# home.nix wires into ko-wif-token; fall back to plain nixpkgs for a host with no
# homeConfiguration. The `cd "$repo"` is load-bearing -- `.#` is resolved against
# the CWD, so without it a run from outside the repo silently takes the registry's
# nixpkgs for one of the two and this flake's for the other.
pick() { # pick <relative path> <flake attr>... -> first out path that contains it
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

# The kid of <key>, recomputed from scratch: RFC 7638 is sha256 over the compact,
# key-sorted JSON of exactly {crv,kty,x,y}. Deriving it here from the PUBLIC POINT
# rather than from the signer's own JWK is what makes it evidence -- reusing
# jwk()["kid"] would only prove the signer agrees with itself.
kid_of_point() { # kid_of_point <64-byte x||y, base64 std> -> kid
  python3 - "$1" <<'PY'
import sys, base64, hashlib, json
pt = base64.b64decode(sys.argv[1])
if len(pt) != 64:
    sys.exit(f"expected a 64-byte public point, got {len(pt)}")
u = lambda b: base64.urlsafe_b64encode(b).rstrip(b"=").decode()
j = {"crv": "P-256", "kty": "EC", "x": u(pt[:32]), "y": u(pt[32:])}
print(u(hashlib.sha256(json.dumps(j, separators=(",", ":"), sort_keys=True).encode()).digest()))
PY
}

point_of() { # point_of <key> [openssl provider args...] -> 64-byte x||y, base64 std
  # SubjectPublicKeyInfo for P-256 is 91 bytes ending in the x||y point.
  "$OSSL" pkey "${@:2}" -in "$1" -pubout -outform DER 2>/dev/null | tail -c 64 | base64 | tr -d '\n'
}

# Sign a JWT with <key>, then check it against the public half the signer itself
# published as a JWK. Silent on success; prints the reason and returns non-zero
# otherwise.
roundtrip() { # roundtrip <key> [openssl provider args...]
  local k="$1" jwt sig msg point jwkpoint jwkjson
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
  point=$(point_of "$k" "${@:2}")
  # Error-check the JWK separately: folded into the comparison below, a signer
  # that died here would be reported as a point mismatch, which is a different bug.
  jwkjson=$(python3 "$signer" --key "$k" --jwk 2>&1) || { echo "--jwk failed: $(printf '%s' "$jwkjson" | tail -1)"; return 1; }
  jwkpoint=$(printf '%s' "$jwkjson" | python3 -c 'import json, sys, base64
j = json.load(sys.stdin)
u = lambda s: base64.urlsafe_b64decode(s + "==")
sys.stdout.write(base64.b64encode(u(j["x"]) + u(j["y"])).decode())') || { echo "unparseable JWK"; return 1; }
  [ "$jwkpoint" = "$point" ] || { echo "the JWK x||y is not this key's public point"; return 1; }
  return 0
}

have_tpm=0; [ -e "${tcti#device:}" ] && have_tpm=1
have_key=0; [ -r "$key" ] && have_key=1

# ── the TPM key ─────────────────────────────────────────────────────────────
if [ "$have_tpm" = 0 ]; then
  skip "TPM key checks: no TPM at ${tcti#device:} (this host has none, or set TPM2OPENSSL_TCTI)"
elif [ "$have_key" = 0 ]; then
  bad "the device key exists and is TPM-resident"$'\n'"      $key is missing or unreadable; generate it with ./install/wif/tpm-keygen.sh"
else
  # A TSS2 PRIVATE KEY is the wrapped-blob format tpm2-openssl writes. A plain
  # "EC PRIVATE KEY" here would mean the key never left the filesystem.
  is "the device key is a TSS2 (TPM-wrapped) private key, not an EC file key" \
     "$(head -1 "$key" | tr -d '\r')" "-----BEGIN TSS2 PRIVATE KEY-----"

  if [ -z "${OPENSSL_MODULES:-}" ]; then
    # Not a skip: this is the very thing check "ko-wif-token exports ..." asserts
    # the wrapper provides, so quietly passing without it would be circular.
    needed "the tpm2 openssl provider is available (nixpkgs#tpm2-openssl)"
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

    # The kid, recomputed from the public point. Without this, nothing in the
    # suite would notice jwk() hashing the wrong field set or dropping
    # sort_keys -- the JWT header and --jwk both come from that one function, so
    # they agree with each other however wrong they are, and the only other
    # witness (the live JWKS) is network-gated.
    want_kid=$(printf '%s' "$j" | python3 -c 'import json,sys; print(json.load(sys.stdin)["kid"])' 2>/dev/null)
    got_kid=$(kid_of_point "$(point_of "$key" -provider tpm2 -provider default)" 2>/dev/null)
    if [ -n "$got_kid" ]; then
      is "the kid is the RFC 7638 thumbprint of {crv,kty,x,y}" "$want_kid" "$got_kid"
    else
      needed "the kid is the RFC 7638 thumbprint of {crv,kty,x,y} (could not recompute it)"
    fi

    # See rationale 2 at the top for what this does and does not prove. The
    # failure text is asserted because "any non-zero exit" would also be
    # satisfied by a typo in $signer or an unreadable key -- a pass for the
    # wrong reason, in the one check whose whole job is to be a negative.
    if out=$(TPM2OPENSSL_TCTI="device:/dev/null" python3 "$signer" --key "$key" \
               --iss "$issuer" --sub "device:$host" --aud test-aud --skip-clock-check 2>&1); then
      bad "the TPM key's signing path goes through the TPM"$'\n'"      it signed with TPM2OPENSSL_TCTI=device:/dev/null"
    elif printf '%s' "$out" | grep -qiE 'provider|tcti'; then
      ok "the TPM key's signing path goes through the TPM (a dead TCTI refuses it)"
    else
      bad "the TPM key's signing path goes through the TPM"$'\n'"      it failed, but for an unrelated reason:"$'\n'"$(printf '%s' "$out" | tail -2 | sed 's/^/      /')"
    fi

    # `.sops.yaml` has an equivalent check for the paper key; this is the same
    # idea for the device tier. A key configured but not published is a host that
    # boots to an STS 401, and nothing local can tell you that.
    kid=$(printf '%s' "$j" | python3 -c 'import json, sys; print(json.load(sys.stdin)["kid"])' 2>/dev/null)
    if [ -z "$kid" ]; then
      needed "JWKS membership: no kid to look for (the JWK check above failed)"
    elif ! jwks=$(fetch_jwks); then
      skip "JWKS membership: $issuer is unreachable or did not answer with a JWKS"
    elif printf '%s\n' "$jwks" | grep -qxF "$kid"; then
      ok "the TPM key's kid is published in the live JWKS"
    else
      bad "the TPM key's kid is published in the live JWKS"$'\n'"      $kid is not in $issuer/.well-known/jwks.json"$'\n'"      publish it from the admin host (install/wif/admin-setup.sh)"
    fi
  fi
fi

# ── the old file key must no longer be authorised ───────────────────────────
# Rationale 1. Generating a TPM key removes nothing by itself: authority lives in
# the JWKS, so while the old kid is published the old key still decrypts every
# dotfiles secret and this migration is not finished. Keyed on the KID, not on the
# key file: the file being gone proves nothing, because a copy taken before it was
# deleted works exactly as well, and because that is precisely the moment an
# operator would most like the check to keep asserting.
oldkid="$oldkid_pinned"
# The file is only a fallback source for the kid, never a precondition.
if [ -z "$oldkid" ] && [ -r "$oldkey" ]; then
  oldkid=$(python3 "$signer" --key "$oldkey" --jwk 2>/dev/null \
           | python3 -c 'import json,sys; print(json.load(sys.stdin)["kid"])' 2>/dev/null)
fi
if [ -z "$oldkid" ]; then
  skip "the old file key is no longer authorised: no kid pinned (KO_WIF_OLD_KID) and $oldkey unreadable"
elif ! jwks=$(fetch_jwks); then
  # Deliberately a skip and not an ok: "I could not read the JWKS" is not
  # "the key is revoked", and this is the check where confusing the two is how
  # the migration gets called done while the old identity still works.
  skip "the old file key is no longer authorised: $issuer is unreachable or did not answer with a JWKS"
else
  if printf '%s\n' "$jwks" | grep -qxF "$oldkid"; then
    bad "the old file key is no longer authorised"$'\n'"      kid $oldkid is STILL in the live JWKS, so whoever holds that key -- the plaintext"$'\n'"      file at $oldkey, or any copy made of it -- still mints device tokens."$'\n'"      The TPM key is an addition, not yet a migration. Once the new generation is live:"$'\n'"        ./install/wif/revoke-kid.sh $oldkid $oldkey"
  else
    ok "the old file key's kid is no longer in the live JWKS"
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
  case "${cfg:-}" in
    "")
      skip "home.nix wiring: $host has no homeConfiguration in this flake" ;;
    /nix/store/*)
      : ;;
    *)
      # A non-store path means sops.environment took the gcp-key.json branch, i.e.
      # this host is not in wifHosts. On a host holding a TSS2 device key that is
      # the regression these checks exist for, not a reason to skip them.
      if [ "$have_key" = 1 ]; then
        needed "home.nix wiring: $host has a TPM device key but its credential config is $cfg (not in wifHosts?)" \
               "this host holds a TSS2 device key"
      else
        skip "home.nix wiring: $host is not a WIF host (credential config is $cfg)"
      fi
      cfg="" ;;
  esac
  if [ -n "${cfg:-}" ] && [ ! -e "$cfg" ]; then
    needed "home.nix wiring: could not realise $cfg" "$host has a WIF credential config"
    cfg=""
  fi
  if [ -n "${cfg:-}" ]; then
    if grep -qF -- "--key $key " "$cfg"; then
      ok "$host's credential config signs with $key"
    else
      bad "$host's credential config signs with $key"$'\n'"      it names: $(grep -o -- '--key [^ \"]*' "$cfg" | head -1)"
    fi
    wrapper=$(grep -o '/nix/store/[^ "]*ko-wif-token[^ "]*/bin/ko-wif-token' "$cfg" | head -1)
    moddir=$(grep -o 'OPENSSL_MODULES=[^ ]*' "${wrapper:-/dev/null}" 2>/dev/null | head -1)
    moddir=${moddir#OPENSSL_MODULES=}
    moddir=${moddir%\"}; moddir=${moddir#\"}   # bare today; do not depend on that
    # Grepping the wrapper for the two names is not enough: the exports could
    # point at a derivation with no provider in it and every token would still
    # fail at runtime. Check the module is actually there.
    if [ -n "$wrapper" ] && [ -e "$wrapper" ] && grep -q 'TPM2OPENSSL_TCTI=' "$wrapper" \
       && [ -n "$moddir" ] && [ -e "$moddir/tpm2.so" ]; then
      ok "ko-wif-token exports TPM2OPENSSL_TCTI and an OPENSSL_MODULES dir holding tpm2.so"
    else
      bad "ko-wif-token exports TPM2OPENSSL_TCTI and an OPENSSL_MODULES dir holding tpm2.so"$'\n'"      wrapper: ${wrapper:-none referenced by $cfg}"$'\n'"      OPENSSL_MODULES: ${moddir:-unset}${moddir:+ (tpm2.so present: $([ -e "$moddir/tpm2.so" ] && echo yes || echo NO))}"
    fi
  fi
fi

echo
note=""
[ "$skipped" -gt 0 ] && note=", $skipped skipped"
if [ "$fails" = 0 ]; then
  echo "$n checks, all good$note"
else
  echo "$n checks, $fails failed$note"
fi
[ "$fails" = 0 ]
