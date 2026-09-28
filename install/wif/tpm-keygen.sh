#!/usr/bin/env bash
# Device side. Generates a TPM-RESIDENT P-256 device WIF key (report A1 #1, A6.5,
# Part A item 2) and prints its public JWK for the admin machine.
#
# The file this writes is not a private key: it is a TPM key blob, wrapped to a
# primary under this TPM's owner hierarchy. Copy it to another machine and it
# signs nothing. That is the entire upgrade over device-keygen.sh, whose
# ~/.config/ko/wif.pem is a plain P-256 private key that anyone who reads it once
# can use forever, from anywhere, indistinguishably from this host.
#
#   ./install/wif/tpm-keygen.sh [path]     # default: ~/.config/ko/wif-tpm.pem
#
# What it does NOT survive:
#   * a TPM clear (firmware setup, some BIOS updates, `tpm2_clear`) -- the owner
#     seed changes and the blob becomes undecryptable. That is a re-enrolment,
#     not a recovery: run this again and publish the new JWK.
#   * a motherboard swap, for the same reason.
# Both are fine as long as another recipient of secrets.yaml still works, which
# is what the paper key and (until the item 4 gate) nix-key are for.
#
# No PCR policy and no auth value: sops-nix decrypts unattended at activation and
# at boot, so anything that needs a passphrase or a matching boot measurement
# turns a kernel update into a host that cannot read its own secrets.
set -euo pipefail

out="${1:-${XDG_CONFIG_HOME:-$HOME/.config}/ko/wif-tpm.pem}"
here="$(cd "$(dirname "$0")" && pwd)"
repo="$(cd "$here/../.." && pwd)"
host="$(uname -n)"; host="${host%%.*}"
tcti="${TPM2OPENSSL_TCTI:-device:/dev/tpmrm0}"

# The repo is public, and a TPM blob is still a credential in the sense that
# matters here: committing one advertises the host's enrolment.
case "$(realpath -m "$out")" in
  "$repo"/*) echo "refusing to write the device key inside the repo: $out" >&2; exit 1 ;;
esac
if [ -e "$out" ]; then
  # Overwriting is how a host silently stops matching its published JWK: the old
  # kid stays in the JWKS, the new key signs, and STS returns 401 with nothing
  # local to explain it.
  echo "exists, refusing to overwrite: $out" >&2
  echo "to rotate, move it aside, run this again, and republish the JWKS with the new JWK" >&2
  exit 1
fi

dev="${tcti#device:}"
if [ ! -e "$dev" ]; then
  echo "no TPM at $dev (set TPM2OPENSSL_TCTI to point elsewhere)" >&2; exit 1
fi
if [ ! -w "$dev" ]; then
  echo "$dev is not writable by $(id -un); on Arch that means joining the 'tss' group" >&2; exit 1
fi

# openssl and the tpm2 provider must come from ONE source: a provider built
# against a different OpenSSL than the binary loading it fails to load, and the
# error names neither. This flake's pkgs first, because that is the pair home.nix
# wires into ko-wif-token, so a key made here is made by the tools that will use
# it; nixpkgs as the fallback for a checkout that cannot evaluate.
pick() { # pick <relative path> <flake attr>... -> first out path that contains it
  local p attr
  for attr in "${@:2}"; do
    for p in $(nix build --no-link --print-out-paths "$attr" 2>/dev/null); do
      [ -e "$p/$1" ] && { printf '%s\n' "$p"; return 0; }
    done
  done
  return 1
}
ossl_root=$(pick bin/openssl ".#homeConfigurations.$host.pkgs.openssl" "nixpkgs#openssl") \
  || { echo "need openssl (nixpkgs#openssl)" >&2; exit 1; }
prov_root=$(pick lib/ossl-modules/tpm2.so ".#homeConfigurations.$host.pkgs.tpm2-openssl" "nixpkgs#tpm2-openssl") \
  || { echo "need the tpm2 openssl provider (nixpkgs#tpm2-openssl)" >&2; exit 1; }
OSSL="$ossl_root/bin/openssl"
export OPENSSL_MODULES="$prov_root/lib/ossl-modules"
export TPM2OPENSSL_TCTI="$tcti"

dir="$(dirname "$out")"
mkdir -p "$dir"; chmod 700 "$dir"
umask 077
# Beside $out so the final mv stays on one filesystem: the key appears whole or
# not at all, and a half-written blob is indistinguishable from a wrong one.
tmpd="$(mktemp -d "$dir/.tpm-keygen.XXXXXX")"
trap 'rm -rf "$tmpd"' EXIT

if ! err="$("$OSSL" genpkey -provider tpm2 -provider default \
              -algorithm EC -pkeyopt group:P-256 -out "$tmpd/key" 2>&1)"; then
  printf '%s\n' "openssl genpkey (tpm2) failed: $err" >&2; exit 1
fi
head -1 "$tmpd/key" | grep -qx -- '-----BEGIN TSS2 PRIVATE KEY-----' || {
  echo "openssl wrote $(head -1 "$tmpd/key") -- that is not a TPM key blob; refusing it" >&2
  exit 1
}

# Prove the key signs through the TPM before installing it. A blob the chip will
# not load is worth finding now, not at the next boot in sops-install-secrets.
printf 'tpm-keygen self test' > "$tmpd/msg"
"$OSSL" dgst -sha256 -sign "$tmpd/key" -provider tpm2 -provider default \
  -out "$tmpd/sig" "$tmpd/msg" 2>/dev/null \
  && "$OSSL" pkey -provider tpm2 -provider default -in "$tmpd/key" -pubout -out "$tmpd/pub.pem" 2>/dev/null \
  && "$OSSL" dgst -sha256 -verify "$tmpd/pub.pem" -signature "$tmpd/sig" "$tmpd/msg" >/dev/null 2>&1 \
  || { echo "the new key could not sign-and-verify through the TPM; not installing it" >&2; exit 1; }

chmod 400 "$tmpd/key"
mv "$tmpd/key" "$out"

mkdir -p "$here/out"
jwk="$here/out/$host-tpm.jwk.json"
KO_OPENSSL="$OSSL" python3 "$repo/home/scripts/ko-wif-token.py" --key "$out" --jwk | tee "$jwk"

echo
echo "wrote $out (mode 400, TPM-wrapped) and $jwk"
echo
echo "Next:"
echo "  1. carry ONLY $jwk to the admin machine (it is public)"
echo "  2. republish the JWKS with EVERY current key, this one included --"
echo "     admin-setup.sh replaces the JWKS with exactly the JWKs you pass it"
echo "  3. add $host to wifTpmHosts in home.nix, then build and switch (hms)"
echo "  4. ./tests/wif-tpm.sh $out"
echo
echo "Keep the old key published until the new generation is live: removing its"
echo "kid revokes the identity the RUNNING generation still signs with."
