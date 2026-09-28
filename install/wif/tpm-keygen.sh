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
# Both sides through realpath: $repo comes from a logical cd+pwd, so with the
# repo reached via a symlinked prefix (~/dotfiles -> ~/devel/dotfiles) the two
# spellings never share a prefix and the guard silently passes.
real_out="$(realpath -m "$out")"; real_repo="$(realpath -m "$repo")"
case "$real_out" in
  "$real_repo"/*) echo "refusing to write the device key inside the repo: $out" >&2; exit 1 ;;
esac
if [ -e "$out" ]; then
  # Overwriting is how a host silently stops matching its published JWK: the old
  # kid stays in the JWKS, the new key signs, and STS returns 401 with nothing
  # local to explain it.
  echo "exists, refusing to overwrite: $out" >&2
  # The common reason for landing here is a re-run after the JWK print failed,
  # not a rotation. Say how to get the JWK back out of a key that is already
  # installed, or the advice above reads as "throw away a working TPM key".
  echo "if you only need its public JWK again:" >&2
  echo "  python3 $repo/home/scripts/ko-wif-token.py --key $out --jwk" >&2
  echo "  (with KO_OPENSSL, OPENSSL_MODULES and TPM2OPENSSL_TCTI set as this script sets them)" >&2
  echo "to rotate instead, move it aside, run this again, and republish the JWKS with the new JWK" >&2
  exit 1
fi

# home.nix builds the credential config's --key from "${homeDirectory}/.config/ko"
# literally; it does not consult XDG_CONFIG_HOME. Writing the key somewhere this
# generator thinks is right and sops-nix will never look at fails at activation
# with a bare "No such file or directory", so say it here instead.
if [ "$real_out" != "$(realpath -m "$HOME/.config/ko/wif-tpm.pem")" ]; then
  echo "note: home.nix looks for the device key at \$HOME/.config/ko/wif-tpm.pem," >&2
  echo "      not at $out. Point wifKeyFile at it, or the unit will not find it." >&2
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
# The `cd "$repo"` is load-bearing: `.#` resolves against the CWD, so without it
# a run from outside the repo takes the registry's nixpkgs for one of the two and
# this flake's for the other -- the mixed pair this comment exists to prevent.
pick() { # pick <relative path> <flake attr>... -> first out path that contains it
  local p attr
  for attr in "${@:2}"; do
    for p in $(cd "$repo" && nix build --no-link --print-out-paths "$attr" 2>/dev/null); do
      [ -e "$p/$1" ] && { printf '%s\n' "$p"; return 0; }
    done
  done
  return 1
}
ossl_root=$(pick bin/openssl ".#homeConfigurations.$host.pkgs.openssl" "nixpkgs#openssl") \
  || { echo "need openssl (nixpkgs#openssl)" >&2; exit 1; }
prov_root=$(pick lib/ossl-modules/tpm2.so ".#homeConfigurations.$host.pkgs.tpm2-openssl" "nixpkgs#tpm2-openssl") \
  || { echo "need the tpm2 openssl provider (nixpkgs#tpm2-openssl)" >&2; exit 1; }
# Absolute, like openssl: this script's audience is a freshly-installed host,
# where `python3` may not be on PATH at all -- and the JWK print is the one step
# whose failure used to strand a key that is already installed.
py_root=$(pick bin/python3 ".#homeConfigurations.$host.pkgs.python3" "nixpkgs#python3") \
  || { echo "need python3 (nixpkgs#python3)" >&2; exit 1; }
PY="$py_root/bin/python3"
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
# No pipe: under `set -o pipefail`, grep exiting early on a match can SIGPIPE
# head and turn a successful check into 141.
if ! grep -qx -- '-----BEGIN TSS2 PRIVATE KEY-----' < <(head -1 "$tmpd/key"); then
  echo "openssl wrote $(head -1 "$tmpd/key") -- that is not a TPM key blob; refusing it" >&2
  exit 1
fi

# Prove the key signs through the TPM before installing it. A blob the chip will
# not load is worth finding now, not at the next boot in sops-install-secrets.
printf 'tpm-keygen self test' > "$tmpd/msg"
selftest() {
  "$OSSL" dgst -sha256 -sign "$tmpd/key" -provider tpm2 -provider default \
    -out "$tmpd/sig" "$tmpd/msg" 2>/dev/null || return 1
  "$OSSL" pkey -provider tpm2 -provider default -in "$tmpd/key" -pubout \
    -out "$tmpd/pub.pem" 2>/dev/null || return 1
  "$OSSL" dgst -sha256 -verify "$tmpd/pub.pem" -signature "$tmpd/sig" "$tmpd/msg" >/dev/null 2>&1
}
if ! selftest; then
  echo "the new key could not sign-and-verify through the TPM; not installing it" >&2
  exit 1
fi

# Print the JWK BEFORE installing the key. A failure here after the mv leaves a
# host holding a TPM key nobody has the public half of, and a re-run that refuses
# to overwrite it -- so do the step that can fail while the key is still
# discardable, and install only once its public half is in hand.
mkdir -p "$here/out"
jwk="$here/out/$host-tpm.jwk.json"
if ! KO_OPENSSL="$OSSL" "$PY" "$repo/home/scripts/ko-wif-token.py" --key "$tmpd/key" --jwk > "$tmpd/jwk.json"; then
  echo "could not derive the public JWK from the new key; not installing it" >&2
  exit 1
fi
cp "$tmpd/jwk.json" "$jwk"

chmod 400 "$tmpd/key"
mv "$tmpd/key" "$out"
cat "$jwk"

echo
echo "wrote $out (mode 400, TPM-wrapped) and $jwk"
echo
echo "Next:"
echo "  1. carry ONLY $jwk to the admin machine (it is public)"
echo "  2. republish the JWKS with EVERY current key, this one included --"
echo "     admin-setup.sh replaces the JWKS with exactly the JWKs you pass it"
echo "  3. add $host to wifTpmHosts in home.nix, then build and switch (hms)"
echo "  4. ./tests/wif-tpm.sh $out"
echo "  5. ONLY NOW, and this is the step that makes it a migration rather than an"
echo "     addition:"
echo "       ./install/wif/revoke-kid.sh <old kid> ~/.config/ko/wif.pem"
echo "     Until that runs, the old plaintext key still mints device tokens and"
echo "     nothing about this host has actually got safer."
echo "     tests/wif-tpm.sh stays red on exactly that until it is done."
echo
echo "Steps 3 and 5 are in that order for a reason: the kid you remove in 5 is the"
echo "one the RUNNING generation signs with until 3 has switched it."
