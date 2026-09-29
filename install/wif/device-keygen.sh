#!/usr/bin/env bash
# Device side (run ON the host being enrolled, e.g. shiori). Generates the P-256 WIF key
# and writes its public JWK to out/<host>.jwk.json for the admin machine.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
host="$(uname -n)"; host="${host%%.*}"
# NOT a subdirectory of its own: home.nix's wifKeyFile hands ko-wif-token
# ~/.config/ko/wif.pem on a host without a TPM, and that is the only file the
# running system ever signs with. Generating the key anywhere else enrols the
# host successfully on every visible signal and leaves it unable to decrypt.
# tests/wif-keygen.sh reads the expected path out of home.nix and checks it.
kdir="${XDG_CONFIG_HOME:-$HOME/.config}/ko"
key="$kdir/wif.pem"

# A TPM key beside it means this host does NOT sign with a file key, and that
# $key is its superseded predecessor -- on shiori, the one whose kid was removed
# from the JWKS and which is kept only so the removal can be timed. Everything
# below would then happily print ITS public JWK, and the only thing anyone does
# with this script's output is carry it to the admin and publish it. That is how
# a revoked identity comes back: not by anyone deciding to, but by an operator
# doing exactly what the tool told them. Refuse here, before a single file is
# written, and say which key this host actually uses.
if [ -e "$kdir/wif-tpm.pem" ]; then
  echo "$host already has a TPM-resident device key at $kdir/wif-tpm.pem." >&2
  echo "This script enrols a FILE key, and $key is this host's superseded one:" >&2
  echo "publishing its JWK would re-authorise a key the host no longer signs with." >&2
  echo "To rotate the TPM key instead, use install/wif/tpm-keygen.sh." >&2
  exit 1
fi

mkdir -p "$kdir" "$here/out"; chmod 700 "$kdir"
if [ ! -s "$key" ]; then
  (umask 077; openssl ecparam -name prime256v1 -genkey -noout -out "$key")
  echo "generated $key"
else
  echo "reusing $key"
fi
python3 "$here/../../home/scripts/ko-wif-token.py" --key "$key" --jwk | tee "$here/out/$host.jwk.json"
echo
echo "Next: get out/$host.jwk.json to the admin machine and run admin-setup.sh there, e.g. from the admin machine:"
echo "  scp $host:devel/dotfiles/install/wif/out/$host.jwk.json ."
