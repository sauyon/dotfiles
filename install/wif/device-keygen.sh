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
mkdir -p "$kdir" "$here/out"; chmod 700 "$kdir"
key="$kdir/wif.pem"
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
