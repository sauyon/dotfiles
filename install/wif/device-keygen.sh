#!/usr/bin/env bash
# Device side (run ON the host being enrolled, e.g. shiori). Generates the P-256 WIF key
# and writes its public JWK to out/<host>.jwk.json for the admin machine.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
host="$(uname -n)"; host="${host%%.*}"
kdir="${XDG_CONFIG_HOME:-$HOME/.config}/ko/experiment"
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
echo "  scp $host:devel/dotfiles/experiments/wif/{admin-setup.sh,out/$host.jwk.json} ."
