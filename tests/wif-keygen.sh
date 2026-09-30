#!/usr/bin/env bash
# Cases for install/wif/device-keygen.sh — the device-side half of enrolling a
# host that has no TPM (report Part A item 3: utsuho, setsuna, kyuusaku,
# fujiwara's user, mari).
#
# The whole point of this script is to produce a keypair whose PUBLIC half goes
# into the JWKS and whose PRIVATE half is at the path the running system will
# actually sign with. Those are two different files in two different places and
# nothing at runtime ties them together: `home.nix` picks the key path, the JWKS
# grants authority to a kid, and the two only meet minutes later inside Google's
# token exchange. Get the path wrong and enrolment SUCCEEDS on every visible
# signal -- a key is generated, a JWK is printed, the admin publishes it, the
# bucket serves it -- and then the host cannot decrypt, because the signer looks
# somewhere else and finds nothing. The failure surfaces as a 401 on a machine
# you have already walked away from.
#
# So check 1 is not a style point. It reads the key path out of `home.nix` and
# requires the script to write THAT file. Keeping the expectation derived rather
# than pinned is deliberate: if someone moves the key, this test moves with it
# and keeps checking the same property instead of silently asserting history.
#
# Everything here runs against a throwaway $HOME and touches no network, no TPM
# and no bucket.
#
#   ./tests/wif-keygen.sh
#
# Run it from anywhere; paths resolve against the repo this file lives in.
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
prog="$repo/install/wif/device-keygen.sh"
signer="$repo/home/scripts/ko-wif-token.py"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

# The path home.nix hands the signer on a host WITHOUT a TPM, i.e. every host
# item 3 enrols. Parsed, not pinned -- see the header.
#   wifKeyFile = "${config.home.homeDirectory}/.config/ko/"
#     + (if useWifTpm then "wif-tpm.pem" else "wif.pem");
want_rel=$(sed -n '/wifKeyFile *=/,/;/p' "$repo/home.nix" \
  | tr -d '\n' \
  | sed -n 's/.*homeDirectory}\/\([^"]*\)".*else *"\([^"]*\)".*/\1\2/p')
if [ -z "$want_rel" ]; then
  bad "could not read the non-TPM key path out of home.nix (did wifKeyFile change shape?)"
  echo; echo "$n checks, $fails failed"; exit 1
fi

# One run, in a $HOME of its own.
# KO_OUT_DIR keeps the JWK this writes out of the REPO's install/wif/out/. That
# directory is where the admin is told to collect JWKs from, and a test that
# leaves one there plants an unaccountable public key: the private half lives in
# $work and is deleted on exit, so `admin-setup.sh install/wif/out/*.jwk.json`
# would publish a kid nobody holds. That is the GKJJEO3B... incident, generated
# by the test suite instead of by an experiment.
run_keygen() { # run_keygen <home> -> stdout+stderr in $work/log, exit status
  HOME="$1" XDG_CONFIG_HOME="$1/.config" KO_OUT_DIR="$work/out" "$prog" >"$work/log" 2>&1
}

# Snapshot the real out/ so the last check can prove this suite did not touch it.
repo_out="$repo/install/wif/out"
snapshot() { find "$repo_out" -maxdepth 1 -printf '%P %s %T@\n' 2>/dev/null | sort; }
repo_out_before="$(snapshot)"

h1="$work/h1"; mkdir -p "$h1"
if run_keygen "$h1"; then
  ok "device-keygen.sh runs clean on a host with no key yet"
else
  bad "device-keygen.sh runs clean on a host with no key yet"$'\n'"      $(tail -3 "$work/log")"
fi

# --- 2: the key lands where the system will look for it ------------------------
if [ -s "$h1/$want_rel" ]; then
  ok "the private key is written to \$HOME/$want_rel, the path home.nix signs with"
else
  bad "the private key is written to \$HOME/$want_rel, the path home.nix signs with"$'\n'\
"      home.nix hands ko-wif-token \$HOME/$want_rel on a non-TPM host, but that"$'\n'\
"      file does not exist after enrolment. Written instead, relative to \$HOME:"$'\n'\
"      $(cd "$h1" && find . -name '*.pem' -printf '%P\n' 2>/dev/null | head -5)"$'\n'\
"      A host enrolled this way publishes a kid whose private half the signer"$'\n'\
"      never finds: the JWKS is correct, the bucket is correct, and the host"$'\n'\
"      still cannot decrypt."
fi

# --- 3: the private key is not world- or group-readable ------------------------
found=$(cd "$h1" && find . -name '*.pem' | head -1)
if [ -n "$found" ]; then
  mode=$(stat -c '%a' "$h1/${found#./}")
  case "$mode" in
    600|400) ok "the private key is mode $mode" ;;
    *) bad "the private key is mode $mode, not 600/400" ;;
  esac
else
  bad "the private key is mode 600/400 (no key file was written at all)"
fi

# --- 4: the JWK it publishes describes the key it wrote -------------------------
jwkfile="$work/out/$(uname -n | cut -d. -f1).jwk.json"
if [ -n "$found" ] && [ -s "$jwkfile" ]; then
  from_key=$(python3 "$signer" --key "$h1/${found#./}" --jwk 2>/dev/null \
    | python3 -c 'import json,sys; print(json.load(sys.stdin)["kid"])' 2>/dev/null)
  from_out=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["kid"])' "$jwkfile" 2>/dev/null)
  if [ -n "$from_key" ] && [ "$from_key" = "$from_out" ]; then
    ok "out/<host>.jwk.json carries the kid of the key that was generated"
  else
    bad "out/<host>.jwk.json carries the kid of the key that was generated"$'\n'\
"      key says $from_key, out/ says $from_out -- the admin would publish a kid"$'\n'\
"      nobody holds the private half of."
  fi
else
  bad "out/<host>.jwk.json carries the kid of the key that was generated (missing)"
fi

# --- 5: a second run is idempotent, not a silent new identity -------------------
# Re-running after the JWK is already published must NOT mint a new keypair: the
# published kid would keep authorising the old key while the host started signing
# with a new one, and the host would fail to decrypt with everything looking right.
if [ -n "$found" ]; then
  before=$(sha256sum "$h1/${found#./}" | cut -d' ' -f1)
  run_keygen "$h1" || true
  after=$(sha256sum "$h1/${found#./}" | cut -d' ' -f1)
  if [ "$before" = "$after" ]; then
    ok "a second run reuses the existing key instead of minting a new identity"
  else
    bad "a second run reuses the existing key instead of minting a new identity"$'\n'\
"      the key changed, so the published kid now authorises a key the host no"$'\n'\
"      longer has."
  fi
else
  bad "a second run reuses the existing key (no key to re-run against)"
fi

# --- 6: it refuses on a host that already signs with a TPM ----------------------
# Pointing this script at ~/.config/ko/wif.pem (check 2) put it on the same path
# a TPM host's SUPERSEDED file key occupies. On shiori that file is the key whose
# kid was deliberately removed from the JWKS, kept only so the removal could be
# timed. The reuse branch would then print ITS JWK -- and the whole job of this
# script's output is to be carried to the admin and published, which is exactly
# how a revoked identity gets re-authorised, by an operator doing what the script
# told them. The sibling wif-tpm.pem is the unambiguous signal that this host
# does not sign with a file key, so refuse before anything is written.
h2="$work/h2"; mkdir -p "$h2/.config/ko"
: > "$h2/.config/ko/wif-tpm.pem"; chmod 600 "$h2/.config/ko/wif-tpm.pem"
# A DIFFERENT key from h1's, deliberately: if the two were the same, the JWK the
# script would wrongly write is byte-identical to the one already there and
# check 7 can never fail, no matter what the script does.
(umask 077; openssl ecparam -name prime256v1 -genkey -noout -out "$h2/.config/ko/wif.pem")
jwk_before=$(cat "$jwkfile" 2>/dev/null)
if run_keygen "$h2"; then
  bad "refuses on a host that already has a TPM key"$'\n'\
"      it exited 0 and printed a JWK for the superseded file key. Carried to the"$'\n'\
"      admin, that republishes a kid this host no longer signs with."
else
  if grep -qi 'tpm' "$work/log"; then
    ok "refuses on a host that already has a TPM key, and says why"
  else
    bad "refuses on a host that already has a TPM key, and says why"$'\n'\
"      it refused, but the message never mentions the TPM:"$'\n'"      $(tail -2 "$work/log")"
  fi
fi

# --- 7: that refusal left nothing behind for the admin to publish ---------------
if [ "$(cat "$jwkfile" 2>/dev/null)" = "$jwk_before" ]; then
  ok "the refusal does not overwrite out/<host>.jwk.json"
else
  bad "the refusal does not overwrite out/<host>.jwk.json"$'\n'\
"      it refused but still left a JWK where the admin is told to fetch one."
fi

# --- 8: the instructions it prints name paths that exist ------------------------
# The last thing this script does is tell an operator where to carry the JWK. It
# named experiments/wif/ long after the kit moved to install/wif/, which sends
# the one person following it literally to a directory that is not there.
missing=""
while read -r p; do
  [ -n "$p" ] || continue
  [ -e "$repo/$p" ] || missing="$missing $p"
done < <(grep -oE '[a-z][a-z0-9_-]*/wif/' "$work/log" | sort -u)
if [ -z "$missing" ]; then
  ok "the next-step instructions point at directories that exist in this repo"
else
  bad "the next-step instructions point at directories that exist in this repo"$'\n'\
"      not in the repo:$missing"
fi

# --- 9: this suite must not litter the directory the admin collects from -------
if [ "$(snapshot)" = "$repo_out_before" ]; then
  ok "the suite leaves the repo's install/wif/out/ untouched"
else
  bad "the suite leaves the repo's install/wif/out/ untouched"$'\n'\
"      A JWK written here is a public key the admin is told to collect and"$'\n'\
"      publish, whose private half this suite deletes on exit. Diff:"$'\n'\
"      $(diff <(printf '%s' "$repo_out_before") <(snapshot) | head -5)"
fi

echo
if [ "$fails" = 0 ]; then echo "$n checks, all good"; else echo "$n checks, $fails failed"; fi
[ "$fails" = 0 ]
