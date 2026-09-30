#!/usr/bin/env bash
# Cases for gnome-keyring-tpm-seal (defined in home.nix), the one-time enrolment
# that seals this host's gnome-keyring passphrase to its TPM.
#
# What changed and why it needs cases: the passphrase used to be escrowed in
# secrets.yaml and piped in on stdin, so every host shared one value. That made
# the design's own claim -- "the keyring file is useless off this machine" -- false,
# because the escrow is decryptable by a cloud KMS from anywhere, and it meant a
# fresh host could not enrol until it could decrypt someone else's secret. The
# passphrase is now generated here, per host, and never leaves the box.
#
# The cost of that is a footgun this file exists to pin: a *new* passphrase cannot
# open an *existing* login.keyring. Re-sealing over a live keyring would lock the
# user out of every secret in it with no way back, so the script must refuse unless
# told explicitly, and the refusal must not have touched the existing seal.
#
# Seams, so no case needs a TPM or touches the real keyring:
#   SEAL_TPM2_BIN      directory of tpm2_* binaries (stubs here)
#   SEAL_KEYRING_DIR   where login.keyring is looked for
#   XDG_DATA_HOME      where the sealed blob is written
#
#   ./tests/gnome-keyring-seal.sh                 # builds the script, then tests it
#   ./tests/gnome-keyring-seal.sh /path/to/script # tests one you already have
set -u

# The flake ref comes from this script's own location, not `.#`, so running it from
# a worktree tests THAT tree rather than whichever happens to be the cwd; and the
# host is pinned to shiori rather than read from /etc/hostname, because nothing in
# these cases is host-specific and `gnomeKeyringHost` is false on headless and
# darwin hosts -- where the filter below would return [] and `builtins.head` would
# throw, reported only as "could not evaluate". Both conventions are spelled out in
# tests/steam-env.sh and tests/even-terminal.sh.
REPO=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

SCRIPT="${1:-}"
if [ -z "$SCRIPT" ]; then
  drv=$(nix eval --raw "$REPO#homeConfigurations.shiori.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: p.name or "" == "gnome-keyring-tpm-seal") ps)).drvPath') \
    || { echo "could not evaluate gnome-keyring-tpm-seal for shiori" >&2; exit 1; }
  out=$(nix build --no-link --print-out-paths "$drv^out") \
    || { echo "could not build gnome-keyring-tpm-seal" >&2; exit 1; }
  SCRIPT="$out/bin/gnome-keyring-tpm-seal"
fi
[ -x "$SCRIPT" ] || { echo "not executable: $SCRIPT" >&2; exit 1; }
echo "testing $SCRIPT"

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0
ok() { n=$((n+1)); printf 'ok   %s\n' "$1"; }
no() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %s\n     %s\n' "$1" "$2"; }

# --- stubs --------------------------------------------------------------------
# tpm2_create records what it was handed to seal, so a case can assert the
# passphrase's shape without ever seeing a real one. tpm2_unseal echoes that same
# recording back, which is what makes the round-trip check pass; a case flips it to
# prove the mismatch path still fires.
mkdir -p "$D/bin"

cat > "$D/bin/tpm2_createprimary" <<'EOF'
#!/usr/bin/env bash
while [ $# -gt 0 ]; do [ "$1" = -c ] && { printf 'ctx' > "$2"; shift; }; shift; done
exit 0
EOF

cat > "$D/bin/tpm2_create" <<'EOF'
#!/usr/bin/env bash
pub=; priv=; in=
while [ $# -gt 0 ]; do
  case "$1" in
    -u) pub=$2; shift ;;
    -r) priv=$2; shift ;;
    -i) in=$2; shift ;;
  esac
  shift
done
cp "$in" "$SEAL_TEST_RECORD"
printf 'pub'  > "$pub"
printf 'priv' > "$priv"
exit 0
EOF

cat > "$D/bin/tpm2_load" <<'EOF'
#!/usr/bin/env bash
while [ $# -gt 0 ]; do [ "$1" = -c ] && { printf 'ctx' > "$2"; shift; }; shift; done
exit 0
EOF

cat > "$D/bin/tpm2_unseal" <<'EOF'
#!/usr/bin/env bash
out=
while [ $# -gt 0 ]; do [ "$1" = -o ] && { out=$2; shift; }; shift; done
if [ "${SEAL_TEST_CORRUPT:-0}" = 1 ]; then
  printf 'not-what-was-sealed' > "$out"
else
  cp "$SEAL_TEST_RECORD" "$out"
fi
exit 0
EOF

chmod +x "$D/bin"/*

# Run the script in a throwaway home. Prints nothing; the caller inspects $? and
# the files. stdin is closed, which is itself part of what is being asserted: the
# script must not wait on it.
run() { # $@ = extra args
  rm -rf "$D/home" "$D/keyrings"
  mkdir -p "$D/home" "$D/keyrings"
  [ "${WANT_KEYRING:-0}" = 1 ] && printf 'pretend-keyring' > "$D/keyrings/login.keyring"
  # The PKCS#11 store is a second file with the same problem: its own secret is
  # kept *inside* the login keyring, so a fresh login passphrase orphans it.
  [ "${WANT_KEYSTORE:-0}" = 1 ] && printf 'pretend-keystore' > "$D/keyrings/user.keystore"
  [ "${WANT_OLD_SEAL:-0}" = 1 ] && {
    mkdir -p "$D/home/.local/share/gnome-keyring-tpm"
    printf 'GOODpub'  > "$D/home/.local/share/gnome-keyring-tpm/seal.pub"
    printf 'GOODpriv' > "$D/home/.local/share/gnome-keyring-tpm/seal.priv"
  }
  # SEAL_TEST=1 is what unlocks the two override seams. Without it a real run
  # cannot be pointed at a different keyring dir or a different tpm2 -- which
  # matters because SEAL_KEYRING_DIR is the guard standing between a user and an
  # unopenable keyring.
  env HOME="$D/home" XDG_DATA_HOME="$D/home/.local/share" \
      XDG_RUNTIME_DIR="$D" SEAL_TEST=1 \
      SEAL_TPM2_BIN="$D/bin" SEAL_KEYRING_DIR="$D/keyrings" \
      SEAL_TEST_RECORD="$D/sealed" SEAL_TEST_CORRUPT="${SEAL_TEST_CORRUPT:-0}" \
      "$SCRIPT" "$@" </dev/null >"$D/out" 2>"$D/err"
}

SEALDIR="$D/home/.local/share/gnome-keyring-tpm"

# 1. The point of the change: it enrols with nothing on stdin. Under the old
#    escrow design this read stdin and refused an empty passphrase.
WANT_KEYRING=0 run; rc=$?
if [ "$rc" -eq 0 ] && [ -s "$SEALDIR/seal.pub" ] && [ -s "$SEALDIR/seal.priv" ]; then
  ok "seals with no stdin and no secret to hand it"
else
  no "seals with no stdin and no secret to hand it" "exit $rc; $(tail -1 "$D/err" 2>/dev/null)"
fi

# 2. A generated passphrase, not an empty file or a fixed string. 32 bytes is what
#    the design says it is; anything shorter is a silent weakening.
# Exactly 44, not "at least 32": the recorded value is base64, so 32 characters
# would be 24 raw bytes -- a regression to `head -c 24` would satisfy a >= 32 check
# and every other case here while quietly dropping 64 bits.
size=$(wc -c < "$D/sealed" 2>/dev/null || echo 0)
if [ "$size" -eq 44 ]; then
  ok "the sealed passphrase is 44 base64 chars, i.e. exactly 32 random bytes"
else
  no "the sealed passphrase is 44 base64 chars, i.e. exactly 32 random bytes" "got $size bytes"
fi

# 2b. ...and it must survive the *consumer*, which is the thing that bit this.
#     gnome-keyring-tpm reads the unsealed value with `PW="$(unseal)"` and pipes it
#     with printf; command substitution drops NUL bytes and strips trailing
#     newlines (measured: 8 raw bytes in, 6 out), and `--unlock` reads a
#     newline-terminated password. Raw /dev/urandom therefore silently shortens the
#     passphrase for roughly a fifth of enrolments and can empty it outright, with
#     nothing on either side to notice -- the seal round-trip compares files, not
#     what the daemon receives. So what gets sealed has to be text.
if LC_ALL=C grep -qP '^[A-Za-z0-9+/=]+$' "$D/sealed" 2>/dev/null; then
  ok "the sealed passphrase is printable text, so the daemon's pipeline cannot eat it"
else
  no "the sealed passphrase is printable text, so the daemon's pipeline cannot eat it" \
    "sealed bytes are not plain base64-safe text"
fi

raw_len=$(wc -c < "$D/sealed" 2>/dev/null || echo 0)
stripped_len=$(tr -d '\n' < "$D/sealed" 2>/dev/null | wc -c)
if [ "$raw_len" -gt 0 ] && [ "$raw_len" -eq "$stripped_len" ]; then
  ok "the sealed passphrase contains no newline"
else
  no "the sealed passphrase contains no newline" \
    "$raw_len bytes, $stripped_len without newlines -- a newline truncates it at --unlock"
fi

# 3. Two runs must not produce the same passphrase -- per host and per enrolment,
#    or it is a shared secret again by another route.
first=$(cat "$D/sealed" 2>/dev/null | base64 -w0)
WANT_KEYRING=0 run
second=$(cat "$D/sealed" 2>/dev/null | base64 -w0)
if [ -n "$first" ] && [ "$first" != "$second" ]; then
  ok "each enrolment generates a fresh passphrase"
else
  no "each enrolment generates a fresh passphrase" "two runs produced the same bytes"
fi

# 4. The footgun. An existing login.keyring is encrypted with the OLD passphrase,
#    so sealing a new one destroys access to it. Refuse.
WANT_KEYRING=1 run; rc=$?
if [ "$rc" -ne 0 ]; then
  ok "refuses to re-seal over an existing login.keyring"
else
  no "refuses to re-seal over an existing login.keyring" "exited 0 and sealed anyway"
fi

# 5. ...and the refusal must be readable, since the whole risk is a user not
#    understanding why they lost their secrets.
if grep -qi "login.keyring" "$D/err" 2>/dev/null; then
  ok "the refusal names login.keyring"
else
  no "the refusal names login.keyring" "stderr: $(tail -1 "$D/err" 2>/dev/null)"
fi

# 6. Refusing must be inert: a half-done enrolment that wrote seal.pub before
#    bailing would leave the daemon unsealing a passphrase the keyring never had.
if [ ! -e "$SEALDIR/seal.pub" ] && [ ! -e "$SEALDIR/seal.priv" ]; then
  ok "the refusal writes no seal files"
else
  no "the refusal writes no seal files" "seal files exist after a refusal"
fi

# 7. There has to be a way through for someone who has decided to lose the
#    keyring, or the guard just gets worked around with rm.
WANT_KEYRING=1 run --force; rc=$?
if [ "$rc" -eq 0 ] && [ -s "$SEALDIR/seal.pub" ]; then
  ok "--force enrols anyway, over an existing keyring"
else
  no "--force enrols anyway, over an existing keyring" "exit $rc; $(tail -1 "$D/err" 2>/dev/null)"
fi

# 7b. --force has to finish the job. Sealing a new passphrase while leaving the old
#     login.keyring in place gives the daemon a collection it cannot unlock, so the
#     unlock prompts come back -- the exact symptom this whole design removes -- and
#     the script has just printed "sealed ... and verified". Move it aside.
if [ ! -e "$D/keyrings/login.keyring" ] && ls "$D/keyrings"/login.keyring.superseded-* >/dev/null 2>&1; then
  ok "--force moves the unopenable keyring aside instead of leaving it"
else
  no "--force moves the unopenable keyring aside instead of leaving it" \
    "login.keyring still present, or no superseded copy: $(ls "$D/keyrings" | tr '\n' ' ')"
fi

# 7c. user.keystore is the PKCS#11 half, and the daemon runs with
#     --components="pkcs11,secrets" so it opens both. Its unlock secret lives inside
#     the login keyring, so a fresh login passphrase leaves it unopenable -- and the
#     prompt it then raises is the same "Prompt was dismissed" this design removes.
#     Moving only login.keyring would claim the job was done while leaving it.
WANT_KEYRING=1 WANT_KEYSTORE=1 run --force; rc=$?
if [ "$rc" -eq 0 ] && [ ! -e "$D/keyrings/user.keystore" ] &&
   ls "$D/keyrings"/user.keystore.superseded-* >/dev/null 2>&1; then
  ok "--force moves user.keystore aside as well, not just login.keyring"
else
  no "--force moves user.keystore aside as well, not just login.keyring" \
    "left behind: $(ls "$D/keyrings" | tr '\n' ' ')"
fi

# 7d. And the guard has to see it, or the refusal's own advice breaks things: it
#     says "delete it, run again", which removes login.keyring, leaves user.keystore,
#     and then *passes* a guard that only looks for login.keyring.
WANT_KEYRING=0 WANT_KEYSTORE=1 run; rc=$?
if [ "$rc" -ne 0 ] && grep -qi "user.keystore" "$D/err" 2>/dev/null; then
  ok "a lone user.keystore is refused too, and named"
else
  no "a lone user.keystore is refused too, and named" \
    "exit $rc; $(tail -1 "$D/err" 2>/dev/null)"
fi

# 8. The round-trip check is the reason to trust the blob at all, so prove it can
#    still fail rather than assuming the happy path proved it.
SEAL_TEST_CORRUPT=1 WANT_KEYRING=0 WANT_OLD_SEAL=1 run; rc=$?
if [ "$rc" -ne 0 ] && grep -qi "mismatch" "$D/err" 2>/dev/null; then
  ok "a seal that does not round-trip is rejected"
else
  no "a seal that does not round-trip is rejected" "exit $rc; $(tail -1 "$D/err" 2>/dev/null)"
fi

# 8b. ...and rejecting it must be inert. tpm2_create writing straight into the seal
#     dir means the blob the script just called untrusted is the blob the daemon
#     reads at next start -- and with no escrow there is nothing to restore. The
#     previous enrolment has to survive its own replacement failing.
if [ "$(cat "$SEALDIR/seal.pub" 2>/dev/null)" = GOODpub ] &&
   [ "$(cat "$SEALDIR/seal.priv" 2>/dev/null)" = GOODpriv ]; then
  ok "a rejected seal leaves the previous enrolment intact"
else
  no "a rejected seal leaves the previous enrolment intact" \
    "seal.pub is now '$(cat "$SEALDIR/seal.pub" 2>/dev/null)' -- the untrusted blob was kept"
fi
SEAL_TEST_CORRUPT=0

echo
if [ "$fails" -eq 0 ]; then echo "all $n cases passed"; else echo "$fails of $n cases failed"; fi
exit $(( fails > 0 ? 1 : 0 ))
