#!/usr/bin/env bash
# Generates the offline paper recipient for the dotfiles secrets (report A6.4,
# A7 step 1) and writes it as ONE file that is both the printable sheet and a
# working `SOPS_AGE_KEY_FILE`.
#
# That is not a convenience: age key files ignore `#` lines, so the recovery
# procedure can live on the paper next to the key it applies to, and retyping
# the sheet verbatim gives you a file sops accepts with no editing -- at the
# moment you need it you will not remember which lines were decoration.
#
#   ./install/wif/paper-keygen.sh [path]        # default: ~/.config/ko/paper-host-key.txt
#
# Prints only the PUBLIC recipient. Put that in `.sops.yaml` (an `age:` entry in
# the same creation rule as `gcp_kms`, so either decrypts), then run
# `sops updatekeys secrets.yaml` on a machine that can already decrypt.
#
# A6.4: text only. No QR, no photo, no password manager, no online copy. The
# paper key is unrevocable and unauditable; print it, store the paper, shred the
# file.
set -euo pipefail

out="${1:-${XDG_CONFIG_HOME:-$HOME/.config}/ko/paper-host-key.txt}"
repo="$(cd "$(dirname "$0")/../.." && pwd)"

# The dotfiles repo is public. Refusing here is cheaper than noticing in a diff.
# `realpath -m` resolves a path whose directory does not exist yet, which is the
# normal case the first time this runs.
case "$(realpath -m "$out")" in
  "$repo"/*) echo "refusing to write a private key inside the repo: $out" >&2; exit 1 ;;
esac
if [ -e "$out" ]; then
  echo "exists, refusing to overwrite: $out" >&2; exit 1
fi

if command -v age-keygen >/dev/null 2>&1; then keygen=age-keygen
else
  d=$(nix build --no-link --print-out-paths nixpkgs#age) || { echo "need age-keygen" >&2; exit 1; }
  keygen="$d/bin/age-keygen"
fi

dir="$(dirname "$out")"
mkdir -p "$dir"
umask 077
# A directory, not `mktemp "$out.XXXXXX"`: age-keygen -o refuses a path that
# already exists, and mktemp's whole job is to create the file. Keeping it beside
# $out also keeps the final mv on one filesystem, so the sheet appears whole or
# not at all.
tmpd="$(mktemp -d "$dir/.paper-keygen.XXXXXX")"
trap 'rm -rf "$tmpd"' EXIT

# age-keygen writes the identity plus its own `# created:`/`# public key:`
# comments. Keep those and prepend the procedure; the key line stays last so a
# retyped sheet cannot end mid-key without being obvious.
if ! err="$("$keygen" -o "$tmpd/key" 2>&1)"; then
  printf '%s\n' "age-keygen failed: $err" >&2; exit 1
fi
recipient="$("$keygen" -y "$tmpd/key")"

body="$(cat "$tmpd/key")"
cat > "$tmpd/sheet" <<SHEET
# ─────────────────────────────────────────────────────────────────────────────
# DOTFILES SECRETS — OFFLINE PAPER RECOVERY KEY
#
# Decrypts secrets.yaml in the dotfiles git repo, at any commit, forever. It
# cannot be revoked and its use is not logged anywhere. Treat this sheet as the
# secrets themselves.
#
# Generated: $(date -u '+%Y-%m-%d %H:%M:%SZ')  (UTC)
# Recipient: $recipient
#
# TO RECOVER, on any machine, with no Google account and no network:
#   1. Retype this whole sheet into a file, say paper.txt. The '#' lines are
#      comments; copying them costs nothing and keeps these instructions with
#      the key. Only the AGE-SECRET-KEY line has to be exact.
#   2. chmod 600 paper.txt
#   3. git clone the dotfiles repo, then from its root:
#        env -u GOOGLE_APPLICATION_CREDENTIALS -u GOOGLE_CREDENTIALS \\
#          SOPS_AGE_KEY_FILE=paper.txt sops -d secrets.yaml
#      sops is at nixpkgs#sops if it is not already installed.
#
# AFTER USING IT, or if the paper is ever lost: this key must be replaced, not
# just removed. Generate a new one (install/wif/paper-keygen.sh), swap the
# 'age:' recipient in .sops.yaml, run 'sops updatekeys secrets.yaml' — and
# rotate the secrets themselves: the old ciphertext stays in git history, where
# the old paper key still opens it.
#
# NOW: print this file, store the paper, then shred the file:
#   shred -u <this file>
# Before shredding, prove the key works — from the dotfiles repo root:
#   ./tests/sops-recipients.sh <this file>
# ─────────────────────────────────────────────────────────────────────────────
$body
SHEET

chmod 400 "$tmpd/sheet"
mv "$tmpd/sheet" "$out"

echo "wrote $out (mode 400) — print it, then shred it"
echo
echo "recipient (public, safe to commit):"
echo "  $recipient"
echo
echo "Next:"
echo "  1. add it to .sops.yaml as an 'age:' entry in the same creation rule as gcp_kms"
echo "  2. on a machine that can already decrypt (NOT one whose only key is a device"
echo "     WIF identity — that grants decrypt on host-key but not encrypt):"
echo "       sops updatekeys -y secrets.yaml"
echo "  3. ./tests/sops-recipients.sh $out"
