#!/usr/bin/env bash
# Cases for the dotfiles secrets recipient set (report A7 step 1, the offline
# paper recipient). Three things have to hold at once and only the first is
# visible in a diff:
#
#   1. `.sops.yaml`'s creation rule stays ONE key group. Recipients inside a
#      group are OR -- any single one decrypts -- and that is the whole point of
#      the paper key. Splitting them into `key_groups` makes sops require a
#      share from EVERY group (Shamir), which would mean the paper key alone
#      could not recover anything.
#   2. `secrets.yaml` is actually encrypted to that set. `.sops.yaml` only
#      describes what NEW files get; an existing file changes only when someone
#      remembers to run `sops updatekeys`, and forgetting leaves a recipient
#      that looks configured and cannot decrypt.
#   3. The paper identity really does decrypt, with no Google credentials
#      anywhere in the environment. That is the case worth having: it is the
#      only one that exercises the recovery path the paper exists for, and the
#      only moment it can be run is while the identity still exists on disk,
#      i.e. before you shred it after printing.
#
#   ./tests/sops-recipients.sh                       # 1 and 2
#   ./tests/sops-recipients.sh ~/.config/ko/paper-host-key.txt   # + 3
#
# Run it from anywhere; paths are resolved against the repo this file lives in.
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
rules="$repo/.sops.yaml"
secrets="$repo/secrets.yaml"
identity="${1:-${SOPS_AGE_KEY_FILE:-}}"

fails=0; n=0
ok()   { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad()  { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }
skip() { n=$((n+1)); printf 'skip %d - %s\n' "$n" "$1"; }
is()   { if [ "$2" = "$3" ]; then ok "$1"; else bad "$1"$'\n'"      want: $3"$'\n'"      got:  $2"; fi; }

# yq/age/sops may be on PATH (mise, a profile) or only in nixpkgs. Resolve each
# once: `nix shell` per invocation would dominate the runtime of the whole file.
resolve() { # resolve <binary> <flake attr>
  if command -v "$1" >/dev/null 2>&1; then command -v "$1"; return; fi
  local d
  d=$(nix build --no-link --print-out-paths "nixpkgs#$2" 2>/dev/null) || return 1
  [ -x "$d/bin/$1" ] && printf '%s\n' "$d/bin/$1"
}
YQ=$(resolve yq yq-go)   || { echo "need yq (nixpkgs#yq-go)" >&2; exit 1; }
SOPS=$(resolve sops sops) || { echo "need sops (nixpkgs#sops)" >&2; exit 1; }

# ── 1. the creation rule ────────────────────────────────────────────────────
nrules=$("$YQ" '.creation_rules | length' "$rules")
is ".sops.yaml has exactly one creation rule" "$nrules" 1

# A `key_groups` key anywhere under the rule turns OR into Shamir-shared AND.
groups=$("$YQ" '[.creation_rules[] | select(has("key_groups"))] | length' "$rules")
is ".sops.yaml uses no key_groups (recipients stay OR)" "$groups" 0

# `gcp_kms:`/`age:` in a creation rule are comma-joined strings, and sops also
# accepts a list; normalise both to one recipient per line, sorted.
field() { # field <file> <yq path>
  "$YQ" -r "$2 // \"\"" "$1" | tr ', ' '\n\n' | grep -v '^$' | sort -u
}
rule_kms=$(field "$rules" '.creation_rules[0].gcp_kms')
rule_age=$(field "$rules" '.creation_rules[0].age')

# Membership, not a suffix match: rule_kms is a sorted list and nix-key sorts
# after host-key, so a glob on the whole blob would pass or fail by accident.
if printf '%s\n' "$rule_kms" | grep -qx '.*/cryptoKeys/host-key'; then
  ok ".sops.yaml creation rule encrypts to host-key"
else
  bad ".sops.yaml creation rule encrypts to host-key"$'\n'"      gcp_kms: $(echo "$rule_kms" | tr '\n' ' ')"
fi

# age X25519 recipients are bech32: "age1" plus 58 chars, and bech32 omits
# 1/b/i/o to stay unambiguous when read off paper -- which is exactly how this
# one will be read, so check the charset and not just the length.
nage=$(printf '%s' "$rule_age" | grep -c . || true)
if [ "$nage" -lt 1 ]; then
  bad ".sops.yaml creation rule has an age recipient (the paper key)"
else
  bogus=$(printf '%s\n' "$rule_age" | grep -vc '^age1[02-9ac-hj-np-z]\{58\}$' || true)
  is ".sops.yaml's $nage age recipient(s) are well-formed" "$bogus" 0
fi

# ── 2. secrets.yaml agrees ──────────────────────────────────────────────────
file_kms=$("$YQ" -r '[.sops.gcp_kms[]?.resource_id] | .[]' "$secrets" | sort -u)
file_age=$("$YQ" -r '[.sops.age[]?.recipient] | .[]' "$secrets" | sort -u)
is "secrets.yaml's KMS keys match the creation rule" "$file_kms" "$rule_kms"
is "secrets.yaml's age recipients match the creation rule (updatekeys was run)" "$file_age" "$rule_age"

# ── 3. the recovery path ────────────────────────────────────────────────────
# Strip every way a Google credential can reach sops, including gcloud's ADC
# file, so a pass here cannot be KMS quietly doing the work: CLOUDSDK_CONFIG
# points at an empty directory rather than being unset, because unset means
# "look in ~/.config/gcloud".
if [ -z "$identity" ]; then
  skip "offline recovery: no identity given (pass one as \$1, or set SOPS_AGE_KEY_FILE)"
elif [ ! -r "$identity" ]; then
  bad "offline recovery: identity not readable: $identity"
else
  if AGE=$(resolve age-keygen age); then
    recipient=$("$AGE" -y "$identity" 2>/dev/null)
    if printf '%s\n' "$rule_age" | grep -qxF "$recipient"; then
      ok "the given identity's recipient is one of .sops.yaml's age recipients"
    else
      bad "the given identity's recipient is one of .sops.yaml's age recipients"$'\n'"      got:  $recipient"
    fi
  else
    skip "identity/recipient match: no age-keygen available"
  fi
  empty=$(mktemp -d); trap 'rm -rf "$empty"' EXIT
  if out=$(env -u GOOGLE_APPLICATION_CREDENTIALS -u GOOGLE_CREDENTIALS \
               -u GOOGLE_OAUTH_ACCESS_TOKEN -u GOOGLE_APPLICATION_CREDENTIALS_JSON \
               CLOUDSDK_CONFIG="$empty" SOPS_AGE_KEY_FILE="$identity" \
               "$SOPS" -d "$secrets" 2>&1); then
    # A decrypt that returns nothing is not a decrypt; sops exits 0 on an empty
    # file, and an empty secrets.yaml is its own failure.
    if [ -n "$out" ]; then
      ok "secrets.yaml decrypts with the paper identity alone, no Google credentials"
    else
      bad "secrets.yaml decrypts with the paper identity alone: exit 0 but no output"
    fi
  else
    bad "secrets.yaml decrypts with the paper identity alone, no Google credentials"$'\n'"      $(printf '%s' "$out" | tail -3 | sed 's/^/      /')"
  fi
fi

echo
if [ "$fails" = 0 ]; then echo "$n checks, all good"; else echo "$n checks, $fails failed"; fi
[ "$fails" = 0 ]
