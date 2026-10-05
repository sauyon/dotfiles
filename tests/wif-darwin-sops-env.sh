#!/usr/bin/env bash
# Cases for mari's TWO sops-nix configurations agreeing on one device identity
# (report Part A item 4).
#
# mari is the only host with two of them: homeConfigurations.mari decrypts the
# five user secrets, and darwinConfigurations.mari decrypts the attic token, the
# authorized-keys file and the ssh-oidc service token as root. Each one picks its
# own GOOGLE_APPLICATION_CREDENTIALS, and for a while they disagreed -- the home
# side had been migrated to the WIF device identity while the darwin side still
# named ~/.config/sops/gcp-key.json, the cluster service-account key.
#
# That disagreement was survivable only while nix-key was still a recipient of
# secrets.yaml. Item 4 drops it, and from that point a configuration pointed at
# gcp-key.json cannot decrypt anything at all: not a warning at activation, a
# hard failure on secrets the OIDC gate and the binary cache need at boot. So the
# agreement is the case worth pinning, and pinning it in a diff is not enough --
# both sides build their credential config from nix/wif-credentials.nix, and the
# arguments they pass it are what has to match.
#
# Why store-path equality rather than comparing the JSON field by field: the
# credential config is `writeText` over `builtins.toJSON`, so the path IS a hash
# of (audience, issuer, sub, key file, signer closure) -- every component of the
# identity and nothing else. Two equal paths cannot be two identities. The
# tradeoff is that the failure message has to say what to look at, because an
# unequal hash does not say which field moved; the diagnostic below prints both
# commands for that reason.
#
# Eval-only: nothing here builds, so it runs on Linux despite being about a
# darwin configuration. It is slow (two flake evaluations, ~1 min cold) and
# needs the dotfiles-private input, which is why it is its own file rather than
# a case inside tests/sops-recipients.sh.
#
#   ./tests/wif-darwin-sops-env.sh
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"

fails=0; n=0
ok()   { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad()  { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }
is()   { if [ "$2" = "$3" ]; then ok "$1"; else bad "$1"$'\n'"      want: $3"$'\n'"      got:  $2"; fi; }

# `nix eval` on the flake, as JSON. A failure here is a failure of the case, not
# a reason to skip: an unevaluatable configuration is the regression.
#
# stderr goes to a file rather than into the capture: nix warns about a dirty
# git tree on every eval, and folding that into stdout would feed the warning
# text to the JSON parser -- which fails in exactly the shape a missing
# attribute does, i.e. a red case with a misleading message.
errs=$(mktemp -d); trap 'rm -rf "$errs"' EXIT
sopsenv() { # sopsenv <flake attr path> <name for the stderr file>
  nix eval --json "$repo#$1.config.sops.environment" 2>"$errs/$2"
}

darwin=$(sopsenv darwinConfigurations.mari darwin) || true
home=$(sopsenv homeConfigurations.mari home)       || true

field() { # field <json> <key>
  printf '%s' "$1" | python3 -c 'import json,sys; print(json.load(sys.stdin).get(sys.argv[1],""))' "$2" 2>/dev/null
}

d_cred=$(field "$darwin" GOOGLE_APPLICATION_CREDENTIALS)
h_cred=$(field "$home" GOOGLE_APPLICATION_CREDENTIALS)
d_exec=$(field "$darwin" GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES)

# ── 1. the darwin side is on a credential config, not a key on disk ──────────
# The shape is the assertion, not the exact store path: a path under /Users (or
# anywhere outside the store) is a decryption key sitting in a file, which is
# the thing the whole trust root exists to remove.
case "$d_cred" in
  /nix/store/*-wif-hosts.json)
    ok "darwinConfigurations.mari decrypts through a WIF credential config" ;;
  "")
    bad "darwinConfigurations.mari decrypts through a WIF credential config"$'\n'"      sops.environment did not evaluate; nix said:"$'\n'"$(tail -5 "$errs/darwin" | sed 's/^/      /')" ;;
  *)
    bad "darwinConfigurations.mari decrypts through a WIF credential config"$'\n'"      got a non-store credential (a key on disk): $d_cred" ;;
esac

# ── 2. the library will actually run the signer ──────────────────────────────
# Google's auth library refuses executable-sourced credentials unless this is
# set, and refuses them at decrypt time -- so without it the credential config
# above is present, correct and inert.
is "darwinConfigurations.mari allows executable-sourced credentials" "$d_exec" "1"

# ── 3. one device identity for mari, not two ────────────────────────────────
if [ -z "$d_cred" ] || [ -z "$h_cred" ]; then
  bad "mari's darwin and home configurations sign as the same device identity"$'\n'"      one of the two did not evaluate (see above)"
elif [ "$d_cred" = "$h_cred" ]; then
  ok "mari's darwin and home configurations sign as the same device identity"
else
  bad "mari's darwin and home configurations sign as the same device identity"$'\n'"      darwin: $d_cred"$'\n'"      home:   $h_cred"$'\n'"      Both come from nix/wif-credentials.nix; compare the arguments each passes"$'\n'"      (hostname, keyFile, useTpm, audience). Diff the two configs with:"$'\n'"        diff <(nix eval --raw $repo#darwinConfigurations.mari.config.sops.environment.GOOGLE_APPLICATION_CREDENTIALS | xargs cat) \\"$'\n'"             <(nix eval --raw $repo#homeConfigurations.mari.config.sops.environment.GOOGLE_APPLICATION_CREDENTIALS | xargs cat)"
fi

# ── 4. neither side kept the cluster service-account key ────────────────────
# Named explicitly because `gcp-key.json` is still the right answer for a host
# that is NOT in wifHosts, so it cannot be grepped out of the repo wholesale --
# it only has to be gone for mari, whose secrets are now encrypted to host-key
# alone.
leftover=$(printf '%s\n%s\n' "$darwin" "$home" | grep -c 'gcp-key\.json' || true)
is "neither mari configuration still names gcp-key.json" "$leftover" "0"

echo
if [ "$fails" = 0 ]; then echo "$n checks, all good"; else echo "$n checks, $fails failed"; fi
[ "$fails" = 0 ]
