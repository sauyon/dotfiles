#!/usr/bin/env bash
# Cases for the repo's host lists agreeing with the configurations that exist.
#
# `flake.nix`'s `homeConfigurations` is the only authority on which hosts this
# repo builds. Six other places repeat that set by hand, because reading the
# flake costs a full evaluation and three of the six are shell running on a box
# mid-activation:
#
#   home.nix  archHosts          the Arch-specific package/deploy paths
#   home.nix  hms's CI case       which hosts have a nix-home.yml job
#   home.nix  hmeval's all_hosts  which hosts get evaluated
#   system/deploy's case          which hosts enable multilib
#   nix-home.yml                  which activationPackages CI actually builds
#   home.nix  wifHosts/wifTpmHosts the WIF device-identity fleet
#
# A hand-copied set drifts in two directions and both are silent. A host that
# exists but is missing from a list never gets built, evaluated or deployed --
# `hmeval`'s own comment says so ("Out of step with flake.nix => a host silently
# never gets checked, so keep them together") and then nothing enforced it. A
# host that is in a list but no longer exists is worse than noise: CI spends a
# build slot per run on it, and `nix build` on a missing attribute fails the
# whole job, so one decommission can red every run until someone reads the log.
#
# Why exact set equality rather than a subset for cases 2-4: archHosts, hms's
# case and nix-home.yml's targets are three spellings of one fact -- "the Linux
# hosts CI builds". A subset check passes while two of them disagree, which is
# the state this file exists to catch.
#
# wifHosts is checked as a subset instead, deliberately: it is a different fact
# (which hosts have a published JWK) and mari is in it while having no CI job at
# all. wifTpmHosts ⊆ wifHosts is the real constraint there -- a host keeping its
# key in the TPM must first be enrolled, and the reverse order mints a key
# nothing authorises.
#
#   ./tests/host-inventory.sh
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

# Every reader below is a grep, and a grep that matches nothing returns the
# empty string -- which compares equal to another empty string. That is exactly
# how tests/sops-recipients.sh once reported two green checks off a yq that was
# writing nothing at all, so an empty read is a failure here, never a match.
same() { # same <label> <got> <want>
  if [ -z "$2" ] || [ -z "$3" ]; then
    bad "$1"$'\n'"      read nothing: one side came back empty, so nothing was compared."$'\n'"      want: ${3:-<empty>}"$'\n'"      got:  ${2:-<empty>}"
  elif [ "$2" = "$3" ]; then
    ok "$1"
  else
    bad "$1"$'\n'"      want: $3"$'\n'"      got:  $2"
  fi
}

# subset <label> <needles> <haystack> -- every word of $2 appears in $3.
subset() {
  if [ -z "$2" ] || [ -z "$3" ]; then
    bad "$1"$'\n'"      read nothing: one side came back empty, so nothing was compared."
    return
  fi
  local missing=""
  for h in $2; do
    case " $3 " in *" $h "*) ;; *) missing="$missing $h" ;; esac
  done
  if [ -z "$missing" ]; then ok "$1"
  else bad "$1"$'\n'"      not in $4:$missing"; fi
}

# Sort and space-join, so a list's order is never the thing that fails.
norm() { tr -s ' |",;()[]' '\n' | sed '/^$/d' | sort -u | tr '\n' ' ' | sed 's/ $//'; }

# ── the authority ────────────────────────────────────────────────────────────
# A failure to evaluate is a failure of the case, not a reason to skip: a flake
# that does not evaluate is the regression these lists are copied from.
err=$(mktemp); trap 'rm -f "$err"' EXIT
HOSTS=$(nix eval --json "$repo#homeConfigurations" --apply builtins.attrNames 2>"$err" \
          | tr -d '[]"' | tr ',' '\n' | sort -u | tr '\n' ' ' | sed 's/ $//')
if [ -z "$HOSTS" ]; then
  echo "FAIL 1 - flake.nix#homeConfigurations evaluates" >&2
  sed 's/^/      /' "$err" >&2
  echo; echo "1 check, 1 failed"
  exit 1
fi
ok "flake.nix#homeConfigurations evaluates ($HOSTS)"

# ── the six copies ───────────────────────────────────────────────────────────
line()  { grep -m1 -E "$1" "$2"; }

arch=$(line '^ +archHosts = \[' "$repo/home.nix"      | sed 's/.*\[//' | norm)
hmsci=$(line '^ +[a-z]+(\|[a-z]+)+\) ;;' "$repo/home.nix" | norm)
hmeval=$(line '^ +all_hosts="' "$repo/home.nix"       | sed 's/.*="//;s/"//' | norm)
deploy=$(line '^[a-z]+( \| [a-z]+)+\)' "$repo/system/deploy" | norm)
ciblt=$(grep -oE 'homeConfigurations\.[a-z]+\.activationPackage' \
          "$repo/.forgejo/workflows/nix-home.yml" | sed 's/homeConfigurations\.//;s/\.activationPackage//' | norm)
wif=$(line '^ +wifHosts = \[' "$repo/home.nix"        | sed 's/.*\[//' | norm)
wiftpm=$(line '^ +wifTpmHosts = \[' "$repo/home.nix"  | sed 's/.*\[//' | norm)

# hmeval claims to cover every configuration, so it is the one exact match
# against the flake rather than against the Linux-CI set.
same "hmeval's all_hosts covers exactly the configurations that exist" "$hmeval" "$HOSTS"

# The three spellings of "the Linux hosts CI builds".
same "hms's CI-job case matches archHosts" "$hmsci" "$arch"
same "nix-home.yml builds exactly archHosts" "$ciblt" "$arch"
same "system/deploy's multilib case matches archHosts" "$deploy" "$arch"

# Each list names only hosts that exist.
subset "every archHosts entry is a real configuration" "$arch" "$HOSTS" "homeConfigurations"
subset "every wifHosts entry is a real configuration" "$wif" "$HOSTS" "homeConfigurations"
# Enrolment precedes key storage: a TPM host must already have a published JWK.
subset "every wifTpmHosts entry is also in wifHosts" "$wiftpm" "$wif" "wifHosts"

echo
if [ "$fails" = 0 ]; then echo "$n checks, all good"; else echo "$n checks, $fails failed"; fi
[ "$fails" = 0 ]
