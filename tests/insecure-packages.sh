#!/usr/bin/env bash
# Cases for the claim that nothing in this config needs a permittedInsecurePackages
# entry -- the claim home.nix's nixpkgs.config block now makes by having no such
# entry at all.
#
# Why this needs a test rather than a comment. The entry that used to live there
# was scoped to "electron-39.8.10" precisely so that a bitwarden-desktop bump onto
# a different Electron would re-raise the flag for review. The bump happened and
# nobody noticed: the comment still described bitwarden-desktop 2026.6.1 on
# electron 39.8.10 long after the lock had moved to 2026.9.0 on electron 43.6.0,
# so the permit sat there as a dangling exception that read like a live dependency
# on an EOL Electron. A scoped comment is exactly the thing that goes stale
# silently across a lock bump; this file is the same intent expressed as something
# that fails.
#
# Not the same job as .forgejo/workflows/vulnix-scan.yml. That scans the realised
# closure for CVEs weekly and never fails, because this closure always matches
# some. This asks a narrower question with a yes/no answer: does anything here
# need nixpkgs' *insecure* flag switched off. One is a report, this is a gate.
#
# What is actually being protected here, in three parts:
#
#   No host needs a permit. nixpkgs raises its insecure-package error while
#   *evaluating* the flagged derivation, and forcing activationPackage.drvPath
#   instantiates every input derivation transitively -- so with no permit in the
#   config, "every host evaluates" is the claim itself, not a proxy for it. The
#   host list is read out of the flake rather than written down, because this is
#   global config: a new host added to flake.nix is covered the day it lands, and
#   a hand-maintained case list would silently stop covering it.
#
#   The check is still live, under each host's own config. "Every host evaluates"
#   also comes out true if nixpkgs stops flagging things, or if the config turns
#   the check off -- and it turns off in more ways than one. permittedInsecurePackages
#   is the obvious one; allowInsecurePredicate is worse, because check-meta.nix
#   short-circuits on it before it ever consults the permit list, so one line makes
#   every insecure package everywhere evaluate. So the canary is evaluated through
#   `homeConfigurations.<host>.pkgs` -- the host's real nixpkgs, with the real
#   config attached -- rather than a config this file makes up. A permit or a
#   predicate that appears in home.nix stops the canary throwing, and this fails.
#
#   It throws for the *right* reason. A canary that merely "failed to evaluate"
#   would pass on a typo, a renamed attr, an unsupported platform or a fetch
#   error -- which would leave the one case whose whole job is to catch a vacuous
#   pass being a vacuous pass itself. So its stderr has to actually say the
#   package was refused as insecure.
#
#   ./tests/insecure-packages.sh            # tests this checkout
#   ./tests/insecure-packages.sh /path/to/flake
#
# Slower than the other files here: it forces each host's whole closure, which is
# the only thing that exercises the insecure check at all. Seconds on a warm eval
# cache, minutes cold.
set -u
set -o pipefail

FLAKE="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
[ -f "$FLAKE/flake.nix" ] || { echo "no flake.nix in: $FLAKE" >&2; exit 1; }
echo "testing $FLAKE"

D=$(mktemp -d) || { echo "mktemp failed" >&2; exit 1; }
trap 'rm -rf "$D"' EXIT
fails=0; n=0

pass() { # pass <what>
  n=$((n+1)); echo "ok   $1"
}
fail() { # fail <what> <why>
  n=$((n+1)); echo "FAIL $1: $2"; fails=$((fails+1))
}

# Every attr of a flake output, as one name per line. Kept out of a pipeline so a
# nix failure is a nix failure: piping into jq hands the pipeline jq's status, and
# jq exits 0 on empty stdin, which would turn a broken checkout into an empty list
# and report it as "this flake has no hosts".
# Fills the global `names` with one attr per line. Not a pipeline and not a
# process substitution: piping into jq hands the pipeline jq's status, and jq
# exits 0 on empty stdin, which would turn a broken checkout into an empty list
# reported as "this flake has no hosts" -- and an `exit` inside a `< <(...)`
# would kill only the subshell, letting that same wrong message print anyway.
names=()
attrs_of() { # attrs_of <output> [optional]
  names=()
  if ! nix eval --json "$FLAKE#$1" --apply builtins.attrNames >"$D/out" 2>"$D/err"; then
    # An output this flake simply does not define is only an error when the
    # caller says the output is required. Anything else is a broken checkout and
    # is fatal either way -- never a silently empty list, which would drop every
    # case below it and still exit green.
    if [ "${2-}" = optional ] && grep -q 'does not provide attribute' "$D/err"; then
      return 0
    fi
    echo >&2; echo "could not list $1:" >&2; cat "$D/err" >&2
    exit 1
  fi
  mapfile -t names < <(jq -r '.[]' <"$D/out")
}

attrs_of homeConfigurations
hosts=("${names[@]}")
[ "${#hosts[@]}" -gt 0 ] || { echo "flake defines no homeConfigurations" >&2; exit 1; }

# --- no host needs a permit --------------------------------------------------
# Held-back stderr, printed only on failure: on a healthy box it is substituter
# warnings, and on a failing one it carries the "marked as insecure" line and the
# name of the package to go look at -- the entire point of the case.
for host in "${hosts[@]}"; do
  if nix eval --raw "$FLAKE#homeConfigurations.$host.activationPackage.drvPath" \
       >/dev/null 2>"$D/err"; then
    pass "$host evaluates with no permittedInsecurePackages entry"
  else
    fail "$host needs a permittedInsecurePackages entry" \
         "$(grep -iE 'marked as insecure|error:' "$D/err" | head -2 | tr '\n' ' ')"
    echo "     full eval output:"; sed 's/^/     /' "$D/err"
  fi
done

# A nix-darwin system is a separate closure with its own nixpkgs.config, which
# home.nix has no authority over -- so it is checked, not assumed. Its eval works
# from any platform even though its build does not.
attrs_of darwinConfigurations optional
for sys in ${names+"${names[@]}"}; do
  if nix eval --raw "$FLAKE#darwinConfigurations.$sys.system.drvPath" \
       >/dev/null 2>"$D/err"; then
    pass "darwin system $sys evaluates with no permittedInsecurePackages entry"
  else
    fail "darwin system $sys needs a permittedInsecurePackages entry" \
         "$(grep -iE 'marked as insecure|error:' "$D/err" | head -2 | tr '\n' ' ')"
    echo "     full eval output:"; sed 's/^/     /' "$D/err"
  fi
done

# --- the teeth ---------------------------------------------------------------
# Candidates, not one hardcoded package: the hazard this whole file exists for is
# a pin going stale, and a canary is a pin. The first one nixpkgs still flags is
# used; if a lock bump leaves none of them flagged, that is a failure telling you
# to name a currently-flagged package, not a pass.
canary=""
for c in electron_39 electron_37 electron_35; do
  flagged=$(nix eval --json "$FLAKE#homeConfigurations.${hosts[0]}.pkgs" \
              --apply "p: ((p.$c.meta.knownVulnerabilities or [ ]) != [ ])" 2>/dev/null)
  if [ "$flagged" = "true" ]; then canary="$c"; break; fi
done

if [ -z "$canary" ]; then
  fail "no canary is flagged insecure any more" \
       "none of electron_39/electron_37/electron_35 has meta.knownVulnerabilities under this lock; the cases above cannot be trusted until a still-flagged package is named here"
else
  # Through each host's OWN pkgs, so the config under test is home.nix's rather
  # than one this file invents. NIXPKGS_ALLOW_INSECURE is cleared because it would
  # reach an impure eval and let the canary through, reporting a broken check on a
  # config that is fine. These evals are pure, so it cannot reach them -- cleared
  # anyway, so that stays true if this ever gains --impure.
  for host in "${hosts[@]}"; do
    if env -u NIXPKGS_ALLOW_INSECURE \
         nix eval --raw "$FLAKE#homeConfigurations.$host.pkgs.$canary.drvPath" \
         >/dev/null 2>"$D/err"; then
      fail "$host permits insecure packages" \
           "$canary is flagged insecure but evaluated anyway under $host's nixpkgs.config, so a permittedInsecurePackages entry or an allowInsecurePredicate is in effect and this host's case above proves nothing"
    elif ! grep -qi 'marked as insecure' "$D/err"; then
      fail "canary failed for the wrong reason on $host" \
           "$canary did not evaluate, but not because it is insecure: $(grep -iE 'error:' "$D/err" | head -2 | tr '\n' ' ')"
    else
      pass "$host refuses $canary as insecure (its check is live)"
    fi
  done
fi

echo
if [ "$fails" -eq 0 ]; then echo "all $n passed"; else echo "$fails of $n failed"; fi
exit $(( fails > 0 ))
