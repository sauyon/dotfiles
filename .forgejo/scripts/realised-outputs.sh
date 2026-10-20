#!/usr/bin/env bash
# realised-outputs.sh INSTALLABLE... -- print, one per line, every output of
# every derivation in the installables' build closures that is valid in the
# local store. nix-home.yml calls it after a build has failed for good, so the
# push step can hand attic whatever that run did manage to realise.
#
# Why: run 231 (a057c270) failed on kimi-code-2.2.0.drv after realising ~756
# derivations, but only the four top-level out-paths were ever pushed, and
# those did not exist. Run 232 then rebuilt 1,236 derivations, crate by crate
# for forgejo-cli and package by package for hunkdiff's bun deps, in over ten
# minutes. Every one of those had been sitting valid in run 231's store.
#
# Outputs only, never the drv closure as a whole. A .drv closure also holds the
# .drv files and every plain source path a derivation names -- the flake source
# and any private-input file among them -- and attic needs none of that to
# substitute a build. Pushing an output does push its runtime closure, but that
# is exactly what a green run pushes too.
#
# Validity, not existence: nix moves an output into place only once its build
# succeeds, but `nix-store --check-validity` is the store's own answer and costs
# one call. Uses bash builtins and nix only -- the CI image has no grep/awk/sed.
set -euo pipefail

[ "$#" -gt 0 ] || { echo "usage: $0 INSTALLABLE..." >&2; exit 2; }

# The four home closures' drv graph is ~17k paths, ~1.2 MB of text: one argv
# that size sits uncomfortably close to Linux's ARG_MAX (2 MB under an 8 MB
# stack, environment included) and grows with every input bump. So pass paths
# in batches. `|| return`, because a failure inside a $(...) loop would
# otherwise be masked by the next batch succeeding.
batched() { # batched ARRAY-NAME CMD [ARG...] -- runs CMD ARG... <1000 paths>
  local -n _paths=$1; shift
  local i
  for ((i = 0; i < ${#_paths[@]}; i += 1000)); do
    "$@" "${_paths[@]:i:1000}" || return
  done
}

# Each nix call is captured with $(...) rather than read through <(...), so a
# failing one stops the script under `set -e` instead of yielding a silently
# short list. Instantiates without building; the failed build already
# evaluated these, so it is quick.
tops=$(nix path-info --derivation "$@")
reqs=$(nix-store --query --requisites $tops)   # unquoted: one arg per path

# `if`, not `[[ ]] && …`: a false test as a loop's last command makes the loop
# itself return 1, which is easy to misread under `set -e`.
drvs=()
while IFS= read -r p; do
  if [[ "$p" == *.drv ]]; then drvs+=("$p"); fi
done <<< "$reqs"

outs=$(batched drvs nix-store --query --outputs)
mapfile -t outv <<< "$outs"
bad=$(batched outv nix-store --check-validity --print-invalid)

declare -A skip=()
while IFS= read -r p; do
  if [ -n "$p" ]; then skip["$p"]=1; fi
done <<< "$bad"

# Same map dedupes: a path printed once is skipped thereafter.
while IFS= read -r p; do
  if [ -z "$p" ] || [ -n "${skip[$p]:-}" ]; then continue; fi
  skip["$p"]=1
  printf '%s\n' "$p"
done <<< "$outs"
