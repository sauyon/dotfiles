#!/usr/bin/env bash
# Cases for nix-home.yml's "push what a failed build did realise" path.
#
# The bug that prompted these: run 231 (a057c270) failed all three build
# attempts on kimi-code-2.2.0.drv after realising ~756 derivations, but the push
# step only ran after a green build and only pushed the four top-level out-paths
# -- so none of that reached attic, and run 232 rebuilt all 1,236 derivations.
#
# The contract pinned here:
#   1. `nix build --keep-going` over a closure with one failing leaf still exits
#      non-zero (the job must stay red) while its independent siblings realise.
#   2. .forgejo/scripts/realised-outputs.sh, given the same installables, lists
#      exactly the valid outputs in their derivation closures: the sibling that
#      built is listed, the leaf that failed and the top that never built are
#      not, and no .drv or plain source path leaks in.
#   3. The workflow still parses, builds with --keep-going, and runs the push
#      step after a failed build but not after an earlier step failed.
#
#   ./tests/ci-partial-push.sh
set -u

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
SCRIPT="$ROOT/.forgejo/scripts/realised-outputs.sh"
WF="$ROOT/.forgejo/workflows/nix-home.yml"

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0
ok()   { n=$((n+1)); echo "ok $n - $1"; }
fail() { n=$((n+1)); fails=$((fails+1)); echo "not ok $n - $1"; }
check() { if eval "$2"; then ok "$1"; else fail "$1"; fi; }

# --- a tiny flake: top depends on good (builds) and bad (fails) -------------
# The nonce keeps every run's derivations fresh, so "good" is genuinely built
# here rather than found valid from a previous run, and "bad" is not short-cut.
nonce="$$-$RANDOM-$(date +%s%N)"
sys=$(nix eval --raw --impure --expr builtins.currentSystem)
mkdir -p "$D/flake"
cat > "$D/flake/flake.nix" <<EOF
{
  outputs = { self }: let
    mk = name: script: deps: builtins.derivation {
      inherit name;
      system = "$sys";
      builder = "/bin/sh";
      args = [ "-c" script ];
      nonce = "$nonce";
      deps = deps;
      # A plain source path in the drv closure: must never be listed.
      src = ./payload.txt;
    };
    good = mk "good" "echo good > \$out" [];
    bad  = mk "bad"  "echo failing on purpose; exit 1" [];
    top  = mk "top"  "echo \$deps > \$out" [ good bad ];
  in { packages.$sys = { inherit good bad top; default = top; }; };
}
EOF
echo payload > "$D/flake/payload.txt"
F="path:$D/flake"

good_out=$(nix eval --raw "$F#good.outPath")
bad_out=$(nix eval --raw "$F#bad.outPath")
top_out=$(nix eval --raw "$F#top.outPath")

nix build --keep-going --no-link "$F#top" >"$D/build.log" 2>&1
status=$?
check "failing build exits non-zero under --keep-going" '[ "$status" -ne 0 ]'
check "independent sibling still realised" 'nix-store --check-validity "$good_out" 2>/dev/null'

if [ -x "$SCRIPT" ]; then
  "$SCRIPT" "$F#top" > "$D/list" 2>"$D/script.err"
  sstatus=$?
else
  : > "$D/list"; sstatus=127
fi
check "realised-outputs.sh exits 0" '[ "$sstatus" -eq 0 ]'

has() { local p; while IFS= read -r p; do [ "$p" = "$1" ] && return 0; done < "$D/list"; return 1; }
check "list includes the realised sibling" 'has "$good_out"'
check "list excludes the failed leaf" '! has "$bad_out"'
check "list excludes the unbuilt top" '! has "$top_out"'
no_drv_or_src() {
  local p
  while IFS= read -r p; do
    case "$p" in *.drv|*-payload.txt) return 1 ;; esac
  done < "$D/list"
  return 0
}
check "list holds no .drv or source paths" 'no_drv_or_src'
all_valid() {
  local p
  while IFS= read -r p; do nix-store --check-validity "$p" 2>/dev/null || return 1; done < "$D/list"
}
check "every listed path is valid" 'all_valid'

# --- workflow wiring --------------------------------------------------------
YQ=(nix run nixpkgs#yq-go --)
check "workflow YAML parses" '"${YQ[@]}" -e . "$WF" >/dev/null 2>&1'
build_run=$("${YQ[@]}" '.jobs.build-and-push.steps[] | select(.id == "build") | .run' "$WF" 2>/dev/null)
push_if=$("${YQ[@]}" '.jobs.build-and-push.steps[] | select(.name == "push closures to attic") | .if' "$WF" 2>/dev/null)
check "build step has id 'build' and passes --keep-going" '[[ "$build_run" == *"--keep-going"* ]]'
check "build step calls realised-outputs.sh on failure" '[[ "$build_run" == *"realised-outputs.sh"* ]]'
check "push step runs after a green build or a failed build step" \
  '[[ "$push_if" == *"success()"* && "$push_if" == *"steps.build.conclusion == '"'"'failure'"'"'"* ]]'
check "run blocks parse as bash" '
  rc=0
  while IFS= read -r -d "" blk; do bash -n <(printf "%s" "$blk") || rc=1; done \
    < <("${YQ[@]}" -0 ".jobs.build-and-push.steps[].run // \"\"" "$WF")
  [ "$rc" -eq 0 ]'
check "realised-outputs.sh parses as bash" '[ -f "$SCRIPT" ] && bash -n "$SCRIPT"'

echo "$((n-fails))/$n passed"
[ "$fails" -eq 0 ]
