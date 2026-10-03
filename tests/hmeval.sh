#!/usr/bin/env bash
# Cases for `hmeval` (defined in home.nix).
#
# Two contracts worth pinning, both of which fail silently rather than loudly if
# they ever break:
#
#   * **Local mode can never become a build.** The whole reason hmeval exists is
#     that `nix eval` on a home-manager config will happily start compiling if
#     evaluation needs a derivation realised, and that is what eats every core
#     on the machine. `--max-jobs 0` is the guard. Drop it and nothing errors --
#     the command still prints the right answer, just twenty minutes later with
#     the fans at full. Case 1 is that regression.
#
#   * **--ci must not touch your index, HEAD or working tree.** It commits the
#     dirty tree with `commit-tree` against a private GIT_INDEX_FILE. Get that
#     wrong and it quietly stages the user's whole working tree -- which, on a
#     machine running several sessions against this repo at once, means a later
#     `git commit` sweeps up somebody else's half-finished work. Cases 3 and 4.
#
# They drive the real built script through its seams -- HMEVAL_REPO (a throwaway
# git repo with a bare origin, so nothing touches the real one), HMEVAL_CURL (a
# stub forge), HMEVAL_TOKEN_CMD (a stub credential helper), HMEVAL_NIX (a stub
# nix that records its argv) and HMEVAL_WAIT_SECONDS (seconds, not the half hour
# it ships with).
#
#   ./tests/hmeval.sh                     # builds hmeval, then tests it
#   ./tests/hmeval.sh /path/to/hmeval     # tests one you already have
set -u

SCRIPT="${1:-}"
if [ -z "$SCRIPT" ]; then
  host=$(cat /etc/hostname)
  drv=$(nix eval --raw ".#homeConfigurations.$host.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: p.name or "" == "hmeval") ps)).drvPath') \
    || { echo "could not evaluate hmeval for $host" >&2; exit 1; }
  SCRIPT="$(nix-store --realise "$drv" | tail -1)/bin/hmeval"
  echo "testing $SCRIPT"
fi
[ -x "$SCRIPT" ] || { echo "not executable: $SCRIPT" >&2; exit 1; }

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

ok()   { n=$((n+1)); printf '  ok   %s\n' "$1"; }
bad()  { n=$((n+1)); fails=$((fails+1)); printf '  FAIL %s\n     %s\n' "$1" "$2"; }
check(){ if [ "$2" = "$3" ]; then ok "$1"; else bad "$1" "want [$3], got [$2]"; fi; }

# nix-eval.yml fences its output with these two markers and hmeval slices
# between them. Tests that assert on the result block must therefore serve a log
# that carries them -- a body without fences is the separate "markers out of
# step" case at the bottom of this file, not a shortcut for this one.
fenced(){ printf -- '---8<--- hmeval\n%s\n---8<--- end\n' "$1"; }

# A throwaway repo whose origin is a bare repo next to it, so the push half runs
# for real without touching anything that matters.
setup_repo() {
  rm -rf "$D/origin.git" "$D/repo"
  git init --quiet --bare "$D/origin.git"
  git init --quiet "$D/repo"
  git -C "$D/repo" config user.email "t@t"
  git -C "$D/repo" config user.name "t"
  mkdir -p "$D/repo/tests"
  echo committed > "$D/repo/tracked"
  git -C "$D/repo" add tracked
  git -C "$D/repo" commit --quiet -m first
  git -C "$D/repo" branch -M master
  git -C "$D/repo" remote add origin "$D/origin.git"
  git -C "$D/repo" push --quiet -u origin master
}

# A stub nix that records every argv it is handed and prints a plausible path.
setup_nix() {
  : > "$D/nix.argv"
  cat > "$D/nix" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$NIX_ARGV_LOG"
printf '/nix/store/deadbeef-home-manager-generation'
EOF
  chmod +x "$D/nix"
}

# A stub credential helper, and a stub forge that walks a run from running to a
# terminal status and then serves a job log.
setup_forge() {
  local status="${1:-success}" body="${2:-}"
  cat > "$D/token" <<'EOF'
#!/usr/bin/env bash
cat >/dev/null
printf 'username=oauth2\npassword=tok\n'
EOF
  chmod +x "$D/token"

  : > "$D/calls"
  printf '%s' "$status" > "$D/status"
  printf '%s' "$body" > "$D/body"
  # Mimics the two endpoints hmeval uses, plus the job-id-in-a-404-body quirk.
  # Timestamps: every real log line carries one. $PREFIX lets a case vary its
  # width, because hmeval measures the prefix off the marker rather than
  # assuming 29 -- the assumption hms makes, and the one that turns a green run
  # into "no output" when it is off by one.
  cat > "$D/curl" <<'EOF'
#!/usr/bin/env bash
url=""
for a in "$@"; do case "$a" in https://*) url=$a ;; esac; done
printf '%s\n' "$url" >> "$CALLS"
sha=$(cat "$SHAFILE" 2>/dev/null || echo none)
case "$url" in
  *actions/tasks*)
    printf '{"workflow_runs":[{"run_number":7,"head_sha":"%s","workflow_id":"nix-eval.yml","status":"%s"}]}\n200' \
      "$sha" "$(cat "$STATUSFILE")" ;;
  *actions/runs/7/jobs/0)
    printf 'oh no job_id 42 not found' ;;
  *actions/jobs/42/logs*)
    while IFS= read -r line; do printf '%s%s\n' "$PREFIX" "$line"; done < "$BODYFILE"
    printf '200' ;;
  *) printf '{}\n404' ;;
esac
EOF
  chmod +x "$D/curl"
}

run_hmeval() {
  CALLS="$D/calls" STATUSFILE="$D/status" BODYFILE="$D/body" SHAFILE="$D/sha" \
  PREFIX="${PREFIX:-2026-10-02T00:00:00.00000000Z}" \
  NIX_ARGV_LOG="$D/nix.argv" \
  HMEVAL_REPO="$D/repo" HMEVAL_CURL="$D/curl" HMEVAL_TOKEN_CMD="$D/token" \
  HMEVAL_NIX="$D/nix" HMEVAL_WAIT_SECONDS=5 \
    "$SCRIPT" "$@" > "$D/out" 2> "$D/err"
  echo $?
}

# The stub forge has to answer for whatever SHA hmeval just pushed, and hmeval
# is the only thing that knows it. Read it back off the scratch branch.
arm_forge_for_push() {
  git -C "$D/origin.git" rev-parse "refs/heads/eval/$(uname -n)" > "$D/sha" 2>/dev/null \
    || echo none > "$D/sha"
}

echo "== local mode (the default)"
setup_repo; setup_nix; setup_forge
rc=$(run_hmeval utsuho)
check "local: exits 0" "$rc" 0
# Case 1 -- the regression. Without --max-jobs 0 an evaluation that needs a
# derivation realised compiles it here instead of failing fast.
if grep -q -- "--max-jobs 0" "$D/nix.argv"; then
  ok "local: passes --max-jobs 0, so eval can never start a build"
else
  bad "local: passes --max-jobs 0" "nix argv was: $(cat "$D/nix.argv")"
fi
if grep -q -- "--cores 1" "$D/nix.argv"; then
  ok "local: passes --cores 1"
else
  bad "local: passes --cores 1" "nix argv was: $(cat "$D/nix.argv")"
fi
check "local: evaluates exactly the host asked for" \
  "$(grep -c 'homeConfigurations.utsuho.activationPackage.drvPath' "$D/nix.argv")" 1
check "local: makes no forge requests" "$(wc -l < "$D/calls" | tr -d ' ')" 0
check "local: pushes nothing" \
  "$(git -C "$D/origin.git" for-each-ref --format='%(refname)' refs/heads/eval | wc -l | tr -d ' ')" 0

echo "== local mode, every host by default"
setup_repo; setup_nix; setup_forge
rc=$(run_hmeval)
check "local: exits 0" "$rc" 0
check "local: bare hmeval covers all six hosts" \
  "$(grep -c 'homeConfigurations' "$D/nix.argv")" 6

echo "== --ci leaves your working tree alone"
setup_repo; setup_nix
# A dirty tree with one modified tracked file, one new untracked file, and one
# *staged* change -- the three states that a careless `git add -A` would merge.
echo dirty > "$D/repo/tracked"
echo fresh > "$D/repo/untracked"
echo staged > "$D/repo/stagedfile"
git -C "$D/repo" add stagedfile
before_head=$(git -C "$D/repo" rev-parse HEAD)
before_status=$(git -C "$D/repo" status --porcelain)
setup_forge success "$(fenced "  utsuho     /nix/store/x-home-manager-generation.drv")"
( sleep 0.2; arm_forge_for_push ) &
rc=$(run_hmeval --ci utsuho)
wait
check "--ci: exits 0 on a green run" "$rc" 0
# Case 3 -- the index/HEAD/worktree contract.
check "--ci: HEAD unmoved" "$(git -C "$D/repo" rev-parse HEAD)" "$before_head"
check "--ci: index and worktree untouched" \
  "$(git -C "$D/repo" status --porcelain)" "$before_status"
check "--ci: your file is still dirty, not committed" \
  "$(cat "$D/repo/tracked")" dirty

echo "== --ci carries the dirty tree to the runner"
# Case 4 -- the point of the scratch commit. The pushed tree must hold the
# *working* content, not what HEAD has.
pushed=$(git -C "$D/origin.git" rev-parse --verify -q "refs/heads/eval/$(uname -n)" || true)
if [ -z "$pushed" ]; then
  # The branch is deleted on exit, so read it from the reflog of the bare repo.
  pushed=$(cat "$D/sha")
fi
if [ "$pushed" != none ] && git -C "$D/origin.git" cat-file -e "$pushed" 2>/dev/null; then
  check "--ci: pushed commit carries the dirty content" \
    "$(git -C "$D/origin.git" show "$pushed:tracked" 2>/dev/null)" dirty
  check "--ci: pushed commit carries untracked files too" \
    "$(git -C "$D/origin.git" show "$pushed:untracked" 2>/dev/null)" fresh
  req=$(git -C "$D/origin.git" show "$pushed:.hmeval-request" 2>/dev/null)
  check "--ci: request names the mode" \
    "$(printf '%s\n' "$req" | grep '^mode=')" "mode=eval"
  check "--ci: request names the host" \
    "$(printf '%s\n' "$req" | grep '^hosts=')" "hosts=utsuho"
  check "--ci: request has no stray newlines in values" \
    "$(printf '%s\n' "$req" | grep -c '^[a-z]*=')" 4
else
  bad "--ci: pushed a scratch commit" "no commit found on the origin"
fi

echo "== --ci cleans up after itself"
check "--ci: scratch branch deleted from origin" \
  "$(git -C "$D/origin.git" for-each-ref --format='%(refname)' refs/heads/eval | wc -l | tr -d ' ')" 0

echo "== --ci reports the runner's answer"
check "--ci: prints the fenced result block" \
  "$(grep -c 'home-manager-generation.drv' "$D/out")" 1

echo "== --tests implies --ci"
setup_repo; setup_nix
setup_forge success "$(fenced "  === tests/hmeval.sh PASSED")"
( sleep 0.2; arm_forge_for_push ) &
rc=$(run_hmeval --tests tests/hmeval.sh)
wait
check "--tests: exits 0" "$rc" 0
pushed=$(cat "$D/sha")
req=$(git -C "$D/origin.git" show "$pushed:.hmeval-request" 2>/dev/null)
check "--tests: mode is tests" "$(printf '%s\n' "$req" | grep '^mode=')" "mode=tests"
check "--tests: carries the script list" \
  "$(printf '%s\n' "$req" | grep '^tests=')" "tests=tests/hmeval.sh"

echo "== a red run is a red exit"
setup_repo; setup_nix
setup_forge failure "$(fenced "  utsuho     FAILED")"
( sleep 0.2; arm_forge_for_push ) &
rc=$(run_hmeval --ci utsuho)
wait
check "--ci: exits 1 on a red run" "$rc" 1
check "--ci: still prints what the runner said" \
  "$(grep -c FAILED "$D/out")" 1

echo "== a log with no markers is reported, not silently green"
setup_repo; setup_nix
setup_forge success "nothing fenced in here at all"
( sleep 0.2; arm_forge_for_push ) &
rc=$(run_hmeval --ci utsuho)
wait
if grep -q 'markers out of step' "$D/err"; then
  ok "--ci: says the result block was missing"
else
  bad "--ci: says the result block was missing" "stderr was: $(cat "$D/err")"
fi

echo "== the timestamp prefix is measured, not assumed"
# hms hardcodes 29. If hmeval did too, a log whose prefix is any other width
# would come back as "no result block" on a run that was perfectly green.
setup_repo; setup_nix
setup_forge success "$(fenced "  utsuho     /nix/store/wide-prefix.drv")"
( sleep 0.2; arm_forge_for_push ) &
PREFIX="2026-10-02T00:00:00.000000000000Z  " rc=$(run_hmeval --ci utsuho)
wait
check "--ci: exits 0 with an unusually wide prefix" "$rc" 0
check "--ci: still finds the block, and strips the prefix cleanly" \
  "$(cat "$D/out")" "  utsuho     /nix/store/wide-prefix.drv"

echo
echo "$((n - fails))/$n passed"
[ "$fails" -eq 0 ]
