#!/usr/bin/env bash
# Cases for the Claude Code PreToolUse hook that refuses Edit/Write in a repo's
# primary checkout while it sits on its default branch (home.nix,
# claudeBaseSettings.hooks.PreToolUse, the `Edit|Write|NotebookEdit` matcher).
#
# Why this exists: the guard is there to keep sustained agent work out of the
# tree `hms` reads, and "work in a worktree, then merge" is the instruction it
# hands back. But the merge lands in that same primary checkout, and a merge
# that conflicts can only be resolved by editing the conflicted files where
# they are -- which the guard refused, leaving CLAUDE_ALLOW_MAIN_EDIT=1 as the
# only way to finish the workflow the guard itself prescribes. An in-progress
# merge is a bounded state git already tracks (MERGE_HEAD), so the guard reads
# that rather than asking for an override.
#
# The hook is an inline shell string, not a derivation, so what is driven here
# is the string as evaluated out of the flake: the shell that actually ships,
# against throwaway repos. A deny is the hook printing a permissionDecision; an
# allow is it printing nothing.
#
#   ./tests/claude-main-edit-guard.sh
set -u

# The flake ref comes from this script's own location, not `.#`, so running it
# from a worktree tests THAT tree rather than whichever happens to be the cwd.
# The host is read from /etc/hostname because the CI job writes $TEST_HOST into
# it before running tests/ (see .forgejo/workflows/nix-eval.yml). Orca's sandbox
# has no /etc/hostname, and neither does darwin, so fall back to `uname -n`.
REPO=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
host=$(cat /etc/hostname 2>/dev/null || uname -n)

T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
fails=0; n=0

if ! nix eval --json \
     "$REPO#homeConfigurations.$host.config.programs.claude-code.settings.hooks.PreToolUse" \
     >"$T/hooks.json" 2>"$T/err"; then
  echo "nix eval failed for $host's PreToolUse hooks:" >&2; cat "$T/err" >&2; exit 1
fi
jq -r '.[] | select(.matcher | test("Edit")) | .hooks[0].command' "$T/hooks.json" >"$T/hook.sh"
[ -s "$T/hook.sh" ] || { echo "no PreToolUse hook matches Edit" >&2; exit 1; }

# The hook calls jq and git by store path, and eval alone does not realise
# them: on a box that has never switched to this config they are not there.
for p in $(grep -o '/nix/store/[^/ ]*' "$T/hook.sh" | sort -u); do
  [ -e "$p" ] || nix build --no-link "$p" 2>"$T/err" \
    || { echo "could not realise $p:" >&2; cat "$T/err" >&2; exit 1; }
done

# Claude Code runs a hook command with `/bin/sh -c`, and /bin/sh is dash on a
# Debian-style host: one bashism there is a syntax error, exit 2, which is a
# deny -- of every edit, everywhere. So the hook runs under dash here.
SH=$(nix build --no-link --print-out-paths "$REPO#homeConfigurations.$host.pkgs.dash")/bin/dash \
  || { echo "could not build dash" >&2; exit 1; }

# Commits need an identity and a throwaway repo has none; nothing reads it.
export GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL= GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null

verdict() { # verdict <file path> [VAR=value ...] -> allow | deny
  local f=$1 out; shift
  out=$(printf '{"tool_input":{"file_path":"%s"}}' "$f" \
    | env -u CLAUDE_ALLOW_MAIN_EDIT "$@" "$SH" "$T/hook.sh")
  case "$out" in
    "") echo allow ;;
    *'"permissionDecision":"deny"'*) echo deny ;;
    *) echo "unexpected: $out" ;;
  esac
}

check() { # check <what> <expected> <actual>
  n=$((n+1))
  if [ "$2" = "$3" ]; then
    echo "ok   $1"
  else
    echo "FAIL $1"; echo "     expected: $2"; echo "     actual:   $3"
    fails=$((fails+1))
  fi
}

# A primary checkout on master, with `topic` conflicting with it on f and
# `clean` adding a file of its own. Each scenario below gets a fresh one, so
# none inherits the state the last one left -- a second merge of an
# already-merged branch is "Already up to date" and sets nothing up at all.
mkrepo() { # mkrepo <name>; sets R
  R="$T/$1"
  git init -q -b master "$R"
  echo base > "$R/f"; git -C "$R" add f; git -C "$R" commit -q -m base
  git -C "$R" checkout -q -b topic
  echo topic > "$R/f"; git -C "$R" commit -q -am topic
  git -C "$R" checkout -q -b clean master
  echo clean > "$R/k"; git -C "$R" add k; git -C "$R" commit -q -m clean
  git -C "$R" checkout -q master
  echo master > "$R/f"; git -C "$R" commit -q -am master
}

conflicted() { # conflicted <what>: the fixture must really have left a conflict
  [ -n "$(git -C "$R" ls-files -u)" ] || { echo "fixture: $1 did not conflict" >&2; exit 1; }
}

mkrepo guard

# --- the guard itself --------------------------------------------------------
check "primary checkout on master: edit refused" deny "$(verdict "$R/f")"
check "a file that does not exist yet is refused too" deny "$(verdict "$R/new/dir/g")"
check "CLAUDE_ALLOW_MAIN_EDIT=1 lets it through" allow \
  "$(verdict "$R/f" CLAUDE_ALLOW_MAIN_EDIT=1)"
check "outside any repo: edit allowed (and the hook parses under dash)" allow "$(verdict /nonexistent/x)"

# The refusal names the way out, and checks facts rather than describing them:
# a session already in a worktree of this repo is pointed at the same file
# there; anyone in a drovr run asks drovr where its worktree is and enters it
# only if git lists it; everyone else makes one -- an Orca worktree where Orca
# is installed (orca.nix: GUI Linux), EnterWorktree's own elsewhere. Never a
# bare in-repo `git worktree add`: its untracked dir makes hms refuse the tree.
orca_on() { # orca_on <host> -> true | false; exits non-zero on an eval failure,
           # which each call site turns into the suite failing
  local v
  v=$(nix eval --raw "$REPO#homeConfigurations.$1.config.home.packages" \
    --apply 'ps: if builtins.any (p: (p.pname or "") == "orca") ps then "true" else "false"') \
    || { echo "could not evaluate $1's packages" >&2; exit 1; }
  case "$v" in true|false) echo "$v" ;; *) echo "orca_on $1: got '$v'" >&2; exit 1 ;; esac
}
refusal() { # refusal <file path> <session cwd> [VAR=value ...] -> deny reason, session sid-1
  printf '{"session_id":"sid-1","cwd":"%s","tool_input":{"file_path":"%s"}}' "$2" "$1" \
    | env -u CLAUDE_ALLOW_MAIN_EDIT -u CLAUDE_PROJECT_DIR "${@:3}" "$SH" "$T/hook.sh" \
    | jq -r '.hookSpecificOutput.permissionDecisionReason'
}
says() { case "$reason" in *"$1"*) echo yes ;; *) echo no ;; esac; }
here=$(orca_on "$host") || exit 1

git -C "$R" worktree add -q -b sess "$T/sess" master; mkdir "$T/sess/sub"
reason=$(refusal "$R/new/dir/g" "$T/sess/sub")
check "a session in a worktree of this repo is pointed at the same file there" yes \
  "$(says "edit $T/sess/new/dir/g there instead")"
check "...and is not sent off to make another" no "$(says 'EnterWorktree with a name')"
reason=$(refusal "$R/f" "$R")
check "a session in the primary checkout is not told it is in a worktree" no "$(says 'there instead')"
check "a drovr run asks drovr where its worktree is" yes "$(says 'run `drovr path <run>`')"
check "...and enters it only if git lists it as another worktree" yes \
  "$(says "If git lists that path as a worktree other than $R (see \`git worktree list\`), EnterWorktree on it")"
check "...or stops and reports" yes "$(says 'otherwise the run is not isolated: stop and report')"
if [ "$here" = true ]; then
  check "refusal names an Orca worktree of this repo" yes "$(says "orca worktree create --repo path:$R ")"
else
  check "refusal names no orca command on a host without it" no "$(says 'orca worktree create')"
fi
check "refusal falls back to EnterWorktree's own worktree" yes "$(says 'EnterWorktree with a name')"
# --resume with this session's id, from its own directory: --continue picks the
# most recent conversation there, which need not be the refused one.
check "refusal names the override for exactly this session" yes \
  "$(says "\`cd $R && CLAUDE_ALLOW_MAIN_EDIT=1 claude --resume sid-1\`")"
reason=$(refusal "$R/f" "$R" CLAUDE_PROJECT_DIR=/proj)
check "the override resumes from the project dir when Claude names one" yes \
  "$(says '`cd /proj && CLAUDE_ALLOW_MAIN_EDIT=1 claude --resume sid-1`')"
reason=$(printf '{"tool_input":{"file_path":"%s"}}' "$R/f" \
  | env -u CLAUDE_ALLOW_MAIN_EDIT -u CLAUDE_PROJECT_DIR "$SH" "$T/hook.sh" \
  | jq -r '.hookSpecificOutput.permissionDecisionReason')
check "with no session in the input the override still reads as a command" yes \
  "$(says '`cd <this directory> && CLAUDE_ALLOW_MAIN_EDIT=1 claude --resume <session id>`')"

# Every host in the flake, not just this one: the hook text, drovr's own
# worktrees and the auto-mode cd rules all follow whether Orca is installed.
hosts=$(nix eval --json "$REPO#homeConfigurations" --apply builtins.attrNames) \
  || { echo "could not list homeConfigurations" >&2; exit 1; }
hosts=$(jq -r '.[]' <<<"$hosts")
with=0; without=0
for h in $hosts; do
  orca=$(orca_on "$h") || exit 1
  if [ "$orca" = true ]; then with=$((with+1)); wt=false; else without=$((without+1)); wt=true; fi
  toml=$(nix eval --raw "$REPO#homeConfigurations.$h.config.xdg.configFile.\"drovr/config.toml\".text") \
    || { echo "could not evaluate $h's drovr config" >&2; exit 1; }
  got=$(sed -n 's/^worktree = //p' <<<"$toml")
  check "$h: drovr worktree = $wt (Orca: $orca)" "$wt" "$got"
  hook=$(nix eval --json "$REPO#homeConfigurations.$h.config.programs.claude-code.settings.hooks.PreToolUse") \
    || { echo "could not evaluate $h's hooks" >&2; exit 1; }
  hook=$(jq -r '[.[] | select(.matcher | test("Edit")) | .hooks[0].command] | first // ""' <<<"$hook")
  # Run this host's own refusal program, not just grep it: CI runs the live hook
  # only as an Orca host, so a broken no-Orca branch would otherwise pass here
  # and, as invalid jq, emit nothing -- which lets every edit through.
  # The jq program between its quotes. Tied to the hook's exact spelling of
  # `--arg w "$wt" '...'`: a respelling fails every host loudly, not silently.
  prog=${hook#*--arg w \"\$wt\" \'}; prog=${prog%%\'*}
  run() { jq -nc --arg r /x --arg f /x/f --arg s sid-1 --arg c /cwd --arg w "$1" "$prog" 2>/dev/null \
    | jq -r 'select(.hookSpecificOutput.permissionDecision == "deny") | .hookSpecificOutput.permissionDecisionReason'; }
  reason=$(run "")
  check "$h: refusal program emits a deny" yes "$([ -n "$reason" ] && echo yes || echo no)"
  check "$h: refusal names orca iff Orca is installed" "$orca" \
    "$([ "$(says 'orca worktree create --repo path:/x ')" = yes ] && echo true || echo false)"
  check "$h: a drovr run enters only a worktree git lists" yes \
    "$(says 'If git lists that path as a worktree other than /x (see `git worktree list`), EnterWorktree on it')"
  check "$h: ...or stops and reports" yes "$(says 'otherwise the run is not isolated: stop and report')"
  check "$h: everyone else falls back to EnterWorktree's own worktree" yes \
    "$(says 'If you are not in a drovr run, work in a worktree:')"
  check "$h: refusal names the override for exactly this session, outside a run" yes \
    "$(says 'outside a drovr run, ask the user to relaunch this session with `cd /cwd && CLAUDE_ALLOW_MAIN_EDIT=1 claude --resume sid-1`')"
  check "$h: refusal says the agent cannot grant the override" yes \
    "$(says 'You cannot set CLAUDE_ALLOW_MAIN_EDIT yourself, and must not route around this guard through Bash.')"
  check "$h: refusal suggests no in-repo git worktree" no "$(says 'git worktree add')"
  check "$h: refusal never says to relaunch with drovr new" no "$(says 'drovr new')"
  reason=$(run /x-wt)
  check "$h: in a worktree, the refusal names the same file there" yes "$(says 'edit /x-wt/f there instead')"
  check "$h: ...and nothing else to do" no "$(says 'drovr path')"
  allow=$(nix eval --json "$REPO#homeConfigurations.$h.config.programs.claude-code.settings.autoMode.allow") \
    || { echo "could not evaluate $h's auto-mode rules" >&2; exit 1; }
  check "$h: auto mode allows cd into .drovr/wt iff drovr makes them" "$wt" \
    "$(jq -r 'any(.[]; test("into a drovr worktree"))' <<<"$allow")"
  check "$h: auto mode allows cd into an Orca worktree" true \
    "$(jq -r 'any(.[]; test("into an Orca worktree"))' <<<"$allow")"
done
# A loop that saw only one kind of host tested only half the gate.
check "hosts with Orca and without both exist" "yes yes" \
  "$([ "$with" -gt 0 ] && echo yes || echo no) $([ "$without" -gt 0 ] && echo yes || echo no)"

git -C "$R" worktree add -q "$T/wt" topic
check "linked worktree: edit allowed" allow "$(verdict "$T/wt/f")"

# --- merges ------------------------------------------------------------------
mkrepo merge
git -C "$R" merge -q topic >/dev/null 2>&1; conflicted "the merge"
check "merge in progress: the conflicted file can be edited" allow "$(verdict "$R/f")"
check "merge in progress: so can a file the merge did not touch" allow "$(verdict "$R/other")"

echo resolved > "$R/f"; git -C "$R" commit -q -am merged
check "merge concluded: edit refused again" deny "$(verdict "$R/f")"

mkrepo merge-abort
git -C "$R" merge -q topic >/dev/null 2>&1; conflicted "the merge"
git -C "$R" merge --abort
check "merge aborted: edit refused again" deny "$(verdict "$R/f")"

# --- squash merges -----------------------------------------------------------
# `git merge --squash` writes no MERGE_HEAD -- which is also why `merge --abort`
# refuses one. Its marker is SQUASH_MSG, there from the squash until its commit
# whether or not anything conflicted.
mkrepo squash
git -C "$R" merge -q --squash topic >/dev/null 2>&1; conflicted "the squash"
check "squash with conflicts: the conflicted file can be edited" allow "$(verdict "$R/f")"

echo resolved > "$R/f"; git -C "$R" add f
check "squash resolved and staged, not committed: still editable" allow "$(verdict "$R/f")"

git -C "$R" commit -q -m squashed
check "squash committed: edit refused again" deny "$(verdict "$R/f")"

mkrepo squash-clean
git -C "$R" merge -q --squash clean >/dev/null 2>&1
check "clean squash, not committed: editable for a touch-up" allow "$(verdict "$R/k")"

mkrepo squash-reset
git -C "$R" merge -q --squash topic >/dev/null 2>&1; conflicted "the squash"
git -C "$R" reset -q --merge
check "squash abandoned with reset --merge: edit refused again" deny "$(verdict "$R/f")"

# The one way to leave the marker behind: `git restore` puts the tree back but
# is not a reset, so SQUASH_MSG survives on a clean checkout and the guard stays
# open until the next commit clears it. Pinned here so that it is a known edge
# and not a surprise; abandon a squash with `git reset --merge`.
mkrepo squash-restore
git -C "$R" merge -q --squash clean >/dev/null 2>&1
git -C "$R" restore --staged --worktree .
check "squash abandoned with restore: guard stays open (known edge)" allow "$(verdict "$R/f")"
echo more >> "$R/f"; git -C "$R" commit -q -am next
check "...until the next commit" deny "$(verdict "$R/f")"

# --- cherry-pick and revert --------------------------------------------------
mkrepo cherry-pick
git -C "$R" cherry-pick topic >/dev/null 2>&1; conflicted "the cherry-pick"
check "cherry-pick in progress: the conflicted file can be edited" allow "$(verdict "$R/f")"
git -C "$R" cherry-pick --abort
check "cherry-pick aborted: edit refused again" deny "$(verdict "$R/f")"

mkrepo revert
echo again > "$R/f"; git -C "$R" commit -q -am again
git -C "$R" revert --no-edit HEAD~1 >/dev/null 2>&1; conflicted "the revert"
check "revert in progress: the conflicted file can be edited" allow "$(verdict "$R/f")"
git -C "$R" revert --abort
check "revert aborted: edit refused again" deny "$(verdict "$R/f")"

echo
echo "$n cases, $fails failed"
[ "$fails" -eq 0 ]
