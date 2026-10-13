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
# it before running tests/ (see .forgejo/workflows/nix-eval.yml).
REPO=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
host=$(cat /etc/hostname)

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

# Commits need an identity and a throwaway repo has none; nothing reads it.
export GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL= GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null

verdict() { # verdict <file path> [VAR=value ...] -> allow | deny
  local f=$1 out; shift
  out=$(printf '{"tool_input":{"file_path":"%s"}}' "$f" | env "$@" bash "$T/hook.sh")
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

# A primary checkout on master, with a branch that conflicts with it.
R="$T/repo"
git init -q -b master "$R"
echo base > "$R/f"; git -C "$R" add f; git -C "$R" commit -q -m base
git -C "$R" checkout -q -b topic
echo topic > "$R/f"; git -C "$R" commit -q -am topic
git -C "$R" checkout -q master
echo master > "$R/f"; git -C "$R" commit -q -am master

# --- the guard itself --------------------------------------------------------
check "primary checkout on master: edit refused" deny "$(verdict "$R/f")"
check "a file that does not exist yet is refused too" deny "$(verdict "$R/new/dir/g")"
check "CLAUDE_ALLOW_MAIN_EDIT=1 lets it through" allow \
  "$(verdict "$R/f" CLAUDE_ALLOW_MAIN_EDIT=1)"

git -C "$R" worktree add -q "$T/wt" topic
check "linked worktree: edit allowed" allow "$(verdict "$T/wt/f")"

# --- merges ------------------------------------------------------------------
git -C "$R" merge -q topic >/dev/null 2>&1
[ -n "$(git -C "$R" ls-files -u)" ] || { echo "fixture: the merge did not conflict" >&2; exit 1; }
check "merge in progress: the conflicted file can be edited" allow "$(verdict "$R/f")"
check "merge in progress: so can a file the merge did not touch" allow "$(verdict "$R/other")"

echo resolved > "$R/f"; git -C "$R" commit -q -am merged
check "merge concluded: edit refused again" deny "$(verdict "$R/f")"

git -C "$R" merge -q --no-commit --no-ff topic >/dev/null 2>&1 || true
git -C "$R" merge --abort 2>/dev/null
check "merge aborted: edit refused again" deny "$(verdict "$R/f")"

echo
echo "$n cases, $fails failed"
[ "$fails" -eq 0 ]
