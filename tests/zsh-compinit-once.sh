#!/usr/bin/env bash
# Cases for the completion-dump guard in zsh.nix: the `compdef` stub in envExtra
# that stops mise from running `compinit` out of .zshenv.
#
# Why this gets a test: the failure is silent and the cost is paid on every
# shell. `mise hook-env` -- eval'd by `mise activate`, which lives in .zshenv so
# that scripts get mise's tools too -- registers per-tool completions, and to do
# that it emits
#
#     if ! (( $+functions[compdef] )); then autoload -Uz compinit; compinit -i; fi
#
# In .zshenv compdef is never defined yet, so that always fires: a full compinit
# in EVERY zsh, `zsh -c` and scripts included. Worse, it and the `compinit -u` in
# .zshrc share the default dump file while seeing different fpaths (~1000 files
# against ~2900). Each finds the other's file count in the dump header, calls the
# dump stale and rewrites it -- so neither is ever served from cache, and an
# interactive start pays for two cold compinits (0.9s measured, against 0.2s).
# Nothing errors; completion works; the shell is just slow.
#
# The guard defines a stub compdef before mise runs, so mise's check passes, and
# replays the queued calls after the real compinit. Rename that check upstream
# and nix still builds, `hms` still succeeds, and the only symptom is the lag
# coming back -- hence cases 1 and 2. Case 3 is the other direction: the stub
# must not cost the completions mise was trying to register.
#
#   ./tests/zsh-compinit-once.sh        # against the live config
#   ./tests/zsh-compinit-once.sh <dir>  # against some other generated ZDOTDIR
#
# It reads a *generated* config (the live `~/.config/zsh` by default), so it
# tests what `hms` switched in rather than what zsh.nix says. The config is
# copied into a scratch ZDOTDIR so the dumps it writes are not the real ones.
set -u

ZD="${1:-${XDG_CONFIG_HOME:-$HOME/.config}/zsh}"

skip() {
  echo "SKIP: $*"
  exit 0
}

command -v zsh >/dev/null || skip "no zsh on PATH"
command -v mise >/dev/null || skip "no mise on PATH, so nothing calls compinit from .zshenv"
[ -r "$ZD/.zshenv" ] || skip "no .zshenv in $ZD"
[ -r "$ZD/.zshrc" ] || skip "no .zshrc in $ZD"

T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT

# The generated .zshenv pins ZDOTDIR to the real directory; repoint it at the
# scratch one, or the shells under test would write the real dumps.
real=$(sed -n 's/^export ZDOTDIR="\(.*\)"$/\1/p' "$ZD/.zshenv")
[ -n "$real" ] || skip "no ZDOTDIR export in $ZD/.zshenv to repoint"
for f in "$ZD"/.[!.]* "$ZD"/*; do
  [ -e "$f" ] || continue
  case "$(basename "$f")" in
    .zcompdump*) ;;
    .zshenv | .zshrc) sed "s|$real|$T|g" "$f" >"$T/$(basename "$f")" ;;
    *) ln -s "$(readlink -f "$f")" "$T/$(basename "$f")" ;;
  esac
done

# stdin from /dev/null and no tty: an interactive zsh with neither a terminal to
# draw on nor input to wait for. p10k and gitstatus complain; that is not what
# is under test.
z() { ZDOTDIR="$T" zsh "$@" </dev/null >/dev/null 2>&1; }
dump="$T/.zcompdump"
stamp() { cksum <"$dump" 2>/dev/null; stat -c %Y.%y "$dump" 2>/dev/null; }

fail=0
check() {
  if [ "$2" = "$3" ]; then
    echo "ok   - $1"
  else
    echo "FAIL - $1"
    echo "         want: $2"
    echo "         got:  $3"
    fail=1
  fi
}

# 1. A non-interactive shell does not run compinit at all. The dump is the
#    evidence: compinit writes one whenever it finds none.
z -c exit
check "zsh -c writes no completion dump" "absent" "$([ -e "$dump" ] && echo present || echo absent)"

# 2. The dump an interactive shell writes survives other shells. Warm it, then
#    interleave a non-interactive and an interactive start: before the guard,
#    each of those rewrote it.
z -i -c exit
z -i -c exit
[ -e "$dump" ] || { echo "FAIL - interactive zsh wrote no dump at $dump"; exit 1; }
before=$(stamp)
z -c exit
check "dump untouched by a non-interactive shell" "$before" "$(stamp)"
z -i -c exit
check "dump untouched by the next interactive shell" "$before" "$(stamp)"

# 3. The stub does not eat mise's completions: every command mise tried to
#    compdef is registered once the interactive shell is up.
cmds=$(cd "$T" && env -u __MISE_DIFF -u __MISE_SESSION mise hook-env -s zsh --force 2>/dev/null |
  sed -n "s/^compdef \(_[^ ]*\) '\([^']*\)'\$/\2=\1/p" | sort -u)
if [ -z "$cmds" ]; then
  echo "skip - mise registers no completions here, nothing to replay"
else
  for pair in $cmds; do
    cmd=${pair%%=*}
    got=$(cd "$T" && ZDOTDIR="$T" zsh -i -c "print -r -- \"ZCOMP:\${_comps[$cmd]-}\"" </dev/null 2>/dev/null |
      sed -n 's/^ZCOMP://p')
    check "mise's completion for $cmd is registered" "${pair#*=}" "$got"
  done
fi

exit $fail
