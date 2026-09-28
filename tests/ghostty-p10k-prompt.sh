#!/usr/bin/env bash
# Cases for the ghostty/powerlevel10k prompt-corruption guard: the line in
# zsh.nix that primes `_ghostty_saved_ps1` so ghostty's zsh integration skips its
# newline-marking pass.
#
# Why this one line gets a test when the rest of the prompt does not: its logic
# is a model of somebody else's code. It works by pre-setting variables private
# to ghostty's `ghostty-integration`, so that script's own `ps1_changed` guard
# fires on the first precmd instead of missing it. A comment asserting that is
# worth nothing -- if upstream renames those variables or changes the `${var+x}`
# gate the guard reads, nix still builds, `hms` still succeeds, and the only
# symptom is a literal `}}` reappearing before the prompt char.
#
# The mechanism being guarded: ghostty marks continuation lines by rewriting PS1
# as text, `PS1=${PS1//$'\n'/$'\n'${mark2}}`. Under PROMPT_SUBST a prompt is
# code, not text -- p10k's PROMPT is one nested parameter expansion holding
# newlines as data inside `${...}` -- so the `}` in the spliced mark closes an
# expansion a level early and the braces after it fall out as literal text.
# Upstream still does this on `main`: PR #11596 renamed the mark from
# `133;A;k=s` to `133;P;k=s` but kept both the substitution and the guard, so the
# bug and this workaround both outlive it.
#
#   ./tests/ghostty-p10k-prompt.sh        # drives the cases against the live config
#   ./tests/ghostty-p10k-prompt.sh <dir>  # against some other ZDOTDIR
#
# It reads a *generated* config (the live `~/.config/zsh` by default), so it
# tests what `hms` actually switched in rather than what zsh.nix says. Before the
# first switch carrying the fix it skips rather than fails, and says why.
#
# Both startup paths are covered, because they are genuinely different and only
# one of them is obvious:
#   - injected: ghostty spawns the shell with ZDOTDIR pointed at its own
#     directory, whose .zshenv sources the user's and then the integration.
#   - plain: no injection, and the integration arrives only via the
#     `source .../ghostty-integration` line home-manager writes into .zshrc.
set -u

ZD="${1:-${XDG_CONFIG_HOME:-$HOME/.config}/zsh}"

# --- preconditions ------------------------------------------------------------
# Every one of these is a skip, not a failure: none of them means the guard is
# broken, and a red suite for "ghostty is not installed here" trains people to
# ignore the suite.
skip() { echo "SKIP: $*"; exit 0; }

GRD="${GHOSTTY_RESOURCES_DIR:-}"
if [ -z "$GRD" ]; then
  # Not running under ghostty; find the resources dir next to the binary.
  gb=$(command -v ghostty) || skip "no ghostty on PATH and GHOSTTY_RESOURCES_DIR unset"
  GRD="$(dirname "$(dirname "$(readlink -f "$gb")")")/share/ghostty"
fi
GZ="$GRD/shell-integration/zsh"
[ -r "$GZ/ghostty-integration" ] || skip "no ghostty zsh integration under $GZ"
[ -r "$ZD/.zshrc" ] || skip "no .zshrc in $ZD"
grep -q 'powerlevel10k' "$ZD/.zshrc" || skip "$ZD/.zshrc does not load powerlevel10k"
grep -q '_ghostty_saved_ps1' "$ZD/.zshrc" ||
  skip "$ZD/.zshrc has no _ghostty_saved_ps1 priming -- run hms first (zsh.nix carries it)"
command -v script >/dev/null || skip "util-linux script(1) not available"

D=$(mktemp -d) || exit 1
trap 'rm -rf "$D"' EXIT

# --- the two configs ----------------------------------------------------------
# `with` is the live config verbatim; `without` is the same config with the
# priming deleted. Deriving the negative case from the positive one is what keeps
# this honest -- it cannot pass by failing to exercise the bug, because the
# `without` cases assert the corruption IS there once the line is gone.
for v in with without; do
  mkdir -p "$D/$v"
  for f in .zshrc .zshenv .p10k.zsh; do
    if [ -r "$ZD/$f" ]; then cp "$ZD/$f" "$D/$v/$f" && chmod u+w "$D/$v/$f"; fi
  done
  # Repoint ZDOTDIR and the p10k config at the copy, or the copy would load the
  # originals and both variants would end up identical.
  if [ -r "$D/$v/.zshenv" ]; then
    sed -i "s|^export ZDOTDIR=.*|export ZDOTDIR=\"$D/$v\"|" "$D/$v/.zshenv"
  fi
  sed -i "s|~/.config/zsh/.p10k.zsh|$D/$v/.p10k.zsh|g" "$D/$v/.zshrc"
done
sed -i '/typeset -g _ghostty_saved_ps1/d' "$D/without/.zshrc"

# A harness that silently stopped differentiating the two configs would report
# "all passed" forever, so check the setup itself before trusting any case.
grep -q 'typeset -g _ghostty_saved_ps1' "$D/with/.zshrc" ||
  { echo "harness bug: priming missing from the 'with' copy" >&2; exit 1; }
if grep -q 'typeset -g _ghostty_saved_ps1' "$D/without/.zshrc"; then
  echo "harness bug: priming survived in the 'without' copy" >&2; exit 1
fi

# --- runner -------------------------------------------------------------------
# A pty is mandatory: p10k renders nothing recognisable on a pipe, and the
# artifact only exists in a real prompt render. Six commands, so a regression
# that only shows after the first prompt still lands.
render() { # <with|without> <injected|plain>  -> raw pty output, control chars visible
  local v=$1 mode=$2
  # shellcheck disable=SC2054 # the commas are inside one env value, not separators
  local -a e=(GHOSTTY_SHELL_FEATURES=cursor:blink,path,title)
  if [ "$mode" = injected ]; then
    e+=(ZDOTDIR="$GZ" GHOSTTY_ZSH_ZDOTDIR="$D/$v")
  else
    e+=(ZDOTDIR="$D/$v")
  fi
  printf 'true\ncd /tmp\ntrue\nprint hi\ncd -\nexit\n' |
    env "${e[@]}" script -qc 'zsh -i' /dev/null 2>&1 | cat -v
}

count() { grep -o "$1" | wc -l | tr -d ' '; } # <grep -o pattern>, text on stdin

n=0
fails=0
ok() {
  n=$((n + 1))
  echo "  ok    $*"
}
bad() {
  n=$((n + 1))
  fails=$((fails + 1))
  echo "  FAIL  $*"
}

braces() { # <desc> <variant> <mode> <eq0|gt0>
  local out b
  out=$(render "$2" "$3")
  b=$(printf '%s' "$out" | count '}}')
  case "$4" in
  eq0) if [ "$b" -eq 0 ]; then ok "$1"; else bad "$1 (expected no '}}', got $b)"; fi ;;
  gt0) if [ "$b" -gt 0 ]; then ok "$1"; else
    bad "$1 (expected '}}' to appear, got $b -- these cases no longer exercise the bug)"
  fi ;;
  esac
}

# --- the guard holds ----------------------------------------------------------
braces "injected: no '}}' with the priming" with injected eq0
braces "plain:    no '}}' with the priming" with plain eq0

# --- the cases above still mean something -------------------------------------
# If either of these goes quiet, upstream changed something and the two cases
# above have stopped testing anything. That is a failure, not a pass.
braces "injected: '}}' returns without the priming" without injected gt0
braces "plain:    '}}' returns without the priming" without plain gt0

# --- the one thing the priming does cost --------------------------------------
# Skipping the newline pass means the continuation-line mark it splices never
# gets emitted. Exactly one does reach the terminal without the priming, on the
# first prompt, so this is a real if small trade and not a no-op -- assert it
# rather than let a parity probe quietly average it away. `[AP]` because upstream
# renamed this mark from `133;A;k=s` to `133;P;k=s` in PR #11596: on a ghostty
# that predates the rename it is the A spelling, after it the P one, and this
# case should hold across the bump either way.
mark2_with=$(render with injected | count '133;[AP];k=s')
mark2_without=$(render without injected | count '133;[AP];k=s')
if [ "$mark2_with" -eq 0 ]; then
  ok "injected: continuation-line mark suppressed with the priming (0)"
else
  bad "injected: expected no continuation-line mark with the priming, got $mark2_with"
fi
if [ "$mark2_without" -gt 0 ]; then
  ok "injected: continuation-line mark present without it ($mark2_without) -- the trade is real"
else
  bad "injected: expected a continuation-line mark without the priming, got $mark2_without"
fi

# --- nothing else is lost -----------------------------------------------------
# The point of priming the guard rather than dropping ghostty's integration
# (which also silences the `}}`, at the cost of the cursor sequences and half the
# title writes) is that everything else still works. Compare the variants mark
# for mark. `133;A;cl=line` rather than a bare `133;A`: the latter also matches
# the continuation-line mark above on a pre-#11596 ghostty, which made this probe
# read as a regression when it was measuring the intended difference.
inj_with=$(render with injected)
inj_without=$(render without injected)
for probe in '133;A;cl=line' '133;B' '133;C' '133;D' '\^\[\[[0-9] q' '\^\[\]2;' 'file://'; do
  a=$(printf '%s' "$inj_with" | count "$probe")
  b=$(printf '%s' "$inj_without" | count "$probe")
  if [ "$a" = "$b" ]; then
    ok "injected: '$probe' count unchanged by the priming ($a)"
  else
    bad "injected: '$probe' count changed by the priming (with=$a without=$b)"
  fi
done

echo
if [ "$fails" -eq 0 ]; then
  echo "all $n cases passed"
else
  echo "$fails of $n cases FAILED"
fi
exit $((fails > 0))
