#!/usr/bin/env bash
# Cases for the ghostty/powerlevel10k prompt-corruption guard: the line in
# zsh.nix that primes `_ghostty_saved_ps1` so ghostty's zsh integration skips its
# newline-marking pass.
#
# Why this one line gets a test when the rest of the prompt does not: its logic
# is a model of somebody else's code. It works by pre-setting variables private
# to ghostty's `ghostty-integration`, so that script's own `ps1_changed` guard
# fires on the first precmd instead of missing it. A comment asserting that is
# worth nothing -- rename those variables upstream and nix still builds, `hms`
# still succeeds, and the only symptom is a literal `}}` before the prompt char.
#
# The mechanism: ghostty marks continuation lines by rewriting PS1 as text,
# `PS1=${PS1//$'\n'/$'\n'${mark2}}`. Under PROMPT_SUBST a prompt is code, not
# text -- p10k's PROMPT is one nested parameter expansion holding newlines as
# data inside `${...}` -- so the `}` in the spliced mark closes an expansion a
# level early and the braces after it fall out as literal text. Upstream still
# does this on `main`: PR #11596 renamed the mark but kept both the substitution
# and the guard, so the bug and this workaround both outlive it.
#
# WHICH PATH THE BUG LIVES ON, because it is not both and the difference is the
# whole reason this file is careful:
#   - plain:    the integration arrives only via the `source .../
#     ghostty-integration` line home-manager writes into .zshrc, which runs after
#     p10k. Its precmd is therefore last, it takes the PS1-rewriting branch, and
#     the `}}` appears. THIS is the path the artifact was reported on.
#   - injected: ghostty points ZDOTDIR at its own directory, whose .zshenv
#     sources the user's and then the integration -- before .zshrc, so before
#     p10k. Its precmd is not last, it prints marks directly instead of
#     rewriting PS1, and the `}}` never happens. The priming is inert here.
# Both are covered: the plain cases prove the guard works, the injected ones
# prove it costs nothing where it was not needed.
#
# Driving the injected path is easy to get wrong, and getting it wrong is silent.
# script(1) runs its `-c` command through $SHELL. When that is zsh, the OUTER
# non-interactive zsh reads ghostty's .zshenv first -- rewriting ZDOTDIR and
# unsetting GHOSTTY_ZSH_ZDOTDIR, while skipping the integration, whose `always`
# block gates on the shell being interactive -- so the inner `zsh -i` never gets
# the handoff and silently runs the plain path instead. Hence SHELL=/bin/sh, and
# hence the `injection really happens` case below, which fails loudly if that
# ever stops being true rather than letting two cases quietly test one path.
#
#   ./tests/ghostty-p10k-prompt.sh        # drives the cases against the live config
#   ./tests/ghostty-p10k-prompt.sh <dir>  # against some other ZDOTDIR
#
# It reads a *generated* config (the live `~/.config/zsh` by default), so it
# tests what `hms` actually switched in rather than what zsh.nix says. Before the
# first switch carrying the fix it skips rather than fails, and says why.
set -u

ZD="${1:-${XDG_CONFIG_HOME:-$HOME/.config}/zsh}"

# --- preconditions ------------------------------------------------------------
# Skips, not failures: none of these means the guard is broken, and a red suite
# for "ghostty is not installed here" trains people to ignore the suite.
skip() {
  echo "SKIP: $*"
  exit 0
}

GRD="${GHOSTTY_RESOURCES_DIR:-}"
if [ -z "$GRD" ]; then
  # Not running under ghostty; find the resources dir next to the binary.
  gb=$(command -v ghostty) || skip "no ghostty on PATH and GHOSTTY_RESOURCES_DIR unset"
  GRD="$(dirname "$(dirname "$(readlink -f "$gb")")")/share/ghostty"
fi
GZ="$GRD/shell-integration/zsh"
[ -r "$GZ/ghostty-integration" ] || skip "no ghostty zsh integration under $GZ"
[ -r "$GZ/.zshenv" ] || skip "no injected .zshenv under $GZ"
[ -r "$ZD/.zshrc" ] || skip "no .zshrc in $ZD"
grep -q 'powerlevel10k' "$ZD/.zshrc" || skip "$ZD/.zshrc does not load powerlevel10k"
grep -q 'typeset -g _ghostty_saved_ps1' "$ZD/.zshrc" ||
  skip "$ZD/.zshrc has no _ghostty_saved_ps1 priming -- run hms first (zsh.nix carries it)"
grep -q 'ghostty-integration' "$ZD/.zshrc" ||
  skip "$ZD/.zshrc does not source ghostty-integration (enableZshIntegration off?)"
command -v script >/dev/null || skip "util-linux script(1) not available"
[ -x /bin/sh ] || skip "no /bin/sh to keep script(1) off zsh"

D=$(mktemp -d) || exit 1
trap 'rm -rf "$D"' EXIT

# --- the config variants ------------------------------------------------------
# `with` is the live config verbatim; `without` is the same config minus the
# priming; `nosrc` keeps the priming but neutralises the .zshrc source line, so
# the only way the integration can load is the injected .zshenv. Deriving all
# three from the live config is what keeps this honest -- it cannot pass by
# failing to exercise the bug, because a case below asserts the corruption IS
# there once the priming is gone.
for v in with without nosrc; do
  mkdir -p "$D/$v"
  for f in .zshrc .zshenv .p10k.zsh; do
    if [ -r "$ZD/$f" ]; then
      cp "$ZD/$f" "$D/$v/$f"
      chmod u+w "$D/$v/$f"
    fi
  done
  # Repoint ZDOTDIR and the p10k config at the copy, or it would load the
  # originals and the variants would collapse into one.
  if [ -r "$D/$v/.zshenv" ]; then
    sed -i -e "s|^export ZDOTDIR=.*|export ZDOTDIR=\"$D/$v\"|" "$D/$v/.zshenv"
  fi
  sed -i -e "s|~/.config/zsh/.p10k.zsh|$D/$v/.p10k.zsh|g" "$D/$v/.zshrc"
done
sed -i -e '/typeset -g _ghostty_saved_ps1/d' "$D/without/.zshrc"
# shellcheck disable=SC2016 # the $ is literal: it is in the .zshrc being matched
sed -i -e 's|^  source "\$GHOSTTY_RESOURCES_DIR"/shell-integration/zsh/ghostty-integration$|  :|' \
  "$D/nosrc/.zshrc"

# A harness that stopped differentiating the variants would report "all passed"
# forever, so check the setup itself before trusting a single case.
setup_bug() {
  echo "harness bug: $*" >&2
  exit 1
}
grep -q 'typeset -g _ghostty_saved_ps1' "$D/with/.zshrc" ||
  setup_bug "priming missing from the 'with' copy"
! grep -q 'typeset -g _ghostty_saved_ps1' "$D/without/.zshrc" ||
  setup_bug "priming survived in the 'without' copy"
# shellcheck disable=SC2016 # the $ is literal: it is in the .zshrc being matched
! grep -q 'source "\$GHOSTTY_RESOURCES_DIR"' "$D/nosrc/.zshrc" ||
  setup_bug "source line survived in the 'nosrc' copy"
grep -q "$D/with/.p10k.zsh" "$D/with/.zshrc" ||
  setup_bug "p10k path in the 'with' copy still points outside the temp dir"

# --- runner -------------------------------------------------------------------
# A pty is mandatory: p10k renders nothing recognisable on a pipe, and the
# artifact only exists in a real prompt render. Six commands, so a regression
# that only shows after the first prompt still lands.
render() { # <variant> <injected|plain> [zsh-command]  -> pty output, ctrl chars visible
  local v=$1 mode=$2 cmd=${3:-}
  # shellcheck disable=SC2054 # the commas are inside one env value, not separators
  local -a e=(
    "GHOSTTY_RESOURCES_DIR=$GRD"
    GHOSTTY_SHELL_FEATURES=cursor:blink,path,title
    SHELL=/bin/sh # keep script(1) from running the command through zsh
  )
  if [ "$mode" = injected ]; then
    e+=("ZDOTDIR=$GZ" "GHOSTTY_ZSH_ZDOTDIR=$D/$v")
  else
    e+=("ZDOTDIR=$D/$v")
  fi
  if [ -z "$cmd" ]; then
    cmd=$(printf 'true\ncd /tmp\ntrue\nprint hi\ncd -\nexit\n')
  fi
  printf '%s\n' "$cmd" | env "${e[@]}" script -qc 'zsh -i' /dev/null 2>&1 | cat -v
}

count() { grep -oE "$1" | wc -l | tr -d ' '; } # <grep -oE pattern>, text on stdin

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
  local b
  b=$(render "$2" "$3" | count '\}\}')
  case "$4" in
  eq0) if [ "$b" -eq 0 ]; then ok "$1"; else bad "$1 (expected no '}}', got $b)"; fi ;;
  gt0) if [ "$b" -gt 0 ]; then ok "$1"; else
    bad "$1 (expected '}}', got $b -- this case no longer exercises the bug)"
  fi ;;
  esac
}

# --- the injected mode is really injected -------------------------------------
# First, because every injected case below is meaningless if it silently ran the
# plain path instead. `nosrc` cannot load the integration any other way, so a set
# `_ghostty_state` proves the ZDOTDIR handoff happened.
# shellcheck disable=SC2016 # ${+...} must reach zsh unexpanded; this is bash
probe='print -r -- "GS=${+_ghostty_state}"; exit'
if [ "$(render nosrc injected "$probe" | count 'GS=1')" -gt 0 ]; then
  ok "injected mode really injects (integration loads with no source line)"
else
  bad "injected mode did NOT inject -- the injected cases below are testing the plain path"
fi
if [ "$(render nosrc plain "$probe" | count 'GS=0')" -gt 0 ]; then
  ok "plain mode really does not inject (no integration without the source line)"
else
  bad "plain mode loaded the integration without the source line -- modes are not distinct"
fi

# --- the guard holds on the path the bug lives on -----------------------------
braces "plain:    no '}}' with the priming" with plain eq0
braces "plain:    '}}' returns without the priming" without plain gt0

# --- and costs nothing on the path it was never needed on ---------------------
# Injected loads the integration before p10k, so its precmd is not last, it never
# rewrites PS1, and there is no artifact to remove. Both cases are eq0: if the
# second ever goes gt0, the load order changed upstream and the plain-path
# reasoning above needs revisiting.
braces "injected: no '}}' with the priming" with injected eq0
braces "injected: no '}}' without it either (nothing to fix here)" without injected eq0

# --- nothing else changes -----------------------------------------------------
# The point of priming the guard rather than dropping ghostty's integration
# (which also silences the `}}`, at the cost of the cursor sequences and half the
# title writes) is that everything else still works. Each probe must match the
# same number of times in both variants AND be non-zero: without that floor a
# probe that stopped matching anything would read as "unchanged (0)", which is
# the exact vacuity these cases exist to rule out.
inj_with=$(render with plain)
inj_without=$(render without plain)
for probe in '133;A;cl=line' '133;B' '133;C' '133;D' '\^\[\[[0-9] q' '\^\[\]2;' 'kitty-shell-cwd'; do
  a=$(printf '%s' "$inj_with" | count "$probe")
  b=$(printf '%s' "$inj_without" | count "$probe")
  if [ "$a" -eq 0 ]; then
    bad "plain: '$probe' matched nothing -- the probe is stale, not the code"
  elif [ "$a" = "$b" ]; then
    ok "plain: '$probe' count unchanged by the priming ($a)"
  else
    bad "plain: '$probe' count changed by the priming (with=$a without=$b)"
  fi
done

echo
if [ "$fails" -eq 0 ]; then
  echo "all $n cases passed"
else
  echo "$fails of $n cases FAILED"
fi
exit $((fails > 0))
