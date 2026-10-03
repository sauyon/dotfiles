#!/usr/bin/env bash
# Cases for `hyprland-graceful-exit` (defined in home.nix, bound to
# SUPER+SHIFT+E in hyprland.nix, and used as the ExecStop of a user unit).
#
# Why this exists: the config backend is Lua, so `hyprctl dispatch <arg>` is
# evaluated as `hl.dispatch(<arg>)`. The legacy word syntax -- `closewindow
# address:0x...`, `exit` -- is a Lua *parse error* under that backend, exits 7,
# and the close loop's `|| true` swallowed it. The whole script was a no-op for
# as long as the Lua migration had been in place: press the keybind, wait five
# seconds, nothing happens, nothing logged.
#
# The other half is nastier. Hyprland validates the *dispatcher path*, not the
# selector, so a dispatch that matches no window still prints "ok" and exits 0:
#
#   hl.dsp.window.cloze({})                              -> error, rc=7
#   hl.dsp.window.close({ window = [[nonsense:zzz]] })   -> ok,    rc=0
#
# An exit status therefore cannot tell "closed it" from "did nothing", which
# makes the window count after the wait loop the only honest evidence. Without
# that check the script's failure mode is to close nothing and exit the session
# anyway, discarding every unsaved window -- strictly worse than not working.
# Ghostty alone triggers it: a surface with a live process puts up a close
# confirmation and stays mapped, so the refusal is the common path, not a
# corner case. `--force` is the way out.
#
# These cases drive the *generated* script with a stub `hyprctl` on PATH, so
# what is under test is the shell that actually ships. No compositor, and no
# lua interpreter (the CI image has bash/coreutils/jq and not much else).
#
#   ./tests/hyprland-graceful-exit.sh              # builds the script, then tests it
#   ./tests/hyprland-graceful-exit.sh /path/script # tests one you already have
set -u

# The flake ref comes from this script's own location, not `.#`, so running it
# from a worktree tests THAT tree rather than whichever happens to be the cwd.
# The host is read from /etc/hostname because the CI job writes $TEST_HOST into
# it before running tests/ (see .forgejo/workflows/nix-eval.yml) -- but it must
# be a host with a desktop, since the attribute is behind `isDesktop`.
REPO=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

SCRIPT="${1:-}"
if [ -z "$SCRIPT" ]; then
  host=$(cat /etc/hostname)
  # build, not eval: the attribute is a derivation, so evaluating it yields a
  # store path that does not exist yet.
  SCRIPT=$(nix build --no-link --print-out-paths \
    "$REPO#homeConfigurations.$host.config.home.file.\".local/bin/hyprland-graceful-exit\".source") \
    || { echo "could not build hyprland-graceful-exit for $host" >&2; exit 1; }
  echo "testing $SCRIPT"
fi
[ -r "$SCRIPT" ] || { echo "not readable: $SCRIPT" >&2; exit 1; }

# The script calls jq by store path. `nix build` above realises it as a
# reference; a hand-passed script from another machine may not have it.
jqpath=$(grep -o '/nix/store/[^/]*/bin/jq' "$SCRIPT" | head -1)
if [ -n "$jqpath" ] && [ ! -x "$jqpath" ]; then
  echo "the script's jq is not in this store: $jqpath" >&2; exit 1
fi
command -v jq >/dev/null 2>&1 || { echo "need jq on PATH for the stub" >&2; exit 1; }

T=$(mktemp -d); trap 'rm -rf "$T"' EXIT
fails=0; n=0

# The stub. `clients -j` renders whatever addresses are left in $GE_TESTDIR/open;
# `dispatch` appends the raw Lua expression to .../trace and, for a close whose
# selector names an open and non-sticky address, drops that address -- which is
# what makes "the selector actually selected something" observable at all.
# Anything not starting with `hl.` exits 7, exactly as the real hyprctl does
# with legacy word syntax, so a regression to the old form fails here.
#
# The `blind` flag makes `clients` print nothing at all, which is how the
# script's `count` goes empty: that is the input that used to walk into
# `set -u` and abort before the notification the branch exists for.
mkdir -p "$T/bin"
cat > "$T/bin/hyprctl" <<'STUB'
#!/usr/bin/env bash
set -u
case "${1:-}" in
  clients)
    # Count every clients call, so a case can fail only the LATE ones.
    calls=$(cat "$GE_TESTDIR/calls" 2>/dev/null || echo 0)
    calls=$((calls + 1)); printf '%s' "$calls" > "$GE_TESTDIR/calls"
    [ -e "$GE_TESTDIR/blind" ] && exit 0
    if [ -e "$GE_TESTDIR/bust" ]; then
      echo "Couldn't write to socket. Error: Connection refused." >&2; exit 1
    fi
    if [ -s "$GE_TESTDIR/failfrom" ] \
       && [ "$calls" -ge "$(cat "$GE_TESTDIR/failfrom")" ]; then
      echo "Couldn't write to socket. Error: Connection refused." >&2; exit 1
    fi
    jq -R -s 'split("\n") | map(select(length > 0))
              | map({address: ., class: "stub", title: ("win " + .)})' \
       < "$GE_TESTDIR/open"
    ;;
  dispatch)
    expr="${2:-}"
    printf '%s\n' "$expr" >> "$GE_TESTDIR/trace"
    case "$expr" in
      hl.*) ;;
      *) echo "error: ')' expected near '${expr%% *}'" >&2; exit 7 ;;
    esac
    case "$expr" in
      hl.dsp.window.close*)
        addr=${expr#*address:}; addr=${addr%%]]*}
        # closefail models a dispatch that errors outright (a renamed Lua API),
        # as distinct from sticky, which models a window that declines to go.
        if grep -qxF "$addr" "$GE_TESTDIR/closefail" 2>/dev/null; then
          echo "error: no such dispatcher" >&2; exit 7
        fi
        if [ -n "$addr" ] && ! grep -qxF "$addr" "$GE_TESTDIR/sticky" 2>/dev/null; then
          grep -vxF "$addr" "$GE_TESTDIR/open" > "$GE_TESTDIR/open.tmp" || :
          mv "$GE_TESTDIR/open.tmp" "$GE_TESTDIR/open"
        fi
        ;;
    esac
    echo ok
    ;;
  *) echo "stub hyprctl: unhandled: $*" >&2; exit 64 ;;
esac
STUB
chmod +x "$T/bin/hyprctl"

# Keep the refusal path from firing a real desktop notification on a dev box --
# and record the argv, because the message body is the one thing in this change
# whose rendering depends on Nix's common-indentation stripping.
cat > "$T/bin/notify-send" <<'NOTIFY'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$GE_TESTDIR/notify"
NOTIFY
chmod +x "$T/bin/notify-send"

# The wait loop sleeps 10 x 0.5s on every refusal case. Real time buys nothing
# here, so shadow it; the loop's iteration count is what the cases care about.
printf '#!/usr/bin/env bash\nexit 0\n' > "$T/bin/sleep"
chmod +x "$T/bin/sleep"

# run <open> <sticky> [args...]; sets $rc, $trace, $err and $notify. As <open>,
# the literal "blind" means `hyprctl clients` succeeds but prints nothing, and
# "bust" means it fails outright; anything else is the newline-separated list
# of open window addresses.
# Per-case stub setup, set before a call and consumed by it. These have to be
# applied inside run(), not after it: run() resets the stub's call counter and
# window list, so writing them afterwards and re-invoking by hand tests a
# half-mutated stub (which is exactly how the first version of these cases
# produced three confusing failures).
#   FAILFROM  - clients call number from which hyprctl starts failing
#   CLOSEFAIL - newline-separated addresses whose close dispatch errors
FAILFROM=; CLOSEFAIL=

run() {
  rm -f "$T/blind" "$T/bust" "$T/notify" "$T/failfrom" "$T/closefail" "$T/calls"
  [ -n "$FAILFROM" ]  && printf '%s' "$FAILFROM"  > "$T/failfrom"
  [ -n "$CLOSEFAIL" ] && printf '%s' "$CLOSEFAIL" > "$T/closefail"
  FAILFROM=; CLOSEFAIL=
  case "$1" in
    blind) : > "$T/open"; : > "$T/blind" ;;
    bust)  : > "$T/open"; : > "$T/bust"  ;;
    *)     printf '%s' "$1" > "$T/open"  ;;
  esac
  printf '%s' "$2" > "$T/sticky"
  : > "$T/trace"
  shift 2
  err=$(GE_TESTDIR="$T" PATH="$T/bin:$PATH" bash "$SCRIPT" "$@" 2>&1 >/dev/null); rc=$?
  trace=$(tr '\n' '|' < "$T/trace")
  notify=$(cat "$T/notify" 2>/dev/null || :)
}

check() {
  local name=$1 want=$2 got=$3
  n=$((n + 1))
  if [ "$got" = "$want" ]; then
    printf 'ok %d - %s\n' "$n" "$name"
  else
    printf 'FAIL %d - %s\n      want: [%s]\n      got:  [%s]\n' "$n" "$name" "$want" "$got"
    fails=$((fails + 1))
  fi
}

# contains <haystack> <needle> -> yes/no, so the checks read as data.
has() { case "$1" in *"$2"*) echo yes ;; *) echo no ;; esac; }

# hasline <haystack> <exact line> -> yes/no. Line-exact, so leading whitespace
# is a failure rather than a near-miss -- which is the point for the dedent.
hasline() { printf '%s\n' "$1" | grep -qxF "$2" && echo yes || echo no; }

# The happy path, and the regression guard: every dispatch must be a Lua
# expression. A revert to `closewindow address:0x1` makes the stub exit 7.
run '0x1
0x2
' ''
check "closes every window by address, then exits" \
  'hl.dsp.window.close({ window = [[address:0x1]] })|hl.dsp.window.close({ window = [[address:0x2]] })|hl.dsp.exit()|' \
  "$trace"
check "happy path succeeds" 0 "$rc"

# The bug the count guard exists for: a window that stays mapped must stop the
# exit. Before the guard this trace ended in hl.dsp.exit() regardless.
run '0x1
0x2
' '0x2
'
check "does not exit while a window refuses to close" no "$(has "$trace" hl.dsp.exit)"
check "refusal is a failure, not a silent no-op" 1 "$rc"
check "refusal says so on stderr" yes "$(has "$err" 'refused to close')"
check "refusal names the window, not just a count" yes "$(has "$err" 'stub: win 0x2')"
check "refusal notifies, since stderr goes to a log nobody reads" yes \
  "$(has "$notify" 'refused to close')"
# The one assertion that mechanically checks Nix's dedent: this line is flush
# left in home.nix on purpose, and must arrive at column 0, not indented.
check "notification offers --force at column 0 (Nix dedent)" yes \
  "$(hasline "$notify" 'hyprland-graceful-exit --force')"
check "notification body carries the window list too" yes \
  "$(has "$notify" 'stub: win 0x2')"

# The escape hatch. Ghostty prompts for any surface with a live process, so
# without this the bind could never log out of a session with a busy terminal.
run '0x1
0x2
' '0x2
' --force
check "--force exits despite the refusal" yes "$(has "$trace" 'hl.dsp.exit()')"
check "--force succeeds" 0 "$rc"

# Nothing open is the one case where exiting immediately is right.
run '' ''
check "exits straight away with no windows open" 'hl.dsp.exit()|' "$trace"

# ExecStop of the systemd user unit passes --no-exit: close the windows, leave
# the compositor up, and never fail the unit mid-shutdown.
run '0x1
' '' --no-exit
check "--no-exit closes without exiting" \
  'hl.dsp.window.close({ window = [[address:0x1]] })|' "$trace"
check "--no-exit succeeds" 0 "$rc"

run '0x1
' '0x1
' --no-exit
check "--no-exit does not fail the unit when a window stays" 0 "$rc"
check "--no-exit still reports the refusal" yes "$(has "$err" 'refused to close')"

# `count` goes empty when hyprctl prints nothing. The seed-and-clamp has to
# turn that into a refusal; unguarded, `set -u` aborted here *before* the
# notification, which is the one path where stderr is never read.
run blind ''
check "empty window count does not abort on an unbound variable" no \
  "$(has "$err" 'unbound variable')"
check "empty window count is treated as 'still open'" no "$(has "$trace" hl.dsp.exit)"
check "empty window count refuses rather than exiting" 1 "$rc"
# Without this the trio above passes just as well against a script that aborted
# at the first pipeline and printed nothing at all.
check "empty window count still tells the user" yes "$(has "$err" 'not exiting')"
check "empty window count does not invent a window count" no \
  "$(has "$err" '1 window(s)')"

# The realistic degenerate case: hyprctl does not merely go quiet, it fails.
# Under `pipefail` an unguarded pipeline would abort here -- before any
# message, before the notification -- which is the silent failure this whole
# change exists to remove.
run bust ''
check "a failing hyprctl does not abort the script silently" yes \
  "$(has "$err" 'not exiting')"
check "a failing hyprctl refuses rather than exiting" no "$(has "$trace" hl.dsp.exit)"
check "a failing hyprctl still notifies" yes "$(has "$notify" 'could not tell')"
check "a failing hyprctl exits 1" 1 "$rc"

# `bust` fails EVERY clients call, so it lands in the count_known=no arm and
# never reaches the third guarded call. This is the transition that does: the
# count succeeds (so count_known=yes), and the later call that builds the
# window list fails. Unguarded, that assignment aborts the script between
# deciding to refuse and saying so.
#
# 12 = 1 address probe + 10 wait-loop polls, so the first failure lands on the
# window-list call. That number is a coupling to the script's structure, and
# the first check below asserts it: if the loop ever SHRINKS, FAILFROM would
# never fire and the other two checks would quietly degenerate into a copy of
# the plain-refusal case above, still passing. Asserting the call count is
# what stops this case going vacuous without anyone noticing.
FAILFROM=12
run '0x1
' '0x1
'
check "the window-list call is call 12 (pins FAILFROM to the script's shape)" 12 \
  "$(cat "$T/calls")"
# ...and this one proves the injection actually fired. The count assertion
# above cannot: with FAILFROM too high the script still makes 12 calls, they
# just all succeed, and the rest of the case degenerates into a duplicate of
# the plain-refusal case while still passing.
check "the injected late failure actually fired" yes \
  "$(has "$err" "Couldn't write to socket")"
check "a late hyprctl failure still reaches the refusal message" yes \
  "$(has "$err" 'refused to close')"
check "a late hyprctl failure still notifies" yes "$(has "$notify" 'refused to close')"
check "a late hyprctl failure exits 1" 1 "$rc"

# The close dispatch failing outright -- a renamed Lua dispatcher -- must be
# reported per window and must not abort the loop under `set -e`.
CLOSEFAIL='0x1
'
run '0x1
0x2
' ''
check "a failing close dispatch is reported, not swallowed" yes \
  "$(has "$err" 'close dispatch failed for 0x1')"
check "a failing close dispatch does not abort the loop" yes \
  "$(has "$trace" 'address:0x2')"

# The ExecStop path must be as honest as the keybind path: with no usable
# count it has to say so rather than report the clamped 1 as fact.
run bust '' --no-exit
check "--no-exit does not invent a window count" no "$(has "$err" '1 window(s)')"
check "--no-exit says what it actually knows" yes "$(has "$err" 'could not tell')"
check "--no-exit still never fails the unit" 0 "$rc"

# An unrecognised flag used to fall through to the exit branch.
run '0x1
' '' --noexit
check "a typo'd flag is rejected, not treated as --no-exit" 2 "$rc"
check "a typo'd flag dispatches nothing" '' "$trace"

printf '\n%d case(s), %d failure(s)\n' "$n" "$fails"
[ "$fails" -eq 0 ]
