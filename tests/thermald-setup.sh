#!/usr/bin/env bash
# Cases for system/thermald-setup, the enable-half of the thermald story that
# system/deploy runs right after its package convergence. They drive the real
# script with stub pacman/sudo/systemctl on PATH, so no case needs the package,
# the unit, root, or an Intel host, and none of them can touch this machine.
#
# What is actually being protected here, in two halves:
#
#   The gate. Which hosts want thermald is decided by ./packages.<host>, so this
#   script must do nothing whatsoever on a host that did not list it. A gate that
#   leaked would enable a unit that is not installed -- on utsuho and fujiwara
#   that is a failure on every boot, which is exactly the outcome keeping
#   thermald out of the shared list was for.
#
#   The silence. A converged host must escalate zero times. system/deploy runs on
#   every `mise run system:deploy`, and a step that sudo'd unconditionally would
#   turn the password prompt into background noise -- so "no sudo at all on the
#   no-op path" is a real assertion, not a tidiness one.
#
#   ./tests/thermald-setup.sh                 # tests system/thermald-setup
#   ./tests/thermald-setup.sh /path/to/script # tests one you already have
set -u

SCRIPT="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../system" && pwd)/thermald-setup}"
[ -x "$SCRIPT" ] || { echo "not executable: $SCRIPT" >&2; exit 1; }
echo "testing $SCRIPT"

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

# --- stubs -------------------------------------------------------------------
# Each records its argv in $D/calls and answers from a STUB_* variable, so a case
# reads as "what the host looks like" rather than as a script of mocks.
mkdir -p "$D/bin"

cat > "$D/bin/pacman" <<'EOF'
#!/usr/bin/env bash
echo "pacman $*" >> "$CALLS"
case "$1" in
-Qq) exit "${STUB_INSTALLED:-1}" ;;   # 0 = thermald installed on this host
esac
exit 0
EOF

# Not `exec "$@"` blindly: recording the sudo itself is what lets a case assert
# that the converged path never escalates at all.
cat > "$D/bin/sudo" <<'EOF'
#!/usr/bin/env bash
echo "sudo $*" >> "$CALLS"
exec "$@"
EOF

cat > "$D/bin/systemctl" <<'EOF'
#!/usr/bin/env bash
echo "systemctl $*" >> "$CALLS"
case "$1" in
is-enabled) echo "${STUB_ENABLED:-disabled}"; [ "${STUB_ENABLED:-disabled}" = enabled ] ;;
is-active)  echo "${STUB_ACTIVE:-inactive}";  [ "${STUB_ACTIVE:-inactive}" = active ] ;;
enable)     exit "${STUB_ENABLE_RC:-0}" ;;
esac
exit 0
EOF

chmod +x "$D/bin"/*

# --- runner ------------------------------------------------------------------
# want_rc, then a list of "+needle" (must appear in the call log) and "-needle"
# (must not). Env for the case comes in as leading VAR=value arguments, and
# `want_out` is matched against combined stdout+stderr when it is not empty.
run_case() { # desc want_rc [VAR=val ...] -- [+/-needle ...]
  local desc=$1 want_rc=$2; shift 2
  local env=() rc out
  while [ $# -gt 0 ] && [ "$1" != -- ]; do env+=("$1"); shift; done
  [ $# -gt 0 ] && shift

  n=$((n + 1))
  : > "$D/calls"
  out=$(env PATH="$D/bin:$PATH" CALLS="$D/calls" "${env[@]}" "$SCRIPT" 2>&1)
  rc=$?

  local bad=() needle
  [ "$rc" = "$want_rc" ] || bad+=("exit $rc, wanted $want_rc")
  for needle in "$@"; do
    case "$needle" in
    out:) [ -z "$out" ] || bad+=("wanted no output, got: $out") ;;
    +*) grep -qF -- "${needle#+}" "$D/calls" || bad+=("missing call: ${needle#+}") ;;
    -*) grep -qF -- "${needle#-}" "$D/calls" && bad+=("unwanted call: ${needle#-}") ;;
    esac
  done

  if [ ${#bad[@]} -eq 0 ]; then
    echo "ok   $desc"
  else
    fails=$((fails + 1))
    echo "FAIL $desc"
    printf '       %s\n' "${bad[@]}"
    printf '       calls: %s\n' "$(tr '\n' '; ' < "$D/calls")"
    [ -n "$out" ] && printf '       output: %s\n' "$out"
  fi
}

# --- the gate ----------------------------------------------------------------
# The host never listed thermald. Nothing may happen -- and in particular
# `systemctl` must not be reached at all, because the only thing this script
# could do with an uninstalled unit is enable a failure.
run_case "package absent: does nothing" 0 STUB_INSTALLED=1 -- \
  -"systemctl" -"sudo" +"pacman -Qq thermald"

# ...and says nothing either. Three of four hosts take this path on every single
# deploy; narrating a no-op there is how output stops being read.
run_case "package absent: says nothing" 0 STUB_INSTALLED=1 -- out:

# --- the listed host ---------------------------------------------------------
# The run that just installed it: ./deploy's pacman step put the unit on disk,
# and this is the step that makes it actually run, without a reboot.
run_case "listed, unit untouched: enables and starts" 0 \
  STUB_INSTALLED=0 STUB_ENABLED=disabled STUB_ACTIVE=inactive -- \
  +"systemctl enable --now thermald.service" +"sudo"

# The prompt-free no-op, and the reason the two is-* checks exist at all. If this
# case ever logs a sudo, every deploy starts asking for a password to do nothing.
run_case "converged: no escalation at all" 0 \
  STUB_INSTALLED=0 STUB_ENABLED=enabled STUB_ACTIVE=active -- \
  -"sudo" +"systemctl is-enabled thermald.service"

# Both halves of the check earn their keep, and they fail differently: enabled-
# but-stopped is a live host where someone ran `systemctl stop`, active-but-
# disabled is one that silently loses the daemon at the next reboot.
run_case "enabled but stopped: re-enables" 0 \
  STUB_INSTALLED=0 STUB_ENABLED=enabled STUB_ACTIVE=inactive -- \
  +"systemctl enable --now thermald.service"
run_case "running but disabled: re-enables" 0 \
  STUB_INSTALLED=0 STUB_ENABLED=disabled STUB_ACTIVE=active -- \
  +"systemctl enable --now thermald.service"

# A unit in no state systemd has a word for (masked, not-found, a systemd too old
# to answer) is not "converged" -- it must take the enable path, not the silent
# one. This is what would break if the checks were ever loosened to "not
# disabled".
run_case "unknown unit state: still tries to enable" 0 \
  STUB_INSTALLED=0 STUB_ENABLED=masked STUB_ACTIVE=failed -- \
  +"systemctl enable --now thermald.service"

# --- failure -----------------------------------------------------------------
# A failed enable has to leave as a non-zero status: that is what makes
# system/deploy's `|| echo ... continuing` print, rather than the deploy
# reporting success over a daemon that never started.
run_case "enable failure is reported" 1 \
  STUB_INSTALLED=0 STUB_ENABLED=disabled STUB_ACTIVE=inactive STUB_ENABLE_RC=1 -- \
  +"systemctl enable --now thermald.service"

echo
if [ "$fails" -eq 0 ]; then
  echo "all $n cases passed"
else
  echo "$fails of $n cases FAILED"
fi
exit $((fails > 0))
