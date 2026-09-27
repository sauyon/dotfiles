#!/usr/bin/env bash
# Cases for hyprlock-faillock (defined in home.nix), the lock-screen label that
# reports a pam_faillock lockout. They drive the real built script through its
# three override seams -- FAILLOCK_BIN (a stub reader emitting synthetic tally
# records), FAILLOCK_CONF (a throwaway faillock.conf) and FAILLOCK_USER -- so no
# real failed logins, and no waiting out a real ten-minute lockout, is needed.
#
# Every expectation here was checked against linux-pam's own pam_faillock.c and
# faillock_config.c; where a case looks surprising (nothing on screen once
# unlock_time elapses, `deny = never` keeping 3, root staying quiet without
# even_deny_root) the comment above it says which function decides that.
#
#   ./tests/hyprlock-faillock.sh                 # builds the script, then tests it
#   ./tests/hyprlock-faillock.sh /path/to/script # tests one you already have
set -u

SCRIPT="${1:-}"
if [ -z "$SCRIPT" ]; then
  host=$(cat /etc/hostname)
  drv=$(nix eval --raw ".#homeConfigurations.$host.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: p.name or "" == "hyprlock-faillock") ps)).drvPath') \
    || { echo "could not evaluate hyprlock-faillock for $host" >&2; exit 1; }
  SCRIPT="$(nix-store --realise "$drv" | tail -1)/bin/hyprlock-faillock"
  echo "testing $SCRIPT"
fi
[ -x "$SCRIPT" ] || { echo "not executable: $SCRIPT" >&2; exit 1; }
D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0

mkstub() { # records on stdin -> stub faillock binary + exit code $1
  local rc=$1; shift
  cat > "$D/records"
  cat > "$D/faillock" <<STUB
#!/bin/sh
printf '%s:\n' "$(id -un)"
printf 'When                Type  Source                                           Valid\n'
cat "$D/records"
exit $rc
STUB
  chmod +x "$D/faillock"
}

rec() { # rec <seconds-ago> <V|I>
  printf '%-19s %-5s %-52.52s %s\n' "$(date -d "@$(( $(date +%s) - $1 ))" '+%Y-%m-%d %H:%M:%S')" TTY "tty2" "$2"
}

conf() { printf '%s\n' "$@" > "$D/faillock.conf"; }

check() { # check <name> <expected>
  local name=$1 want=$2 got
  got=$(FAILLOCK_BIN="$D/faillock" FAILLOCK_CONF="$D/faillock.conf" "$SCRIPT" 2>&1)
  n=$((n+1))
  if [ "$got" = "$want" ]; then
    printf 'ok %d - %s\n' "$n" "$name"
  else
    printf 'FAIL %d - %s\n      want: [%s]\n      got:  [%s]\n' "$n" "$name" "$want" "$got"
    fails=$((fails+1))
  fi
}

conf '# all defaults commented out'

mkstub 0 </dev/null
check "no failures -> silent" ""

{ rec 30 V; rec 60 V; } | mkstub 0
check "2 of 3 -> warning" "2/3 failed attempts"

{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "3 valid, 1min ago -> locked 9 min" "Password locked out — 9 min left"

{ rec 1200 V; rec 1230 V; rec 1260 V; } | mkstub 0
check "lockout expired -> silent" ""

{ rec 30 V; rec 60 I; rec 90 V; } | mkstub 0
check "invalid records do not count" "2/3 failed attempts"

# oldest two are >fail_interval (900s) older than the newest -> out of window
{ rec 30 V; rec 60 V; rec 1500 V; rec 1530 V; } | mkstub 0
check "fail_interval window respected" "2/3 failed attempts"

conf 'deny = 5' 'unlock_time = 1800'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "conf deny=5 -> still just a warning" "3/5 failed attempts"
{ rec 60 V; rec 90 V; rec 120 V; rec 150 V; rec 180 V; } | mkstub 0
check "conf unlock_time=1800 -> 29 min left" "Password locked out — 29 min left"

conf 'unlock_time = 0'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "unlock_time=0 -> no automatic unlock" "Password locked out — no automatic unlock"

conf 'deny = 0'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "deny=0 disables faillock -> silent" ""

conf '# defaults'
mkstub 1 </dev/null
check "faillock unreadable -> silent" ""

rm -f "$D/faillock"
check "faillock missing -> silent" ""

# --- review findings (round 1) ---

conf 'unlock_time = never'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "unlock_time=never -> no automatic unlock" "Password locked out — no automatic unlock"

conf 'fail_interval = 0'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "fail_interval=0 counts nothing -> silent" ""

conf '# defaults'
{ rec 172800 V; } | mkstub 0
check "failure older than fail_interval -> silent" ""

# unlock_time (600) elapsed while still inside fail_interval (900). pam's expiry
# branch voids the whole tally on the next failure, so the budget is back and a
# count here would be a lie.
{ rec 700 V; rec 730 V; rec 760 V; } | mkstub 0
check "expired lockout -> silent, budget is back" ""

# Counted from now, not from the newest record: the 1000s-old one is outside
# fail_interval (900) of now, so pam will void it on the next failure.
{ rec 100 V; rec 1000 V; } | mkstub 0
check "warning counts only what still counts" "1/3 failed attempts"

# The default config path (no FAILLOCK_CONF): nix pam's own faillock.conf, which
# is upstream's all-commented default, so pam's built-in numbers apply.
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
n=$((n+1))
got=$(FAILLOCK_BIN="$D/faillock" "$SCRIPT" 2>&1)
if [ "$got" = "Password locked out — 9 min left" ]; then
  printf 'ok %d - default config path resolves\n' "$n"
else
  printf 'FAIL %d - default config path resolves\n      got: [%s]\n' "$n" "$got"
  fails=$((fails+1))
fi

# The username must come from a resolver that works for systemd-homed users.
{ rec 60 V; } | mkstub 0
cat > "$D/faillock" <<STUB
#!/bin/sh
printf '%s' "\$2" > "$D/got-user"
exit 1
STUB
chmod +x "$D/faillock"
FAILLOCK_BIN="$D/faillock" FAILLOCK_CONF="$D/faillock.conf" env -i \
  FAILLOCK_BIN="$D/faillock" FAILLOCK_CONF="$D/faillock.conf" "$SCRIPT" >/dev/null 2>&1
n=$((n+1))
if [ "$(cat "$D/got-user" 2>/dev/null)" = "$(id -un)" ]; then
  printf 'ok %d - username resolved in an empty environment\n' "$n"
else
  printf 'FAIL %d - username resolved in an empty environment\n      want: [%s]\n      got:  [%s]\n' \
    "$n" "$(id -un)" "$(cat "$D/got-user" 2>/dev/null)"
  fails=$((fails+1))
fi

# The default reader must exist and resolve this user; a stub cannot prove that.
n=$((n+1))
def_bin=$(sed -nE 's/^BIN="\$\{FAILLOCK_BIN:-(.*)\}"$/\1/p' "$SCRIPT")
if [ -x "$def_bin" ] && "$def_bin" --user "$(id -un)" >/dev/null 2>&1; then
  printf 'ok %d - default faillock reader runs\n' "$n"
else
  printf 'FAIL %d - default faillock reader runs\n      bin: [%s]\n' "$n" "$def_bin"
  fails=$((fails+1))
fi

# Under sudo, report the invoking user's tally, not root's empty one.
{ rec 60 V; } | mkstub 0
cat > "$D/faillock" <<STUB
#!/bin/sh
printf '%s' "\$2" > "$D/got-user"
exit 1
STUB
chmod +x "$D/faillock"
# Not running as root, so SUDO_USER names someone else (`sudo -u alice`, or a
# stale value): it must be ignored in favour of the user actually running this.
env -i SUDO_USER=somebodyelse FAILLOCK_BIN="$D/faillock" FAILLOCK_CONF="$D/faillock.conf" \
  "$SCRIPT" >/dev/null 2>&1
n=$((n+1))
if [ "$(cat "$D/got-user" 2>/dev/null)" = "$(id -un)" ]; then
  printf 'ok %d - SUDO_USER ignored when not root\n' "$n"
else
  printf 'FAIL %d - SUDO_USER ignored when not root\n      want: [%s]\n      got:  [%s]\n' \
    "$n" "$(id -un)" "$(cat "$D/got-user" 2>/dev/null)"
  fails=$((fails+1))
fi

# `never` is for the unlock times only: pam rejects `deny = never` and keeps 3.
conf 'deny = never'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "deny=never falls back to pam default 3" "Password locked out — 9 min left"

# An unreadable conf means pam built-in defaults, never the host file.
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
n=$((n+1))
got=$(FAILLOCK_BIN="$D/faillock" FAILLOCK_CONF="$D/no-such-conf" "$SCRIPT" 2>&1)
if [ "$got" = "Password locked out — 9 min left" ]; then
  printf 'ok %d - unreadable conf -> pam built-in defaults\n' "$n"
else
  printf 'FAIL %d - unreadable conf -> pam built-in defaults\n      got: [%s]\n' "$n" "$got"
  fails=$((fails+1))
fi

# root accumulates records but pam does not deny it unless the conf opts in.
conf '# defaults'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
n=$((n+1))
got=$(FAILLOCK_USER=root FAILLOCK_BIN="$D/faillock" FAILLOCK_CONF="$D/faillock.conf" "$SCRIPT" 2>&1)
if [ -z "$got" ]; then
  printf 'ok %d - root not denied without even_deny_root\n' "$n"
else
  printf 'FAIL %d - root not denied without even_deny_root\n      got: [%s]\n' "$n" "$got"
  fails=$((fails+1))
fi

conf 'even_deny_root' 'root_unlock_time = 1800'
n=$((n+1))
got=$(FAILLOCK_USER=root FAILLOCK_BIN="$D/faillock" FAILLOCK_CONF="$D/faillock.conf" "$SCRIPT" 2>&1)
if [ "$got" = "Password locked out — 29 min left" ]; then
  printf 'ok %d - root uses root_unlock_time when opted in\n' "$n"
else
  printf 'FAIL %d - root uses root_unlock_time when opted in\n      got: [%s]\n' "$n" "$got"
  fails=$((fails+1))
fi

conf '# defaults'

# pam clamps durations at MAX_TIME_INTERVAL (7 days) and keeps its default above.
conf 'unlock_time = 3600000'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "unlock_time over the clamp -> pam default 600" "Password locked out — 9 min left"

conf 'fail_interval = 999999999'
{ rec 30 V; rec 60 V; } | mkstub 0
check "fail_interval over the clamp -> pam default 900" "2/3 failed attempts"

# root_unlock_time alone does NOT opt root in: only even_deny_root sets the flag.
conf 'root_unlock_time = 900'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
n=$((n+1))
got=$(FAILLOCK_USER=root FAILLOCK_BIN="$D/faillock" FAILLOCK_CONF="$D/faillock.conf" "$SCRIPT" 2>&1)
if [ -z "$got" ]; then
  printf 'ok %d - root_unlock_time alone does not lock root\n' "$n"
else
  printf 'FAIL %d - root_unlock_time alone does not lock root\n      got: [%s]\n' "$n" "$got"
  fails=$((fails+1))
fi

# even_deny_root alone: root is deniable, and the wait falls back to unlock_time.
conf 'even_deny_root'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
n=$((n+1))
got=$(FAILLOCK_USER=root FAILLOCK_BIN="$D/faillock" FAILLOCK_CONF="$D/faillock.conf" "$SCRIPT" 2>&1)
if [ "$got" = "Password locked out — 9 min left" ]; then
  printf 'ok %d - even_deny_root alone falls back to unlock_time\n' "$n"
else
  printf 'FAIL %d - even_deny_root alone falls back to unlock_time\n      got: [%s]\n' "$n" "$got"
  fails=$((fails+1))
fi

conf '# defaults'

# pam splits a conf key at whitespace or '=', so both spellings must be honored.
conf 'unlock_time 1800'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "whitespace-separated conf key" "Password locked out — 29 min left"

conf 'unlock_time=1800'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "no-space conf key" "Password locked out — 29 min left"

# A leading zero is decimal to pam, octal to the shell.
conf 'unlock_time = 0900'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
check "leading zero read as decimal" "Password locked out — 14 min left"

# even_deny_root=1 sets the flag for pam, so it must here too.
conf 'even_deny_root=1'
{ rec 60 V; rec 90 V; rec 120 V; } | mkstub 0
n=$((n+1))
got=$(FAILLOCK_USER=root FAILLOCK_BIN="$D/faillock" FAILLOCK_CONF="$D/faillock.conf" "$SCRIPT" 2>&1)
if [ "$got" = "Password locked out — 9 min left" ]; then
  printf 'ok %d - even_deny_root=1 spelling honored\n' "$n"
else
  printf 'FAIL %d - even_deny_root=1 spelling honored\n      got: [%s]\n' "$n" "$got"
  fails=$((fails+1))
fi

conf '# defaults'

printf '\n%d/%d passed\n' "$((n-fails))" "$n"
[ "$fails" -eq 0 ]
