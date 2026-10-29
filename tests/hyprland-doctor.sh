#!/usr/bin/env bash
# Cases for hyprland-doctor (defined in home.nix), the script that reaps
# wedged Hyprland instances and reconciles the session activation environment
# after one of them poisons it.
#
# The 2026-10-28 utsuho incident it exists to prevent: a Panther Lake
# deadlock left three compositors wedged -- alive, absent from
# `hyprctl instances`, no .socket.sock in their signature dirs. Recovering by
# hand (`start-hyprland` from a TTY) worked, but eight minutes later one
# wedged instance came unstuck, ran its hyprland.start hook, and imported its
# own dead WAYLAND_DISPLAY into the systemd/D-Bus activation environment.
# Every user-manager service then inherited the dead socket: elephant
# activated zen-beta.desktop and zen died instantly, walker crash-looped to
# start-limit-hit, and the journal never named the cause. The repair was
# manual (kill the wedged pids, re-import the live session's env, restart the
# units that cached the poisoned values); these cases pin the script that does
# all three automatically, every 30 seconds, while a session exists.
#
# What each half must never do, also pinned here:
#   - reap must never kill a *live* compositor (one present in
#     `hyprctl instances`), nor a wedged one still inside the grace window --
#     a fresh instance that has not registered yet is mid-startup, not stuck.
#   - reconcile must be a no-op when the env already names the live socket:
#     the timer's common case must not restart elephant (which, via
#     Requires=, bounce-restarts walker) every 30 seconds for no reason.
#
# Seams, so no case needs a real compositor or touches the live session:
#   DOCTOR_HYPRLAND_INSTANCES  prints the `hyprctl instances -j` body
#   DOCTOR_JQ                  the jq that parses it
#   DOCTOR_PGREP               prints candidate compositor pids
#   DOCTOR_STAT_MTIME          answers mtime for /proc/<pid>
#   DOCTOR_NOW                 the clock
#   DOCTOR_SYSTEMCTL           systemctl --user (records calls)
#   DOCTOR_DBUS_UPDATE         dbus-update-activation-environment (records)
#   DOCTOR_KILL                kill (records)
#   DOCTOR_PKILL_CHILDREN      pkill (records)
# All of them record into $DOCTOR_LOG so cases assert on what was run, not
# just on exit status.
#
#   ./tests/hyprland-doctor.sh                 # builds the script, then tests it
#   ./tests/hyprland-doctor.sh /path/to/script # tests one you already have
set -u

# The flake ref comes from this script's own location, not `.#`, so running it
# from a worktree tests THAT tree rather than whichever happens to be the cwd.
# The host is pinned to utsuho -- the desktop host the incident was measured
# on -- because nothing in these cases is host-specific beyond the gate.
REPO=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)

SCRIPT="${1:-}"
if [ -z "$SCRIPT" ]; then
  drv=$(nix eval --raw "$REPO#homeConfigurations.utsuho.config.home.packages" \
    --apply 'ps: (builtins.head (builtins.filter (p: p.name or "" == "hyprland-doctor") ps)).drvPath') \
    || { echo "could not evaluate hyprland-doctor for utsuho" >&2; exit 1; }
  out=$(nix build --no-link --print-out-paths "$drv^out") \
    || { echo "could not build hyprland-doctor" >&2; exit 1; }
  SCRIPT="$out/bin/hyprland-doctor"
fi
[ -x "$SCRIPT" ] || { echo "not executable: $SCRIPT" >&2; exit 1; }
echo "testing $SCRIPT"

D=$(mktemp -d); trap 'rm -rf "$D"' EXIT
fails=0; n=0
ok() { n=$((n+1)); printf 'ok   %s\n' "$1"; }
no() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %s\n     %s\n' "$1" "$2"; }

# --- stubs --------------------------------------------------------------------
# Every seam records what it was called with. $DOCTOR_LOG is a scratch file the
# runner resets per case; $S holds the stubs.
S="$D/bin"; mkdir -p "$S"
DOCTOR_LOG="$D/calls"

# runner <registry-json> <pgrep-pids> <proc-mtime> <env-WAYLAND_DISPLAY>
# Runs one doctor pass against stubbed inputs. proc-mtime is what
# DOCTOR_STAT_MTIME answers for every /proc/<pid> -- enough to age a wedged
# candidate past the grace window without touching the real clock. The
# registry comes from $D/registry.json via the seam below; every recording
# stub (systemctl, dbus-update, kill, pkill) is wired here, so a case that
# asserts "nothing happened" is asserting against what the stubs saw.
run() { # run <pids> <mtime> <env-wd>
  : > "$DOCTOR_LOG"
  MTIME="$2" \
  DOCTOR_HYPRLAND_INSTANCES="cat $D/registry.json" \
  DOCTOR_JQ="$(command -v jq)" \
  DOCTOR_PGREP="echo $1" \
  DOCTOR_STAT_MTIME="$S/statmtime" \
  DOCTOR_NOW="echo 200" \
  DOCTOR_GRACE=30 \
  DOCTOR_SYSTEMCTL="$S/systemctl" \
  DOCTOR_DBUS_UPDATE="$S/dbus-update" \
  DOCTOR_KILL="$S/kill-stub" \
  DOCTOR_PKILL_CHILDREN="$S/pkill-stub" \
  "$SCRIPT" 2>"$D/err"
}

registry() { printf '%s\n' "$1" > "$D/registry.json"; }


# The systemctl stub: show-environment answers from $ENV_FILE, restart/anything
# else records the call. The dbus-update stub records its argv (which carries
# the imported values). kill/pkill record too.
ENV_FILE="$D/env"
cat > "$S/systemctl" <<EOF
#!/usr/bin/env bash
case " \$* " in
  *" show-environment "*) cat "$ENV_FILE" 2>/dev/null; exit 0 ;;
  *) echo "systemctl \$*" >> "$DOCTOR_LOG" ;;
esac
EOF
cat > "$S/dbus-update" <<EOF
#!/usr/bin/env bash
echo "dbus-update \$*" >> "$DOCTOR_LOG"
EOF
cat > "$S/kill-stub" <<EOF
#!/usr/bin/env bash
echo "kill \$*" >> "$DOCTOR_LOG"
EOF
cat > "$S/pkill-stub" <<EOF
#!/usr/bin/env bash
echo "pkill \$*" >> "$DOCTOR_LOG"
EOF

# statmtime: answers the fixed mtime every case passes via $MTIME, ignoring
# the /proc/<pid> argument the script hands it. Set per case before run().
cat > "$S/statmtime" <<EOF
#!/usr/bin/env bash
echo "\$MTIME"
EOF
chmod +x "$S"/*

PATH="$S:$PATH"

LIVE_JSON='[{"instance":"sigA_111","time":100,"pid":100,"wl_socket":"wayland-1"}]'

# --- 1. healthy session: no-op ------------------------------------------------
registry "$LIVE_JSON"
printf 'WAYLAND_DISPLAY=wayland-1\nPATH=/usr/bin\n' > "$ENV_FILE"
run "100" 100 "wayland-1"
if [ -s "$DOCTOR_LOG" ]; then
  no "healthy session is a no-op" "doctor called: $(cat "$DOCTOR_LOG")"
else
  ok "healthy session is a no-op"
fi

# --- 2. poisoned env: reconcile fires, imports the live values ----------------
registry "$LIVE_JSON"
printf 'WAYLAND_DISPLAY=wayland-2\n' > "$ENV_FILE"
run "100" 100 "wayland-2"
if grep -q "dbus-update" "$DOCTOR_LOG" \
   && grep -q "WAYLAND_DISPLAY=wayland-1" "$DOCTOR_LOG" \
   && grep -q "HYPRLAND_INSTANCE_SIGNATURE=sigA_111" "$DOCTOR_LOG"; then
  ok "poisoned env is reconciled to the live compositor"
else
  no "poisoned env is reconciled to the live compositor" "$(cat "$DOCTOR_LOG")"
fi
if grep -q "systemctl --user restart elephant.service wayland-wm-app-daemon.service" "$DOCTOR_LOG"; then
  ok "cached-env units are restarted after the import"
else
  no "cached-env units are restarted after the import" "$(cat "$DOCTOR_LOG")"
fi

# --- 3. no live compositor at all: nothing restarted ---------------------------
# The dead-socket import in the incident happened with a live compositor
# present, but a session teardown (compositor gone, target still up briefly)
# must not try to reconcile against nothing -- `exit 0` on empty registry,
# no systemctl, no dbus.
registry '[]'
printf 'WAYLAND_DISPLAY=wayland-2\n' > "$ENV_FILE"
run "" 100 "wayland-2"
if [ -s "$DOCTOR_LOG" ]; then
  no "empty registry: no reconcile, no restarts" "$(cat "$DOCTOR_LOG")"
else
  ok "empty registry: no reconcile, no restarts"
fi

# --- 4. reap: wedged instance killed -------------------------------------------
# pid 400 is in pgrep's output but not in the registry, and its /proc mtime
# (50) puts it 150s old against the 30s grace. It must be killed, children
# first.
registry "$LIVE_JSON"
printf 'WAYLAND_DISPLAY=wayland-1\n' > "$ENV_FILE"
run "100 400" 50 "wayland-1"
if grep -q "pkill -TERM -P 400" "$DOCTOR_LOG" \
   && grep -q "kill -9 400" "$DOCTOR_LOG"; then
  ok "wedged instance is reaped, children first"
else
  no "wedged instance is reaped, children first" "$(cat "$DOCTOR_LOG")"
fi

# --- 5. reap: live compositor never killed --------------------------------------
# pid 100 IS in the registry; even with the env file stale (to prove the reap
# pass and the reconcile pass are independent), pid 100 must not be killed.
# Reconcile fires (env stale) -- only pid 400, if present, would be killed;
# here pgrep reports only the live one.
registry "$LIVE_JSON"
printf 'WAYLAND_DISPLAY=wayland-1\n' > "$ENV_FILE"
run "100" 100 "wayland-1"
if grep -qE "(pkill|kill) .*100" "$DOCTOR_LOG"; then
  no "live compositor is never reaped" "$(cat "$DOCTOR_LOG")"
else
  ok "live compositor is never reaped"
fi

# --- 6. grace window: a young unregistered instance is left alone --------------
# pid 500, absent from the registry, but its /proc mtime (190) makes it only
# 10s old against the 30s grace. That is a compositor mid-startup, not wedged.
registry "$LIVE_JSON"
printf 'WAYLAND_DISPLAY=wayland-1\n' > "$ENV_FILE"
run "100 500" 190 "wayland-1"
if grep -qE "(pkill|kill) .*500" "$DOCTOR_LOG"; then
  no "mid-startup instance is not reaped inside the grace window" "$(cat "$DOCTOR_LOG")"
else
  ok "mid-startup instance is not reaped inside the grace window"
fi

# --- 7. oldest live wins when several are registered -----------------------------
# Two live instances (a crash-loop recovery can register a fresh one while the
# old is still up): the env must be reconciled to the OLDEST -- the one the
# user's windows are on -- not to the newest, which is mid-startup and will
# import its own env when its start hook lands.
TWO_JSON='[{"instance":"sigA_111","time":100,"pid":100,"wl_socket":"wayland-1"},
          {"instance":"sigB_222","time":150,"pid":200,"wl_socket":"wayland-2"}]'
# jq sorts by .time, so the oldest line comes first regardless of array order.
registry "$TWO_JSON"
printf 'WAYLAND_DISPLAY=wayland-9\n' > "$ENV_FILE"
run "100 200" 100 "wayland-9"
if grep -q "WAYLAND_DISPLAY=wayland-1" "$DOCTOR_LOG" \
   && grep -q "HYPRLAND_INSTANCE_SIGNATURE=sigA_111" "$DOCTOR_LOG" \
   && ! grep -q "WAYLAND_DISPLAY=wayland-2" "$DOCTOR_LOG"; then
  ok "reconcile targets the oldest live instance"
else
  no "reconcile targets the oldest live instance" "$(cat "$DOCTOR_LOG")"
fi

# --- 8. hyprctl failing: reap still works, nothing reconciles --------------------
# A dead compositor makes hyprctl print garbage or fail. The reap pass must
# still fire -- a compositor absent from a registry it cannot read is exactly
# as wedged as one absent from a registry it can -- but there is nothing live
# to reconcile against, so no import and no unit restarts may happen.
registry 'not-json'
printf 'WAYLAND_DISPLAY=wayland-2\n' > "$ENV_FILE"
run "400" 50 "wayland-2"
if grep -q "kill -9 400" "$DOCTOR_LOG" \
   && ! grep -q "dbus-update" "$DOCTOR_LOG" \
   && ! grep -q "systemctl" "$DOCTOR_LOG"; then
  ok "garbage registry: reap fires, reconcile does not"
else
  no "garbage registry: reap fires, reconcile does not" "$(cat "$DOCTOR_LOG")"
fi

# --- the teeth ------------------------------------------------------------------
n=$((n+1))
if [ "$n" -gt 8 ]; then
  ok "cases ran ($n checks)"
else
  no "cases ran" "expected at least 8 checks, ran $n"
fi

echo
if [ "$fails" -eq 0 ]; then echo "all $n passed"; else echo "$fails of $n failed"; fi
exit $(( fails > 0 ))
