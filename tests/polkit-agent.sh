#!/usr/bin/env bash
# Cases for the hyprpolkitagent user unit (defined in home.nix), the polkit
# authentication agent for graphical sessions.
#
# What is actually being protected here: polkit does not fall back to a prompt of
# its own. With no agent registered, every action whose default is auth_self or
# auth_admin is refused outright, with no prompt and nothing in polkitd's journal
# -- `pkcheck --allow-user-interaction` says "Authorization requires
# authentication but no agent is available." That is not a cosmetic gap. Two
# things in this repo depend on that prompt existing:
#
#   * `fprintd-enroll` (docs/new-host.md): net.reactivated.fprint.device.enroll
#     defaults to auth_self_keep, so enrolling a finger on a new box fails with
#     net.reactivated.Fprint.Error.PermissionDenied until an agent is running.
#   * Bitwarden's "unlock with system authentication", which authorizes
#     com.bitwarden.Bitwarden.unlock as auth_self -- the whole reason
#     system/etc/pam.d/polkit-1 puts pam_fprintd ahead of system-auth. Without an
#     agent that PAM stack is never reached, so the wiring looks broken when it is
#     merely unreachable.
#
# The ExecStart case is the subtle one. sauyon is a systemd-homed user with no
# /etc/passwd entry, so a nix-built binary that has not been through
# `withHostNss` cannot getpwuid the very user it is authenticating -- the agent
# starts and then cannot name whose password it wants. Upstream's unit embeds the
# unwrapped store path, so the assertion is that what we install points at the
# wrapped join, not at pkgs.hyprpolkitagent directly.
#
# The Restart pair is here because the *default* is a trap. With
# Restart=on-failure and no RestartSec, systemd's 100ms default plus this host's
# DefaultStartLimitBurst=5 / StartLimitIntervalSec=10s spends all five attempts
# inside half a second and then marks the unit failed for the rest of the
# session. Nothing retries, nothing notifies, and the symptom is exactly the
# silence this unit exists to remove. So RestartSec is pinned, not left to
# upstream's omission.
#
# The gating cases exist because home.nix serves headless hosts (fujiwara,
# kyuusaku) and a Darwin host (mari). An agent unit on a box where no graphical
# session ever runs is a unit that fails at every activation.
#
#   ./tests/polkit-agent.sh        # evaluates the flake's home configs
#
# Eval-only: nothing here builds hyprpolkitagent, touches polkit, or needs a
# graphical session. Whether an agent is actually *running* on this host is a
# deploy question, not one a test can answer -- `pkcheck --process $$
# --action-id net.reactivated.fprint.device.enroll --allow-user-interaction`
# answers it after `hms`.
#
# Every eval here is fatal on failure rather than being folded into "the
# attribute is absent". Conflating the two is what let an earlier draft of this
# file report `ok mari defines no hyprpolkitagent unit` for a config that never
# evaluated at all -- mari is the only aarch64-darwin host and is evaluated from
# x86_64-linux, so it is the likeliest of the six to break for reasons that have
# nothing to do with polkit. Same class as f242ab9.
set -u

cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 1

fails=0; n=0

ok() { n=$((n+1)); printf 'ok   %s\n' "$1"; }
no() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %s\n     %s\n' "$1" "$2"; }

die() { printf '%s\n' "$*" >&2; exit 1; }

# Is the unit defined for this host? `ss ? hyprpolkitagent` is a membership test,
# so it does not force any other unit's values -- a break in psi-notify or
# gnome-keyring-tpm cannot reach these cases.
#
# These two only *return* nonzero; they must not `die`. A `die` inside the
# `$(...)` a caller wraps them in exits the subshell and nothing more, so the
# caller would read an empty string and report some unrelated case as failed for
# the wrong reason. Callers therefore assign and check -- `x=$(f) || die` runs the
# die in the parent shell, where exiting means something.
has_unit() {
  local out
  out=$(nix eval --json \
    ".#homeConfigurations.$1.config.systemd.user.services" \
    --apply 'ss: ss ? hyprpolkitagent') || return 1
  case "$out" in
    true|false) printf '%s\n' "$out" ;;
    *) printf 'unexpected eval result for %s: %s\n' "$1" "$out" >&2; return 1 ;;
  esac
}

# The unit itself, for the field cases. Narrower than the whole attrset, so the
# blast radius stays this one unit.
unit_json() {
  nix eval --json \
    ".#homeConfigurations.$1.config.systemd.user.services.hyprpolkitagent" ||
    return 1
}

# Exact comparison, not a glob. A substring match here would accept the mutants
# that matter: `!WAYLAND_DISPLAY` is a valid systemd negation that inverts the
# gate, and `graphical-session.target.wants` is a different target -- both would
# pass a `*needle*` case while breaking the behaviour the case exists to pin.
want() {
  local label=$1 section=$2 key=$3 expected=$4 got
  got=$(python3 -c '
import json,sys
u=json.loads(sys.argv[1])
v=(u.get(sys.argv[2]) or {}).get(sys.argv[3])
if isinstance(v,list): v="\x1f".join(str(x) for x in v)
print("<unset>" if v is None else v)
' "$unit" "$section" "$key") || die "could not read $section.$key out of the unit JSON"
  if [ "$got" = "$expected" ]; then
    ok "$label"
  else
    no "$label" "want '$expected', got '$got'"
  fi
}

echo "evaluating shiori (gui = true)"

# 1. The unit exists at all. Everything below is a refinement of this one.
present=$(has_unit shiori) ||
  die "could not evaluate systemd.user.services for shiori (see the nix error above)"
if [ "$present" = true ]; then
  ok "shiori defines a hyprpolkitagent user unit"
  unit=$(unit_json shiori) ||
    die "could not evaluate the hyprpolkitagent unit for shiori (see the nix error above)"
else
  no "shiori defines a hyprpolkitagent user unit" "no such unit in systemd.user.services"
  unit='{}'
fi

# 2. ExecStart must be the withHostNss join, not the bare package: see the
#    getpwuid note in the header. Anchored at both ends -- the whole point is
#    which store path it is, so a suffix match would accept the unwrapped one
#    inside a longer string.
#    home-manager normalises ExecStart to a list even when it is written as a
#    single string, so unwrap exactly one element rather than stringifying the
#    list -- an earlier draft globbed the joined form, which would have accepted
#    a second ExecStart line appended after the wrapped one.
exec_start=$(python3 -c '
import json,sys
v=(json.loads(sys.argv[1]).get("Service") or {}).get("ExecStart")
if isinstance(v,list):
    v = v[0] if len(v)==1 else "<%d ExecStart entries>" % len(v)
print(v if v else "<unset>")
' "$unit")
#    It must also go through nixGL, and that half is not cosmetic: the agent only
#    builds its Qt Quick dialog when a challenge actually arrives, and a nix-built
#    Qt on this non-NixOS host resolves libEGL/GBM/DRI out of the store, where
#    there is no driver for this GPU. Without nixGL the agent runs happily until
#    the first prompt, then logs "EGL not available" / "Failed to initialize
#    graphics backend for OpenGL" and SIGABRTs -- polkitd records that as the
#    operator FAILING to authenticate, and the caller sees the same bare
#    PermissionDenied as having no agent at all. RestartSec then revives it, so
#    the unit reads healthy afterwards and the crash is easy to miss entirely.
#    Order: nixGL outside, host-nss inside. nixGL sets
#    LD_LIBRARY_PATH=<mesa>${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}, so it preserves
#    whatever is already there, and the inner wrapper's --prefix then prepends
#    host-nss onto it -- both survive. (config.lib.nixGL.wrap, used for hyprlock
#    and ghostty, is not usable here: it only rewrites bin/, and this package
#    ships its binary in libexec/.)
nixgl_re='^/nix/store/[a-z0-9]{32}-nixGL/bin/nixGL '
hostnss_re='/nix/store/[a-z0-9]{32}-hyprpolkitagent-[0-9.]+-host-nss/libexec/hyprpolkitagent$'
if [[ $exec_start =~ $nixgl_re$hostnss_re ]]; then
  ok "shiori's ExecStart runs the host-nss wrapped binary through nixGL"
else
  no "shiori's ExecStart runs the host-nss wrapped binary through nixGL" "got: $exec_start"
fi

# 2b. The shape above pins a *name this repo chooses itself* -- `nixGL` comes from
#     our own writeShellScriptBin, so the test and the code agree by construction.
#     Every mutant that matters keeps the name: replace the shim body with
#     `exec "$@"`, or point it at a different vendor wrapper, and the path still
#     matches while the SIGABRT comes straight back. So read the shim and check it
#     actually hands off to a nixGL vendor wrapper. Still eval-only -- the file is
#     already in the store because the unit references it.
shim=${exec_start%% *}
if [ -r "$shim" ] && grep -qE '/nix/store/[a-z0-9]{32}-nixGL[A-Za-z]+/bin/nixGL[A-Za-z]+' "$shim"; then
  ok "shiori's nixGL shim actually dispatches to a nixGL vendor wrapper"
else
  no "shiori's nixGL shim actually dispatches to a nixGL vendor wrapper" \
    "$shim does not exec a nixGL* vendor wrapper"
fi

# 3. Install.WantedBy is what puts the .wants symlink in place. A unit file with
#    no wants link is a unit that never starts -- the failure mode this repo
#    already hit with the portal units (see home.nix's xdg.configFile block).
want "shiori's unit is wanted by graphical-session.target" \
  Install WantedBy "graphical-session.target"

# 4. Upstream's ConditionEnvironment=WAYLAND_DISPLAY keeps the unit a no-op when
#    activation runs outside a Wayland session (a plain ssh login, say) instead
#    of leaving a failed unit behind. Hand-writing the unit is where this gets
#    dropped -- and a negated spelling would invert the gate while still reading
#    like the right string.
want "shiori's unit keeps upstream's WAYLAND_DISPLAY condition" \
  Unit ConditionEnvironment "WAYLAND_DISPLAY"

# 5/6. Restart on failure, but slowly enough to survive one. Pinned as a pair:
#      on-failure with the 100ms default burns this host's five-attempt budget in
#      under half a second and then gives up permanently, which reproduces the
#      exact silence the unit exists to fix. See the header.
want "shiori's unit restarts on failure" Service Restart "on-failure"
want "shiori's unit spaces restarts out rather than taking systemd's 100ms" \
  Service RestartSec "5"

# 7/8. The start limit has to stay *reachable*, which RestartSec fights. systemd's
#      defaults here are burst 5 over a 10s window, and a 5s delay fits only three
#      attempts into 10s -- so the limiter never trips, and an agent that is
#      permanently broken (a mesa regression, a GC'd exec target, a permanent
#      RegisterAuthenticationAgent collision) restarts every five seconds forever
#      while `systemctl --user status` reads `active (running)` for most of any
#      sample. That is the same false-healthy reading that let the EGL crash ship.
#      Widening the window to 60s means five failures inside ~25s do stick, and the
#      unit lands in `failed` where it can be seen. Pinned as a trio with the two
#      above: change any one and the arithmetic stops working.
want "shiori's unit widens the start-limit window so repeated crashes stick" \
  Unit StartLimitIntervalSec "60s"
want "shiori's unit pins the start-limit burst the window is sized against" \
  Unit StartLimitBurst "5"

# The unit is generated identically for every desktop host, so checking only
# shiori leaves the other gui hosts unmeasured -- and the nixGL shim is hardcoded
# to one vendor wrapper regardless of machine.gpu. This does not catch a wrong
# vendor, but it does catch a gui host that silently loses the unit.
echo "evaluating setsuna (gui = true)"
present=$(has_unit setsuna) ||
  die "could not evaluate systemd.user.services for setsuna (see the nix error above)"
if [ "$present" = true ]; then
  ok "setsuna, the other gui host, defines the unit too"
else
  no "setsuna, the other gui host, defines the unit too" "no unit on a gui = true host"
fi

# Gating: no agent on hosts with no graphical session, and none on Darwin,
# which has no polkit at all.
for host in fujiwara mari; do
  echo "evaluating $host (no graphical session)"
  present=$(has_unit "$host") ||
    die "could not evaluate systemd.user.services for $host (see the nix error above)"
  if [ "$present" = false ]; then
    ok "$host defines no hyprpolkitagent unit"
  else
    no "$host defines no hyprpolkitagent unit" "unit is present on a host with no graphical session"
  fi
done

echo
if [ "$fails" -eq 0 ]; then
  echo "all $n cases passed"
else
  echo "$fails of $n cases failed"
fi
exit $(( fails > 0 ? 1 : 0 ))
