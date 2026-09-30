#!/usr/bin/env bash
# Cases for install/finalize-ssh.sh -- the step that decides, at the end of
# `mise run bootstrap`, whether this host is allowed to keep listening on 22.
#
# install-base.sh:208 enables sshd inside the arch-chroot, deliberately: line 205
# says why ("Key-only SSH for sauyon, for running bootstrap from another
# machine"). Nothing ever turned it back off. shiori was installed 2026-09-26 and
# sat with ungated, key-only sshd on every interface for three days, on wifi, with
# no firewall service in front of it -- while fujiwara, mari and the NixOS nodes
# were all behind the ssh-oidc gate. The asymmetry was invisible because both
# states look identical from outside: sshd is `active` either way.
#
# The bootstrap already has the right shape for the fix -- mise.toml:187 drops the
# NOPASSWD sudoers file "however we exit" -- but sshd got no equivalent. So the
# rule this script enforces is the one the operator actually wants:
#
#   a working ssh-oidc gate  -> leave sshd enabled (it is gated; that is the goal)
#   anything else            -> disable sshd (no ungated inbound ssh, ever)
#
# "Working" is deliberately not "the drop-in exists". A drop-in whose
# ForceCommand names a binary that is not there is the worst of the four states:
# sshd starts, accepts the key, and then every session dies in the forced
# command. It reads as installed and behaves as broken, so it must count as no
# gate at all -- hence the cases below read the path out of the drop-in and
# require it to be executable, rather than testing for a hardcoded location.
#
#   ./tests/bootstrap-ssh-finalize.sh
#
# No network, no root, no real systemctl: each case is a crafted fake root plus a
# systemctl shim on PATH that records its arguments.
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
prog="$repo/install/finalize-ssh.sh"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

work=$(mktemp -d); trap 'rm -rf "$work"' EXIT

# Every "disables sshd" case below asserts that the shim recorded a disable. A
# script that does not exist records nothing and would read as "did not disable"
# -- which is also what a correct gated case looks like. Without this gate a
# missing script would show up as a suite where half the cases pass for the one
# reason that means nothing is being checked at all.
if [ ! -x "$prog" ]; then
  echo "FAIL 0 - $prog does not exist or is not executable; the cases below would pass vacuously" >&2
  echo; echo "0 checks, 1 failed"; exit 1
fi

# systemctl shim: records every invocation, succeeds silently.
mkdir -p "$work/bin"
cat > "$work/bin/systemctl" <<'SHIM'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$SYSTEMCTL_LOG"
SHIM
chmod +x "$work/bin/systemctl"
export PATH="$work/bin:$PATH"

# mkroot <name> -- a fake root with an sshd_config.d, echoes its path.
mkroot() {
  local r="$work/$1"
  rm -rf "$r"; mkdir -p "$r/etc/ssh/sshd_config.d"
  printf '%s' "$r"
}

# gate_dropin <root> <forcecommand-path> -- the gate's sshd drop-in, as
# install/10-ssh-oidc.conf lays it out (the ForceCommand path is what matters).
gate_dropin() {
  cat > "$1/etc/ssh/sshd_config.d/10-ssh-oidc.conf" <<EOF
AuthenticationMethods publickey
AuthorizedKeysCommand /etc/ssh-oidc/authkeys.sh %u %k %t
AuthorizedKeysCommandUser ssh-oidc
ExposeAuthInfo yes
ForceCommand $2 /etc/ssh-oidc/config
AuthorizedKeysFile none
EOF
}

# gate_binary <root> <abs-path> -- an executable where the drop-in points.
gate_binary() {
  mkdir -p "$1$(dirname "$2")"
  printf '#!/bin/sh\n' > "$1$2"; chmod +x "$1$2"
}

# run <name> <root> <expect keep|disable>
run() {
  local name="$1" root="$2" expect="$3"
  export SYSTEMCTL_LOG="$work/systemctl.log"; : > "$SYSTEMCTL_LOG"
  local out rc
  out=$(FINALIZE_SSH_ROOT="$root" "$prog" 2>&1); rc=$?
  if [ "$rc" -ne 0 ]; then
    bad "$name: exited $rc (want 0); output: $out"; return
  fi
  local disabled=no
  grep -q -- 'disable' "$SYSTEMCTL_LOG" && disabled=yes
  case "$expect" in
    disable)
      if [ "$disabled" = yes ]; then
        # It must also stop the running daemon, not just clear the symlink --
        # a plain `disable` leaves the listener up until the next boot.
        if grep -q -- '--now' "$SYSTEMCTL_LOG"; then
          ok "$name: disabled sshd, with --now"
        else
          bad "$name: disabled sshd but without --now, so it keeps listening until reboot"
        fi
      else
        bad "$name: left sshd enabled; wanted it disabled. systemctl calls: $(tr '\n' ';' < "$SYSTEMCTL_LOG")"
      fi
      ;;
    keep)
      if [ "$disabled" = no ]; then
        ok "$name: left sshd enabled"
      else
        bad "$name: disabled sshd; the gate is present so it must stay enabled"
      fi
      ;;
  esac
}

bin=/usr/local/bin/ssh-oidc-gate

# 1. The goal state: gate installed and its binary present. sshd stays up,
#    because the gate is the thing making it safe.
r=$(mkroot gated); gate_dropin "$r" "$bin"; gate_binary "$r" "$bin"
run "gate installed and runnable" "$r" keep

# 2. shiori as found on 2026-09-29: sshd enabled, no gate anywhere.
r=$(mkroot bare)
run "no gate at all" "$r" disable

# 3. Drop-in present, binary missing. Reads as installed, behaves as broken --
#    every session dies in the ForceCommand. Must not count as a gate.
r=$(mkroot halfinstalled); gate_dropin "$r" "$bin"
run "drop-in present but gate binary missing" "$r" disable

# 4. Binary present, drop-in missing. sshd is wide open on keys alone; the
#    binary on disk proves nothing about what sshd is configured to do.
r=$(mkroot binaryonly); gate_binary "$r" "$bin"
run "gate binary present but no drop-in" "$r" disable

# 5. The drop-in names a path other than the default. The check must follow the
#    config, not a hardcoded location, or a valid install trips the case-3 path.
alt=/opt/ssh-oidc/bin/gate
r=$(mkroot altpath); gate_dropin "$r" "$alt"; gate_binary "$r" "$alt"
run "gate at a non-default path, per the drop-in" "$r" keep

# 6. Binary exists at the drop-in's path but is not executable -- an interrupted
#    install (install.sh copies then chmods). sshd cannot run it.
r=$(mkroot notexec); gate_dropin "$r" "$bin"
mkdir -p "$r$(dirname "$bin")"; printf '#!/bin/sh\n' > "$r$bin"; chmod 0644 "$r$bin"
run "gate binary present but not executable" "$r" disable

# 7. Idempotence: the trap in mise.toml:187 fires on every exit, so this runs
#    again on the next bootstrap. A second run on an already-disabled host must
#    still be a clean no-drama disable, not an error.
r=$(mkroot twice)
run "no gate, first run" "$r" disable
run "no gate, second run" "$r" disable

echo
if [ "$fails" -eq 0 ]; then
  echo "$n checks, 0 failed"
else
  echo "$n checks, $fails failed"; exit 1
fi
