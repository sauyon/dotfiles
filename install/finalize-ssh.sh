#!/usr/bin/env bash
# Leave inbound ssh in the intended state, as the last act of `mise run bootstrap`.
#
# install-base.sh:208 enables sshd inside the chroot on purpose -- line 205: "Key-only
# SSH for sauyon, for running bootstrap from another machine". That is a bootstrap
# affordance, not a posture, and nothing used to retire it. shiori was installed
# 2026-09-26 and then sat for three days with ungated key-only sshd on every
# interface, on wifi, no firewall service in front of it, while fujiwara, mari and
# the NixOS nodes were all behind the ssh-oidc gate. Both states look identical
# from outside -- sshd is `active` either way -- which is why it went unnoticed.
#
# The rule enforced here:
#
#   a working ssh-oidc gate  -> leave sshd enabled. It is gated; that is the goal.
#   anything else            -> disable sshd. No ungated inbound ssh.
#
# "Working" is not "the drop-in exists". A drop-in whose ForceCommand names a
# binary that is not there is the worst of the four states: sshd starts, accepts
# the key, and every session then dies in the forced command -- it reads as
# installed and behaves as broken. So the gate binary is read out of the drop-in
# (never hardcoded, so a non-default install path still counts) and must be
# executable.
#
# Idempotent: mise.toml runs this from an EXIT trap, so it runs again on every
# bootstrap, and re-disabling an already-disabled unit is a no-op.
#
# FINALIZE_SSH_ROOT prefixes every path, for tests/bootstrap-ssh-finalize.sh.
# Needs root to touch units; the bootstrap trap calls it under `sudo -n` while
# install-base.sh's 99-bootstrap NOPASSWD file is still in place, so it must run
# BEFORE that file is removed.
set -uo pipefail

root="${FINALIZE_SSH_ROOT:-}"
dropin="$root/etc/ssh/sshd_config.d/10-ssh-oidc.conf"

# The gate's binary, as sshd would actually invoke it: the first argument of the
# drop-in's ForceCommand. Absent drop-in, absent ForceCommand, or a ForceCommand
# with no path all yield the empty string, which fails the executable test below.
gate_bin=""
if [ -f "$dropin" ]; then
  gate_bin=$(awk 'tolower($1) == "forcecommand" { print $2; exit }' "$dropin")
fi

if [ -n "$gate_bin" ] && [ -x "$root$gate_bin" ]; then
  echo "finalize-ssh: ssh-oidc gate active ($gate_bin); leaving sshd enabled"
  exit 0
fi

# No working gate. Say which of the two halves is missing -- on a host that was
# meant to be gated, this line is the whole diagnosis.
if [ ! -f "$dropin" ]; then
  reason="no gate drop-in at ${dropin#"$root"}"
elif [ -z "$gate_bin" ]; then
  reason="gate drop-in has no ForceCommand path"
else
  reason="gate binary $gate_bin is missing or not executable"
fi
echo "finalize-ssh: $reason; disabling sshd so nothing ungated is listening"

# Both units can put a listener on 22. Touch only the ones that are actually
# enabled or running: on Arch, sshd.socket ships but is not preset-enabled, and
# `disable --now` on an absent unit is an error, not a no-op.
rc=0
for unit in sshd.socket sshd.service; do
  if systemctl is-enabled "$unit" >/dev/null 2>&1 || systemctl is-active "$unit" >/dev/null 2>&1; then
    if ! systemctl disable --now "$unit"; then
      echo "finalize-ssh: WARNING: failed to disable $unit -- it may still be listening" >&2
      rc=1
    fi
  fi
done
exit "$rc"
