#!/usr/bin/env bash
# sops helpers for ./deploy. Sourced, never executed: sourcing is what lets
# ../tests/system-secrets.sh drive these against a temp tree instead of /etc.
# Nothing here has an effect at source time, and every path is an argument --
# no function reaches for a location of its own.
#
# The seams are `${VAR-default}`, not `${VAR:-default}`, for the same reason
# ./pacman.sh gives: the tests set SUDO to the empty string to mean "write as
# me", which the :- form would quietly turn back into sudo.

# The sops credential environment for <host>, as KEY=VALUE lines.
#
# home.nix already makes this decision, once, per host: a host in wifHosts gets
# a WIF credential config (a store path, plus the ALLOW_EXECUTABLES flag Google's
# auth library requires for executable-sourced credentials), and every other host
# gets the GCP service-account key under $HOME. Reading `sops.environment` back
# out of the flake is what keeps that decision in one place.
#
# ./deploy used to hardcode the service-account path instead. That made it
# impossible to decrypt on the one WIF host -- the host which, by design, has no
# decryption key on disk at all -- and because the failure landed inside a
# pipeline (see install_secret) it presented as an empty credential file rather
# than as an error.
#
# PATH is dropped. sops-nix puts PATH="" in that attrset for sops-install-secrets'
# own sandboxed activation; exporting it here would blank ./deploy's PATH and
# break every command after the export.
sops_env_for() { # sops_env_for <repo> <host>
  local repo=$1 host=$2
  ${NIX-nix} eval --json "$repo#homeConfigurations.$host.config.sops.environment" \
    | ${JQ-jq} -r 'to_entries[] | select(.key != "PATH") | "\(.key)=\(.value)"'
}

# The same, unless the caller set GOOGLE_APPLICATION_CREDENTIALS, in which case
# that wins -- for deliberately deploying with some other credential.
#
# What this deliberately does NOT do is default
# GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES on for that override. That flag is
# not a compatibility toggle: an external_account credential config carries
# `credential_source.executable.command`, an arbitrary command line, and the flag
# is the interlock Google ships so the auth library will not run it. Off means a
# credential config is data; on means it is code.
#
# home.nix sets it because the path it names is a writeText store path --
# immutable, and its own comment says as much. An operator-supplied path carries
# no such guarantee, and credential configs are documented as safe-to-commit
# artifacts, so they circulate: pasted into an issue, downloaded during a
# bootstrap, sitting in a world-writable directory. Turning the interlock on for
# a path someone was handed would make running deploy with it code execution as
# the user -- and in the bootstrap path, one NOPASSWD sudo away from root.
#
# So an override may still ask for it, explicitly. That is the operator
# deciding; it is just not decided for them.
sops_env_select() { # sops_env_select <repo> <host>
  if [ -n "${GOOGLE_APPLICATION_CREDENTIALS-}" ]; then
    printf 'GOOGLE_APPLICATION_CREDENTIALS=%s\n' "$GOOGLE_APPLICATION_CREDENTIALS"
    [ -z "${GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES-}" ] \
      || printf 'GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES=%s\n' \
           "$GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES"
    return 0
  fi
  sops_env_for "$1" "$2"
}

# Decrypt <key> from <secrets-file> and install it at <dest>, root-owned 0600,
# with <line-prefix> prepended if given.
#
# Decrypt first, install second, and that order is the entire point. The shape
# this replaces was
#
#   sops -d --extract '["k"]' ../secrets.yaml | sed 's#^#prefix #' \
#     | sudo install -m600 -o root -g root /dev/stdin DEST
#
# whose three stages run concurrently: install created and truncated DEST before
# sops had finished failing, then exited 0 having written nothing. So a failed
# deploy did not leave the previous credential in place, it replaced it with a
# 0-byte file that looks provisioned -- and `set -euo pipefail` then stopped the
# script on that line, so every step after it silently never ran.
#
# The plaintext lives in a shell variable and reaches `install` on stdin. It is
# never an argument: /proc/<pid>/cmdline is world-readable, so a secret in argv
# is a secret published to every user on the box for the duration of the call.
# printf is a bash builtin, so nothing is forked to do the write either.
# Tracing is off for the whole of the inner function and restored after, which
# is the price of holding the plaintext in a shell variable at all. `set -x`
# traces assignments and the arguments of builtins, so `bash -x system/deploy`
# -- how this script gets debugged, and whose output gets pasted into commit
# messages and issues in a public repo -- would otherwise print the attic pull
# token and the mari SSH private key in full to stderr. The pipeline this
# replaced never held the plaintext in the shell, so it never had that exposure;
# the new shape has to close it deliberately. One restore point, so no early
# return can skip it.
install_secret() { # install_secret <secrets-file> <key> <dest> [line-prefix]
  local xtrace=""
  case $- in *x*) xtrace=1; set +x ;; esac
  _install_secret "$@"
  local rc=$?
  [ -z "$xtrace" ] || set -x
  return "$rc"
}

_install_secret() { # not called directly; see install_secret above
  local secrets=$1 key=$2 dest=$3 prefix=${4-} val

  if ! val=$(${SOPS-sops} -d --extract "[\"$key\"]" "$secrets"); then
    echo "deploy: could not decrypt $key from $secrets" >&2
    return 1
  fi

  # An empty decrypt is the 0-byte-netrc shape arriving by another route. It has
  # to be an error here too, or the file still ends up looking provisioned.
  if [ -z "$val" ]; then
    echo "deploy: $key decrypted to nothing — refusing to install an empty $dest" >&2
    return 1
  fi

  # A prefix is a netrc-entry prefix, and a netrc entry is one line. Prefixing
  # only the first line of a multi-line value would leave a bare token on the
  # second -- a file curl parses happily and wrongly.
  if [ -n "$prefix" ] && [ "$(printf '%s' "$val" | wc -l)" -gt 0 ]; then
    echo "deploy: $key is multi-line; refusing to write it as a prefixed entry in $dest" >&2
    return 1
  fi

  printf '%s%s\n' "$prefix" "$val" \
    | ${SUDO-sudo} install -m600 -o root -g root /dev/stdin "$dest"
}
