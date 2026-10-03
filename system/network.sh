#!/usr/bin/env bash
# NetworkManager helpers for ./deploy. Sourced, never executed: sourcing is what
# lets ../tests/kon-wireguard-profile.sh drive these against a temp tree instead
# of /etc. Nothing here has an effect at source time, and every path is an
# argument -- no function reaches for a location of its own.
#
# The seams are `${VAR-default}`, not `${VAR:-default}`, for the same reason
# ./pacman.sh gives: the tests set SUDO to the empty string to mean "write as
# me", which the :- form would quietly turn back into sudo.

# Install a WireGuard keyfile, carrying the per-device values over from whatever
# is already installed.
#
# The repo owns every field except two, and neither is committable: this
# device's private key, and the home server's public key (which is the
# [wireguard-peer.*] section's name). Both are read back out of $dest and
# spliced into the rendered template.
#
# A destination with neither is an error, not something to paper over with an
# empty key. NetworkManager would accept the resulting profile and bring up a
# device that can never complete a handshake -- provisioned-looking and dead,
# which is the failure mode ../tests/system-secrets.sh case 2 is also about.
#
# The key never reaches another process: the splice is pure bash, and the write
# goes through a `printf` builtin into install's stdin, so it appears in no
# argv and no environment. See CLAUDE.md on secret handling.
install_wg_profile() { # install_wg_profile <template> <dest>
  local template=$1 dest=$2

  if [ ! -r "$template" ]; then
    echo "install_wg_profile: no readable template at $template" >&2
    return 1
  fi

  # $dest is 600 root-owned in production, hence SUDO. Failure to read it is
  # indistinguishable here from "it has no key", and both land in the error
  # below, which is the right outcome either way.
  local priv peer
  priv=$(${SUDO-sudo} sed -n 's/^private-key=\(..*\)$/\1/p' "$dest" 2>/dev/null | head -n1)
  peer=$(${SUDO-sudo} sed -n 's/^\[wireguard-peer\.\(..*\)\]$/\1/p' "$dest" 2>/dev/null | head -n1)

  if [ -z "$priv" ] || [ -z "$peer" ]; then
    cat >&2 <<MSG
install_wg_profile: $dest has no WireGuard private key and/or peer public key to
carry over, and this repo deliberately holds neither -- the private key is
per-device, and the peer key identifies the home server.

Seed them once by hand, then re-run deploy. Either import a .conf the home
server generated:

    sudo nmcli connection import type wireguard file <conf>

or write the two lines into $dest yourself (as root, mode 600):

    [wireguard]
    private-key=<this device's key, e.g. from \`wg genkey\`>

    [wireguard-peer.<the home server's public key>]

A key generated fresh also has to be added to the home server's peer list, or
the handshake will never complete.
MSG
    return 1
  fi

  # Render fully before touching $dest. The ordering is the lesson from
  # ../tests/system-secrets.sh case 2: a producer that fails inside a pipeline
  # has already let `install` create and truncate the destination, so a failed
  # deploy destroys a working profile and exits 0 having written nothing.
  local rendered="" line
  while IFS= read -r line || [ -n "$line" ]; do
    rendered+="${line//@PEER_PUBKEY@/$peer}"$'\n'
    # NetworkManager accepts the key anywhere in the section; immediately after
    # the header keeps it next to the mtu/route-policy fields it belongs with.
    if [ "$line" = '[wireguard]' ]; then
      rendered+="private-key=$priv"$'\n'
    fi
  done < "$template"

  # Belt and braces: if the template ever loses its [wireguard] header or its
  # peer placeholder, the splice silently no-ops and we would install a profile
  # with no key. Refuse instead.
  case $rendered in
    *"private-key=$priv"*) ;;
    *) echo "install_wg_profile: $template has no [wireguard] section; refusing to install" >&2
       return 1 ;;
  esac
  case $rendered in
    *"[wireguard-peer.$peer]"*) ;;
    *) echo "install_wg_profile: $template has no @PEER_PUBKEY@ placeholder; refusing to install" >&2
       return 1 ;;
  esac

  printf '%s' "$rendered" \
    | ${SUDO-sudo} install -m600 -o root -g root /dev/stdin "$dest"
}
