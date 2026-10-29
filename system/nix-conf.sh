#!/usr/bin/env bash
# nix.conf helpers for ./deploy. Sourced, never executed: sourcing is what lets
# ../tests/nix-conf-splice.sh drive these against a temp tree instead of /etc.
# Nothing here has an effect at source time, and every path is an argument --
# no function reaches for a location of its own.
#
# The seams are `${VAR-default}`, not `${VAR:-default}`, for the same reason
# ./pacman.sh gives: the tests set SUDO to the empty string to mean "write as
# me", which the :- form would quietly turn back into sudo.

# Install a nix.conf fragment whose @NIX_BUILDERS@ placeholder carries the
# remote-builder line from dotfiles-private (endpoints.nix: nixBuildersLine).
#
# Why a splice and not a flake eval: /etc/nix/nix.custom.conf is read by the
# nix daemon at boot, before any home-manager generation exists -- the same
# ordering that keeps the builder key itself in sops rather than the flake.
# hms materializes the value at ~/.config/kon/nix-builders (home.nix), the
# same file-shape the WireGuard endpoint uses.
#
# The builder line is topology, not a secret: it names the darwin box's LAN
# address and pins its host key. It lives in dotfiles-private because this
# repo is public and the line assembles the network map.
install_nix_conf() { # install_nix_conf <template> <dest> <builders-file>
  local template=$1 dest=$2 builders_file=$3

  if [ ! -r "$template" ]; then
    echo "install_nix_conf: no readable template at $template" >&2
    return 1
  fi

  if [ ! -r "$builders_file" ]; then
    echo "install_nix_conf: no readable builders file at $builders_file" >&2
    echo "install_nix_conf: hms writes it from dotfiles-private; run 'hms' first, then re-run deploy." >&2
    return 1
  fi

  local builders
  IFS= read -r builders < "$builders_file" || [ -n "$builders" ]

  if [ -z "$builders" ]; then
    echo "install_nix_conf: $builders_file is empty -- refusing to splice an empty builders line" >&2
    return 1
  fi

  # Render fully before touching $dest -- the ../tests/system-secrets.sh
  # case-2 lesson: a producer that fails inside a pipeline has already let
  # `install` create and truncate the destination, so a failed deploy would
  # destroy a working nix.conf fragment and exit 0 having written nothing.
  local rendered="" line
  while IFS= read -r line || [ -n "$line" ]; do
    rendered+="${line//@NIX_BUILDERS@/$builders}"$'\n'
  done < "$template"

  # Refuse rather than install an unsubstituted placeholder: the daemon would
  # read `builders = @NIX_BUILDERS@` as a broken builder spec and every
  # aarch64-darwin build would fail far from the cause.
  case $rendered in
    *@NIX_BUILDERS@*)
      echo "install_nix_conf: $template has no @NIX_BUILDERS@ placeholder; refusing to install" >&2
      return 1 ;;
  esac

  printf '%s' "$rendered" \
    | ${SUDO-sudo} install -m644 -o root -g root /dev/stdin "$dest"
}
