#!/usr/bin/env bash
# Second stage, run as the user (not root) on the freshly booted host, e.g.
#   ssh sauyon@<ip> 'bash -s' < install/post-install.sh
# Installs Determinate Nix, clones the dotfiles, does the first Home Manager
# switch, then drops the temporary passwordless sudo from install-base.sh.
set -euo pipefail

host="$(uname -n)"; host="${host%%.*}"
repo="$HOME/devel/dotfiles"

if [ ! -d /nix/var/nix/profiles/default ]; then
  curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix \
    | sudo sh -s -- install --no-confirm
fi
# shellcheck disable=SC1091
. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

mkdir -p "$HOME/devel"
[ -d "$repo" ] || git clone https://forge.ko.ag/sauyon/dotfiles.git "$repo"

# First switch builds locally: the attic pull token only arrives with
# system/deploy, which needs the sops key that isn't on the host yet.
nix run github:nix-community/home-manager -- switch -b pre-hm --flake "$repo#$host"

sudo rm -f /etc/sudoers.d/99-bootstrap
echo "POST-INSTALL DONE ($host)"
