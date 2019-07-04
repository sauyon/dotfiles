#!/bin/bash

usage() {
	cat <<EOF
USAGE: bootstrap.sh USERNAME
EOF
}

getrep() {
	read -p "$1"
	while [[ ! $REPLY =~ ^[YyNn]|$ ]]; do
		read -p "Please enter y or n: "
	done

	[[ $REPLY =~ ^[Nn]$ ]] && return 1
	return 0
}

if [[ $# -ne 1 ]]; then
  usage
  exit 1
fi

cat <<EOF
These steps must be done outside this bootstrap script:
 - configured the pacman mirrorlist
 - generated and set locales
 - setup the bootloader
EOF
if !getrep "Continue? [Y/n]: "; then
  exit 1
fi

if getrep "Run first-time setup? [Y/n]: "; then
  read -p "Enter hostname: "
  echo "$REPLY" > /etc/hostname
  hostname "$REPLY"

  pacman -Syu --noconfirm
  pacman -S zsh git base-devel --noconfirm --needed

  echo "Setting password for root:"
  passwd

  useradd "$1" -s /bin/zsh -g users -G wheel network video
  echo "Setting password for $1:"
  passwd "$1"

  mkdir -p "/home/$1"
  chown "$1":users "/home/$1"
  git clone https://github.com/sauyon/dotfiles "/home/$1"

  echo -n "Building yay..."
  git clone https://aur.archlinux.org/yay
  cd yay
  sudo -u "$1" makepkg -si
  echo " Done."
  cd ..
  rm -rf yay
fi

if getrep "Install xorg/dm/etc? [Y/n] "; then
  echo -n "Installing packages..."
  yay -S alacritty dunst gdm grim emacs feh firefox noto-fonts sway swayidle \
         swaylock lxappearance mako network-manager-applet networkmanager \
         noto-fonts pam-u2f pavucontrol pcscd polkit-gnome pulseaudio \
         quodlibet scrot slurp sxiv \
      --noconfirm --needed

  # AUR packages
  sudo -u "$1" \
  yay -S acpilight dmenu-wayland-git gtk-theme-numix-sx j4-dmenu-desktop \
         lightdm-settings lightdm-slick-greeter numix-icon-theme-git \
         powerline-fonts-git siji-git ttf-google-fonts-git \
      --noconfirm --needed

	systemctl enable lightdm
else
  echo -n "Installing packages..."
	yay -S emacs-nox --noconfirm --needed
fi
yay -S cowsay htop mlocate openssh p7zip pkgfile ripgrep rsync sudo tldr \
    --noconfirm --needed
sudo -u "$1" \
yay -S oh-my-zsh-git --noconfirm --needed

echo " Done."

echo -n "Updating package and locate databases..."
pkgfile --update
updatedb
echo " Done."
