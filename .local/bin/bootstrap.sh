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

read -p "Enter hostname: "
echo "$REPLY" > /etc/hostname
hostname "$REPLY"

pacman -Syu --noconfirm
pacman -S zsh git base-devel --noconfirm

echo "Setting password for root:"
passwd

useradd "$1" -s /bin/zsh -g users -G wheel network video
echo "Setting password for $1:"
passwd "$1"

mkdir -p "/home/$1"
chown "$1":users "/home/$1"
git clone https://github.com/sauyon/dotfiles "/home/$1"

sudo -u "$1" ssh-keygen -f id_ed25519 -t ed25519 -N ''

echo -n "Building aura-bin..."
git clone https://aur.archlinux.org/aura-bin
cd aura-bin
sudo -u "$1" makepkg -si
echo " Done."

if getrep "Install xorg/dm/etc? [Y/n] "; then
  echo -n "Installing packages..."
	aura -S alacritty dunst emacs feh firefox i3-wm i3blocks i3status \
       lightdm lxappearance network-manager-applet \
       networkmanager pavucontrol polkit-gnome pulseaudio scrot sxiv \
       xorg-xhost xorg-xrdb --noconfirm
  aura -A acpilight gtk-theme-numix-sx j4-dmenu-desktop \
       lightdm-settings lightdm-slick-greeter numix-icon-theme-git \
       powerline-fonts-git siji-git ttf-google-fonts-git --noconfirm

	systemctl enable lightdm
else
  echo -n "Installing packages..."
	aura -S emacs-nox --noconfirm
fi
aura -S cowsay htop mlocate openssh pkgfile ripgrep rsync sudo --noconfirm
aura -A oh-my-zsh --noconfirm

echo " Done."

echo -n "Updating package and locate databases..."
pkgfile --update
updatedb
echo " Done."
