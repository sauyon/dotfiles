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

pacman -Syu --noconfirm
pacman -S zsh git base-devel --noconfirm

useradd "$1" -s /bin/zsh -G wheel network
passwd "$1"

git clone https://aur.archlinux.org/aura-bin
cd aura-bin
sudo -u "$1" makepkg -si

aura -S pkgfile sxiv bc mlocate mercurial thefuck --noconfirm
aura -A oh-my-zsh compton ttf-google-fonts-git --noconfirm
aura -S chomium synapse feh --noconfirm

pkgfile --update

if getrep "Install xorg/dm/etc? [Y/n] "; then
	aura -S xorg emacs compton sddm networkmanager network-manager-applet polkit-gnome xorg-xbacklight mpv

	if [[ -z egrep -i 'synap' /proc/bus/input/devices ]]; then
		aura -S xf86-input-synaptics
	elif [[ -z egrep -i 'etps' /proc/bus/input/devices ]]; then
	fi
	systemctl enable sddm
else
	aura -S emacs-nox
fi
