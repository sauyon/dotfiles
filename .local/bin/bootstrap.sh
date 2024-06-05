#!/usr/bin/env bash

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
 - configure the internet
 - configure the pacman mirrorlist
 - setup the bootloader
EOF
if ! getrep "Continue? [Y/n]: "; then
  exit 1
fi

if getrep "Run first-time setup? [Y/n]: "; then
  pacman -Syu --noconfirm
  pacman -S zsh git sudo base-devel fzf go --noconfirm --needed

  echo "Select a timezone:"
  pushd /usr/share/zoneinfo
  timezone=$(fzf)
  popd
  ln -sf "$timezone" /etc/localtime

  echo "Select locale:"
  locale=$(sed -n '/#[^ ]/ s/^#//gp' /etc/locale.gen | fzf)
  echo "$locale" >> /etc/locale.gen
  echo "en_DK.UTF-8 UTF-8" >> /etc/locale.gen
  locale-gen
  echo "LANG=$locale" >> /etc/locale.conf
  echo "LC_TIME=en_DK.UTF-8" >> /etc/locale.conf

  hwclock --systohc

  timedatectl set-ntp true

  read -p "Enter hostname: "
  echo "$REPLY" > /etc/hostname

  echo "127.0.0.1	localhost" >> /etc/hosts
  echo "::1		localhost" >> /etc/hosts
  echo "127.0.1.1	$REPLY.localdomain	$REPLY" >> /etc/hosts

  echo "Setting password for root:"
  passwd

  echo "%wheel ALL=(ALL) ALL" >> /etc/sudoers

  useradd -s /bin/zsh -g users -G wheel,network,video,disk "$1"
  echo "Setting password for $1:"
  passwd "$1"

  mkdir -p "/home/$1"
  git clone https://github.com/sauyon/dotfiles "/home/$1"
  chown "$1":users "/home/$1" -R

  cd "/home/$1"
  echo -n "Building yay..."
  sudo -u "$1" git clone https://aur.archlinux.org/yay
  cd yay
  sudo -u "$1" makepkg
  pkg=$(sudo -u "$1" makepkg --packagelist)
  pacman -U "$pkg"a
  echo " Done."

  cd ..
  rm -rf yay
fi

if getrep "Install xorg/dm/etc? [Y/n] "; then
  echo -n "Installing packages..."
  # needs to be first to install before gdm dependencies
  sudo -u "$1" \
  yay -S ttf-google-fonts-git --noconfirm --needed

  pacman -S alacritty dunst grim emacs feh firefox noto-fonts sway swayidle \
         swaylock lxappearance mako network-manager-applet networkmanager \
         noto-fonts-cjk noto-fonts-emoji noto-fonts-extra pam-u2f \
         pavucontrol pcscd playerctl polkit-gnome pulseaudio quodlibet scrot \
         slurp sxiv waybar yubikey-manager yubikey-personalization kanshi \
         waybar wofi element-desktop font-awesome pipewire drive vscode \
         otf-font-awesome \
      --noconfirm --needed

  # AUR packages
  sudo -u "$1" \
  yay -S acpilight j4-dmenu-desktop \
         powerline-fonts-git siji-git ttf-material-design-icons-webfont \
         discord zoom \
      --noconfirm --needed

  systemctl enable pcscd.service
else
  echo -n "Installing packages..."
  sudo -u "$1" \
	yay -S emacs-nox --noconfirm --needed
fi

sudo -u "$1" \
byay -S cowsay htop mlocate openssh p7zip pkgfile ripgrep rsync tldr bat \
       ripgrep bfs htop btop elvish rm-improved psmisc github-cli \
     --noconfirm --needed

sudo -u "$1" \
yay -S oh-my-zsh-git --noconfirm --needed

sudo -u "$1" systemctl --user enable emacs.service

echo " Done."

echo -n "Updating package and locate databases..."
pkgfile --update
updatedb
echo " Done."
