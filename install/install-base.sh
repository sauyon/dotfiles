#!/usr/bin/env bash
# Base Arch install for a new host, run as root on the archiso live system:
#   HOST=<name> DISK=/dev/nvme0n1 bash install-base.sh
# Normally invoked via `mise run host:install <ip> <host>`.
# Full-disk LUKS2 -> btrfs, systemd-boot, NetworkManager, user sauyon.
# Prompts for the disk passphrase and the user's password first, then runs
# unattended.
set -euo pipefail

DISK=${DISK:?set DISK, e.g. /dev/nvme0n1}
part() { [[ $DISK == *[0-9] ]] && echo "${DISK}p$1" || echo "${DISK}$1"; }
ESP=$(part 1)
CRYPT=$(part 2)
HOST=${HOST:?set HOST}
USERNAME=${USERNAME:-sauyon}
TZ_NAME=${TZ_NAME:-America/Los_Angeles}

# Refuse to touch a disk that already has partitions; wipe it by hand first
# (sgdisk -Z) if you really mean it.
[[ -b $DISK ]] || { echo "$DISK is not a block device" >&2; exit 1; }
[[ $(lsblk -no NAME "$DISK" | wc -l) -eq 1 ]] || { echo "$DISK already has partitions" >&2; exit 1; }
lsblk -dno NAME,SIZE,MODEL "$DISK"

ask() {
  local a b
  while :; do
    read -rsp "$1: " a; echo >&2
    read -rsp "$1 (again): " b; echo >&2
    [[ -n $a && $a == "$b" ]] && break
    echo "empty or didn't match, try again" >&2
  done
  printf '%s' "$a"
}
luks_pass=$(ask "Disk passphrase")
user_pass=$(ask "Password for $USERNAME")

timedatectl set-ntp true

sgdisk -Z "$DISK"
sgdisk -n1:0:+1G -t1:EF00 -c1:EFI -n2:0:0 -t2:8309 -c2:cryptroot "$DISK"
partprobe "$DISK"; udevadm settle

printf '%s' "$luks_pass" | cryptsetup luksFormat --batch-mode --type luks2 --key-file - "$CRYPT"
printf '%s' "$luks_pass" | cryptsetup open --key-file - --allow-discards \
  --perf-no_read_workqueue --perf-no_write_workqueue --persistent "$CRYPT" root

mkfs.fat -F32 -n EFI "$ESP"
mkfs.btrfs -f -L "$HOST" /dev/mapper/root

mount /dev/mapper/root /mnt
for sv in @ @home @nix @log @cache; do btrfs subvolume create "/mnt/$sv"; done
umount /mnt
opts=noatime,compress=zstd:1,space_cache=v2
mount -o "$opts,subvol=@" /dev/mapper/root /mnt
mkdir -p /mnt/{boot,home,nix,var/log,var/cache}
mount -o "$opts,subvol=@home" /dev/mapper/root /mnt/home
mount -o "$opts,subvol=@nix" /dev/mapper/root /mnt/nix
mount -o "$opts,subvol=@log" /dev/mapper/root /mnt/var/log
mount -o "$opts,subvol=@cache" /dev/mapper/root /mnt/var/cache
mount -o umask=0077 "$ESP" /mnt/boot

pacstrap -K /mnt \
  base base-devel linux linux-headers linux-firmware intel-ucode sof-firmware \
  btrfs-progs cryptsetup dosfstools \
  networkmanager openssh sudo zsh git vim man-db fzf \
  mesa vulkan-intel intel-media-driver \
  pipewire pipewire-pulse pipewire-alsa wireplumber \
  polkit tpm2-tss tpm2-tools bluez bluez-utils fwupd power-profiles-daemon

genfstab -U /mnt >> /mnt/etc/fstab

# Carry the live system's Wi-Fi over as a NetworkManager connection.
psk_file=$(ls /var/lib/iwd/*.psk | head -1)
ssid=$(basename "$psk_file" .psk)
pass=$(sed -n 's/^Passphrase=//p' "$psk_file")
install -m600 /dev/stdin "/mnt/etc/NetworkManager/system-connections/${ssid}.nmconnection" <<EOF
[connection]
id=${ssid}
type=wifi
autoconnect=true

[wifi]
mode=infrastructure
ssid=${ssid}

[wifi-security]
key-mgmt=wpa-psk
psk=${pass}

[ipv4]
method=auto

[ipv6]
method=auto
EOF

luks_uuid=$(blkid -s UUID -o value "$CRYPT")

arch-chroot /mnt /bin/bash -euo pipefail <<CHROOT
ln -sf /usr/share/zoneinfo/${TZ_NAME} /etc/localtime
hwclock --systohc
sed -i 's/^#en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/; s/^#en_DK.UTF-8 UTF-8/en_DK.UTF-8 UTF-8/' /etc/locale.gen
locale-gen
printf 'LANG=en_US.UTF-8\nLC_TIME=en_DK.UTF-8\n' > /etc/locale.conf
echo ${HOST} > /etc/hostname
printf '127.0.0.1\tlocalhost\n::1\t\tlocalhost\n127.0.1.1\t${HOST}.localdomain\t${HOST}\n' >> /etc/hosts

echo 'KEYMAP=us' > /etc/vconsole.conf
sed -i 's/^HOOKS=.*/HOOKS=(base systemd autodetect microcode modconf kms keyboard sd-vconsole block sd-encrypt filesystems fsck)/' /etc/mkinitcpio.conf
mkinitcpio -P

bootctl install
cat > /boot/loader/loader.conf <<EOF
default arch.conf
timeout 3
editor no
EOF
cat > /boot/loader/entries/arch.conf <<EOF
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
options rd.luks.name=${luks_uuid}=root rd.luks.options=discard root=/dev/mapper/root rootflags=subvol=@ rw
EOF

useradd -m -G wheel,video,input,tss -s /bin/zsh ${USERNAME}
echo '%wheel ALL=(ALL:ALL) ALL' > /etc/sudoers.d/10-wheel
chmod 440 /etc/sudoers.d/10-wheel
# Passwordless sudo only until "mise run bootstrap" finishes; it deletes this.
echo '${USERNAME} ALL=(ALL:ALL) NOPASSWD: ALL' > /etc/sudoers.d/99-bootstrap
chmod 440 /etc/sudoers.d/99-bootstrap

# Key-only SSH for sauyon, for running bootstrap from another machine.
install -d -m700 -o ${USERNAME} -g ${USERNAME} /home/${USERNAME}/.ssh
printf 'PermitRootLogin no\nPasswordAuthentication no\nKbdInteractiveAuthentication no\n' > /etc/ssh/sshd_config.d/00-keys-only.conf

systemctl enable NetworkManager sshd systemd-timesyncd fstrim.timer bluetooth power-profiles-daemon systemd-oomd
CHROOT

printf '%s:%s\n' "$USERNAME" "$user_pass" | arch-chroot /mnt chpasswd

# authorized_keys lives on the live system, not inside the chroot.
install -m600 -o 1000 -g 1000 /root/.ssh/authorized_keys /mnt/home/${USERNAME}/.ssh/authorized_keys

umount -R /mnt
cryptsetup close root
echo "BASE INSTALL DONE - pull the stick and reboot"
