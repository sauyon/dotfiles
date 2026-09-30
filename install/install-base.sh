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

# Hardware packages, overridable per host. These are the ones that would be
# wrong rather than merely unused on the other architecture, which is why they
# are NOT in system/packages -- system/deploy re-enforces that file on every run
# and must not push Intel drivers at the AMD ones. Two of our hosts are AMD
# (utsuho, fujiwara; see flake.nix), so this gets overridden about as often as
# it does not; docs/new-host.md step 0 says so.
#   HW_PKGS='amd-ucode vulkan-radeon libva-mesa-driver' mise run host:install ...
# `-` not `:-`, so HW_PKGS='' is honoured as "no hardware packages" (a VM) rather
# than silently meaning Intel. This default is the only one; the --hw flag in
# mise.toml deliberately has none.
HW_PKGS=${HW_PKGS-intel-ucode vulkan-intel intel-media-driver}

# Two package lists, both resolved HERE -- before the passphrase prompts and
# before anything touches the disk, so a missing or unparseable list fails while
# the target is still intact.
#   system/packages            what every host must have; system/deploy enforces
#                              the same file on every run afterwards.
#   install/packages-install-only  fresh-install scaffolding, never re-enforced.
# `mise run host:install` scp's both next to this script; a plain repo checkout
# finds system/packages one level up.
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

# Strip comments, then split on whitespace so a two-names-on-one-line slip
# becomes two packages rather than one fused, nonexistent name.
#
# Read through a command substitution, NOT a process substitution: `$(...)` puts
# the subshell's failure into the assignment's status, where `set -e` sees it, so
# a read that dies partway through aborts the install. `mapfile < <(...)` would
# report success with however many lines it managed -- and a truncated list is
# worse than an absent one, because pacstrap succeeds on the subset and the
# operator gets BASE INSTALL DONE on a host with no cryptsetup.
read_pkglist() {
  local f
  for f in "$@"; do
    if [ -r "$f" ]; then
      sed 's/#.*//' "$f" | tr -s '[:space:]' '\n' | sed '/^$/d'
      return 0
    fi
  done
  echo "install-base.sh: no readable package list at any of: $*" >&2
  return 1
}

# Every name that reaches pacstrap gets checked for a leading dash first.
# pacstrap's own option parsing stops at /mnt and it hands the rest to pacman
# inside the chroot, so a `-`-prefixed entry -- from a mangled list or from a
# pasted --hw -- would be read there as an option (--hookdir= runs arbitrary
# code as root via an alpm hook, --root= and --overwrite= are no better). A `--`
# after /mnt would be the usual answer, but whether pacstrap forwards it to
# pacman rather than choking on it is version-dependent, and this script gets
# exactly one run per machine. Rejecting the input needs no such assumption, and
# happens up here where nothing has touched the disk yet.
check_pkgnames() {
  local p
  for p in "$@"; do
    case $p in
    -*) echo "install-base.sh: refusing package name starting with '-': $p" >&2; return 1 ;;
    esac
  done
}

# Word-split HW_PKGS with `read -ra` rather than leaving it unquoted: splitting
# is wanted, globbing against the ISO's cwd is not. Newlines are flattened first
# because `read` stops at the first one, and a pasted value can arrive wrapped.
read -ra hw_pkgs <<<"${HW_PKGS//$'\n'/ }"

# The assignments abort on a failed or partial read (see read_pkglist). The
# emptiness checks catch the other half: a file that is readable and parses to
# nothing. They test the strings, not the arrays -- `mapfile <<<""` yields one
# empty element, so a count of zero never happens here.
pkgs_raw=$(read_pkglist "$here/packages" "$here/../system/packages")
boot_raw=$(read_pkglist "$here/packages-install-only")
[ -n "$pkgs_raw" ] || { echo "install-base.sh: system/packages parsed to nothing" >&2; exit 1; }
[ -n "$boot_raw" ] || { echo "install-base.sh: packages-install-only parsed to nothing" >&2; exit 1; }
mapfile -t pkgs <<<"$pkgs_raw"
mapfile -t boot_pkgs <<<"$boot_raw"
check_pkgnames "${pkgs[@]}" "${boot_pkgs[@]}" "${hw_pkgs[@]}"

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

pacstrap -K /mnt "${pkgs[@]}" "${boot_pkgs[@]}" "${hw_pkgs[@]}"

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

# Key-only SSH for sauyon, for running bootstrap from another machine. This is a
# bootstrap affordance with a deliberate end: `mise run bootstrap` disables sshd
# on exit unless a working ssh-oidc gate is installed (install/finalize-ssh.sh).
# Do not read the `systemctl enable sshd` below as this host's finished posture.
install -d -m700 -o ${USERNAME} -g ${USERNAME} /home/${USERNAME}/.ssh
printf 'PermitRootLogin no\nPasswordAuthentication no\nKbdInteractiveAuthentication no\n' > /etc/ssh/sshd_config.d/00-keys-only.conf

systemctl enable NetworkManager sshd systemd-timesyncd fstrim.timer bluetooth power-profiles-daemon systemd-oomd
CHROOT

printf '%s:%s\n' "$USERNAME" "$user_pass" | arch-chroot /mnt chpasswd

# authorized_keys lives on the live system, not inside the chroot.
install -m600 -o 1000 -g 1000 /root/.ssh/authorized_keys /mnt/home/${USERNAME}/.ssh/authorized_keys

# Clone the dotfiles and add a `bootstrap` command, so after reboot the whole
# second stage is: log in, type bootstrap.
arch-chroot /mnt sudo -u "${USERNAME}" git clone -q https://forge.ko.ag/sauyon/dotfiles.git "/home/${USERNAME}/devel/dotfiles"
install -m755 /mnt/home/${USERNAME}/devel/dotfiles/install/bootstrap /mnt/usr/local/bin/bootstrap

umount -R /mnt
cryptsetup close root
echo "BASE INSTALL DONE - pull the stick and reboot"
