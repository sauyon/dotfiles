#!/usr/bin/env bash
# One-shot storage tuning for this machine's btrfs-on-LUKS-on-loop-on-ext4 stack.
# Safe to re-run; each step is idempotent.
#
# What it does:
#   1. Sets LUKS dm-crypt perf-no_{read,write}_workqueue flags persistently in
#      the LUKS2 header. These bypass the kcryptd workqueues so encryption runs
#      inline on the caller's CPU and decryption in softirq. Big latency win
#      for fsync-heavy workloads on modern NVMe + AES-NI. See
#      https://blog.cloudflare.com/speeding-up-linux-disk-encryption/ (the
#      upstream flags landed in Linux 5.9; cryptsetup calls them
#      --perf-no_read_workqueue / --perf-no_write_workqueue).
#      Requires LUKS passphrase (you'll be prompted).
#
#   2. Ensures the host /home ext4 mount uses noatime instead of relatime.
#      noatime on the host ext4 is safe to combine with whatever atime policy
#      the inner btrfs uses (they're independent layers).
#
#   3. Grows the inner btrfs to fill the whole LUKS device if it isn't already.
#      systemd-homed grows the LUKS image file but doesn't always grow the
#      inner filesystem; this catches the gap.
#
# Assumptions: systemd-homed-managed home named "sauyon" (mapping "home-sauyon"),
# host /home on ext4. Adjust HOMED_USER below if reused on a different machine.

set -euo pipefail

HOMED_USER="${HOMED_USER:-sauyon}"
LUKS_MAPPING="home-${HOMED_USER}"
BTRFS_MOUNT="/home/${HOMED_USER}"
HOST_HOME_MOUNT="/home"

# --- 1. LUKS perf workqueue-bypass flags --------------------------------------

apply_luks_flags() {
	local backing current_flags
	backing=$(sudo cryptsetup status "${LUKS_MAPPING}" 2>/dev/null \
		| awk '/^[[:space:]]*device:/ {print $2; exit}')
	if [[ -z $backing ]]; then
		echo "[luks] could not find backing device for ${LUKS_MAPPING}; skipping" >&2
		return 1
	fi

	current_flags=$(sudo cryptsetup luksDump "$backing" 2>/dev/null \
		| awk '/^Flags:/ {sub(/^Flags:[ \t]*/, ""); print; exit}')

	if [[ $current_flags == *no-read-workqueue* && $current_flags == *no-write-workqueue* ]]; then
		echo "[luks] perf workqueue-bypass flags already set: $current_flags"
		return 0
	fi

	echo "[luks] backing device: $backing"
	echo "[luks] current header flags: ${current_flags:-<none>}"
	echo "[luks] applying --perf-no_read_workqueue --perf-no_write_workqueue (will prompt for passphrase)"
	sudo cryptsetup \
		--perf-no_read_workqueue \
		--perf-no_write_workqueue \
		--persistent \
		refresh "${LUKS_MAPPING}"
	echo "[luks] done. Takes effect on next homed activation (logout/login or reboot)."
}

# --- 2. noatime on host ext4 --------------------------------------------------

apply_noatime() {
	local current_opts
	current_opts=$(findmnt -no OPTIONS "${HOST_HOME_MOUNT}")
	if [[ $current_opts == *noatime* ]]; then
		echo "[host /home] already noatime"
	else
		echo "[host /home] switching relatime -> noatime"
		sudo sed -i.bak \
			-E "s|^(/dev/disk/by-partlabel/home[[:space:]]+${HOST_HOME_MOUNT}[[:space:]]+ext4[[:space:]]+rw,)relatime|\\1noatime|" \
			/etc/fstab
		sudo mount -o remount "${HOST_HOME_MOUNT}"
		findmnt "${HOST_HOME_MOUNT}" -o SOURCE,FSTYPE,OPTIONS
	fi
}

# --- 3. grow btrfs to fill LUKS device ----------------------------------------

grow_btrfs() {
	local slack
	slack=$(sudo btrfs filesystem usage --raw "${BTRFS_MOUNT}" 2>/dev/null \
		| awk '/Device slack:/ {print $3; exit}')
	if [[ -z $slack || $slack -lt $((100 * 1024 * 1024)) ]]; then
		echo "[btrfs] no significant slack to reclaim (slack=${slack:-?} bytes)"
		return 0
	fi
	echo "[btrfs] device slack: $((slack / 1024 / 1024 / 1024)) GiB; growing to max"
	sudo btrfs filesystem resize max "${BTRFS_MOUNT}"
}

apply_luks_flags
apply_noatime
grow_btrfs

echo
echo "done. To verify LUKS flags after next logout/login:"
echo "  sudo cryptsetup status ${LUKS_MAPPING} | grep flags"
echo "  # should show: flags: no-read-workqueue, no-write-workqueue, discards"
