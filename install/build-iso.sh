#!/usr/bin/env bash
# Runs inside a privileged archlinux container. Builds the releng profile plus
# auto Wi-Fi (iwd) and key-only root SSH, advertised over mDNS as archiso.local.
#   /src  - this install/ directory (wifi.env, authorized_keys, overlay/)
#   /out  - where the finished ISO is copied
set -euo pipefail

pacman -Syu --noconfirm --needed archiso

cp -r /usr/share/archiso/configs/releng /profile
cp -r /src/overlay/. /profile/airootfs/

# SSH: key-only root login.
install -d -m 700 /profile/airootfs/root/.ssh
install -m 600 /src/authorized_keys /profile/airootfs/root/.ssh/authorized_keys

# Wi-Fi: iwd network profile, named per iwd's rules (hex-encoded if the SSID
# has anything besides alphanumerics, space, '_' or '-').
# shellcheck disable=SC1091
source <(tr -d '\r' < /src/wifi.env)
: "${WIFI_SSID:?set WIFI_SSID in wifi.env}" "${WIFI_PASSPHRASE:?set WIFI_PASSPHRASE in wifi.env}"
if [[ $WIFI_SSID =~ ^[A-Za-z0-9\ _-]+$ ]]; then
  psk_name="$WIFI_SSID"
else
  psk_name="=$(printf '%s' "$WIFI_SSID" | od -An -tx1 | tr -d ' \n')"
fi
install -d -m 700 /profile/airootfs/var/lib/iwd
printf '[Security]\nPassphrase=%s\n' "$WIFI_PASSPHRASE" > "/profile/airootfs/var/lib/iwd/${psk_name}.psk"

psk_path="/var/lib/iwd/${psk_name}.psk"
sed -i "/^file_permissions=(/a\\
  [\"/root/.ssh\"]=\"0:0:700\"\\
  [\"/root/.ssh/authorized_keys\"]=\"0:0:600\"\\
  [\"/var/lib/iwd\"]=\"0:0:700\"\\
  [\"${psk_path}\"]=\"0:0:600\"" /profile/profiledef.sh
sed -i 's/^iso_name=.*/iso_name="archlinux-autossh"/' /profile/profiledef.sh
# xz squashfs dies under Docker Desktop's memory limits; zstd is lighter.
sed -i "s/^airootfs_image_tool_options=.*/airootfs_image_tool_options=('-comp' 'zstd' '-Xcompression-level' '15' '-b' '1M')/" /profile/profiledef.sh

mkarchiso -v -w /work -o /build /profile
cp /build/*.iso /out/
ls -la /out/archlinux-autossh-*.iso
