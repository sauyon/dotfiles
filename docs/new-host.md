# New host runbook

From a blank machine to an Arch box running this flake. Everything after
booting the stick happens over SSH, so it can be driven from another machine.

The result: full-disk LUKS2 → btrfs (`@`, `@home`, `@nix`, `@log`, `@cache`,
zstd), systemd-boot, NetworkManager, user `sauyon` in `wheel`, Determinate Nix,
and the first Home Manager switch done.

## 0. Register the host in the flake

Pick a hostname, then in this repo:

- `flake.nix`: add `homeConfigurations.<host> = linuxHome { hostname = "<host>"; gui = true; };`
  (`gpu = "amd"`/`"nvidia"` only if it has one; anything else can be left out).
- `home.nix`: add `<host>` to the `hms` CI case (`utsuho|setsuna|...`) if it
  gets a CI job, and to the `hidpi` scale list if its panel wants 1.25.
- `.forgejo/workflows/nix-home.yml`: add
  `.#homeConfigurations.<host>.activationPackage` to the build step.

Commit and push; CI will have the closure in attic by the time you need it.

## 1. Build the install ISO

`install/` holds an archiso profile overlay on top of the stock `releng`
profile. The ISO auto-joins one Wi-Fi network (iwd) and allows key-only root
SSH, advertised over mDNS as `archiso.local`. Needs Docker (Docker Desktop is
fine; the build runs in a privileged `archlinux` container).

```bash
cp install/wifi.env.example install/wifi.env   # set WIFI_SSID / WIFI_PASSPHRASE
mise run host:iso                               # --out <dir>, default ~/Downloads
```

It uses `~/.ssh/id_ed25519.pub` as the root key unless `install/authorized_keys`
already exists. Both files are gitignored. The Wi-Fi passphrase ends up in
plain text inside the ISO — treat the ISO file accordingly.

## 2. Write it to a stick and boot

`dd` it (or Rufus in DD mode on Windows) and **verify by reading it back** — a
flaky stick or port produced scattered bad blocks once and the result would
not have booted reliably. Compare `sha256sum` of the ISO against the same
number of bytes read from the device.

On the new host: **disable Secure Boot** (archiso isn't signed; the failure
shows up as a bare "EFI boot failed"), then boot the `UEFI:` entry.

Find it with `ssh root@archiso.local`. If mDNS doesn't resolve, scan for SSH
(`nmap -p22 --open <lan>/24`) and pick the `OpenSSH` banner, or read `ip a`
off its screen.

## 3. Base install

Check the target disk first (`lsblk`) — the script refuses a disk that
already has partitions, so wipe it by hand (`sgdisk -Z`) only when you mean it.

```bash
mise run host:install <ip> <host>     # --disk <dev>, default /dev/nvme0n1
```

It asks for the disk passphrase and `sauyon`'s password up front, then runs
unattended and ends with `BASE INSTALL DONE`. Along the way it copies the live
system's Wi-Fi into a NetworkManager connection, installs your SSH key for
`sauyon` (key-only sshd, no root login), and leaves a temporary `NOPASSWD`
sudoers drop-in (`99-bootstrap`) for step 4.

Pull the stick and reboot; it asks for the disk passphrase at boot.

## 4. Nix and Home Manager

The host rejoins Wi-Fi via NetworkManager and accepts your key:

```bash
mise run host:setup <ip>
```

That installs Determinate Nix, clones this repo to `~/devel/dotfiles`, runs the
first `home-manager switch --flake ...#<host>` (local build: the attic pull
token isn't on the host yet), and deletes the `99-bootstrap` sudoers drop-in.
`sops-nix.service` failing at the end of that switch is expected until step 5.

If you were logged in on the console before this ran, log in again
(`exec zsh -l`): that session predates the Home Manager profile and has none
of its PATH or zsh config.

## 5. Secrets and system config (needs you)

These need credentials that should not be copied around by a script:

1. Put the sops GCP key at `~/.config/sops/gcp-key.json` (see *Secrets* in the
   README; for an age identity, add the host's key with
   `mise run sops -- updatekeys secrets.yaml`).
2. `mise run system:deploy` — oomd, polkit, the attic netrc, the remote-builder key and
   patched tailscaled. After this, `hms` pulls from attic instead of building.
3. `sudo tailscale up`.
4. Turn Secure Boot back on only if you also set up signing (sbctl); the base
   install doesn't sign anything.
