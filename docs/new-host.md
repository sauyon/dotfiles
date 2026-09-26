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

## 4. Bootstrap

The base install already cloned this repo to `~/devel/dotfiles` and put a
`bootstrap` command in `/usr/local/bin`. Log in and run it:

```bash
bootstrap
```

On a machine that was not installed this way, `curl -fsSL ko.ag/bootstrap | bash`
clones the repo first and does the same. Either way it runs `mise run bootstrap`
through the committed `bin/mise` (from `mise generate bootstrap`: a pinned,
checksummed mise), so nothing else needs to be installed first.

For sops it signs you in to Google instead of needing a key file copied over:
it prints a URL and a QR code, you sign in on your phone and paste the code
back. Your account needs KMS access for that, granted once from any machine
where you're logged in to gcloud: `mise run gcp-grant-user`.

`bootstrap` is idempotent; rerun it if a step fails. In order it:

1. installs Determinate Nix if it's missing, then the sops sign-in above;
2. runs `system/deploy` — oomd, polkit, the attic netrc, the remote-builder
   key, patched tailscaled — so the next step downloads CI's build;
3. does the Home Manager switch for `<host>` (via `nix run` the first time);
4. runs `tailscale up` if it isn't up;
5. removes the `99-bootstrap` sudoers drop-in from step 3.

Then log in again (`exec zsh -l` on a console that predates the switch) and
`start-hyprland`.

## 5. Afterwards

- Turn Secure Boot back on only if you also set up signing (sbctl); the base
  install doesn't sign anything.
- Delete the ISO, or keep it private: it has the Wi-Fi passphrase in it.
