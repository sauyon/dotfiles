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
  gets a CI job, and to whichever scale knob its panel wants: `hidpi` for 1.25
  app-level scaling, or `laptopScale` for a 2x compositor scale on `eDP-1`.
  Pick one — they multiply.
- `.forgejo/workflows/nix-home.yml`: add
  `.#homeConfigurations.<host>.activationPackage` to the build step.
- `system/deploy`: add `<host>` to the host case in the *host packages* step.
  It is an allow-list on purpose — a box we don't own the OS of must not get
  handed packages — so a host missing from it silently skips that step and says
  so when you run the deploy.

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

**On an AMD box pass `--hw`**, or it gets Intel microcode and Intel video
drivers:

```bash
mise run host:install <ip> <host> --hw 'amd-ucode vulkan-radeon libva-mesa-driver'
```

The package set is two files plus that flag: `system/packages` (what every host
must have — `system/deploy` keeps enforcing it afterwards),
`install/packages-install-only` (base, kernel, and enough shell to reach
`bootstrap`; never re-enforced), and `--hw` for the parts that would be wrong on
the other architecture. Both lists are scp'd to the ISO alongside the script.

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

For sops it needs the GCP key other hosts use, and fetches it from the cluster
rather than having you copy it: it shows a URL and a QR code for a Keycloak
sign-in, you approve on your phone (nothing to type), and it reads the key from
Secret `bootstrap/sops-gcp-key`. Put the key there once, from any host that has
it and a working `kubectl`:

```bash
kubectl create namespace bootstrap
kubectl -n bootstrap create secret generic sops-gcp-key \
  --from-file=gcp-key.json=$HOME/.config/sops/gcp-key.json
```

`bootstrap` is idempotent; rerun it if a step fails. In order it:

1. installs Determinate Nix if it's missing, then the sops sign-in above;
2. runs `system/deploy` — any missing packages from `system/packages`, oomd,
   polkit, the attic netrc, the remote-builder key, patched tailscaled — so the
   next step downloads CI's build;
3. does the Home Manager switch for `<host>` (via `nix run` the first time);
4. runs `tailscale up` if it isn't up;
5. removes the `99-bootstrap` sudoers drop-in from step 3.

Then log in again (`exec zsh -l` on a console that predates the switch) and
`start-hyprland`.

## 5. Afterwards

- Enroll a fingerprint if the box has a reader: `fprintd-enroll` (right index
  only — other fingers need `-f left-index-finger` and so on), then
  `fprintd-list $USER` to confirm. The enrollment is root state in
  `/var/lib/fprint`, per host, and nothing in this repo can carry it over. It
  unlocks hyprlock and the polkit/Bitwarden prompt; `sudo` and TTY login stay
  password-only on purpose.

  Enrollment itself needs the polkit agent already running, which is easy to
  miss because the failure names the wrong thing: `device.enroll` defaults to
  `auth_self_keep`, and with no agent registered polkit refuses instead of
  prompting, so `fprintd-enroll` dies with
  `net.reactivated.Fprint.Error.PermissionDenied` as though the account lacked
  permission. `hyprpolkitagent` (`home.nix`) supplies the agent, so enroll after
  the switch, from inside a graphical session — its unit is conditioned on
  `WAYLAND_DISPLAY`, so an ssh login has no agent either. To check before
  blaming the reader: `pkcheck --process $$ --action-id
  net.reactivated.fprint.device.enroll --allow-user-interaction` says outright
  when no agent is available.

  Two things change the moment a finger is enrolled, both of which read as
  regressions if you don't expect them: every polkit `auth_self` prompt now
  waits on the reader before offering a password field (including `pkexec` from
  a TTY, via the text agent), and because `pam_fprintd` is `sufficient` *ahead*
  of `system-auth` in `system/etc/pam.d/polkit-1`, a fingerprint bypasses
  `pam_faillock` — a locked account can still authorize polkit actions, and
  fingerprint attempts neither count toward the lockout nor clear it.
- Turn Secure Boot back on only if you also set up signing (sbctl); the base
  install doesn't sign anything.
- Delete the ISO, or keep it private: it has the Wi-Fi passphrase in it.
