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

That sshd is a bootstrap affordance, not the finished posture: step 4 disables it
again unless a working ssh-oidc gate is installed. If you want this host to keep
accepting ssh, install the gate before or during step 4 — see step 5.

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
4. runs `tailscale up` if it isn't up — Tailscale here is the **work** VPN
   (`tail1beac.ts.net`); the patched systemd unit + the
   `tailscale-ssh-skip-logind-session` patch are a work-VPN concern, not
   anything residential. The personal tailnet (`alai-ionian`) is
   intentionally empty in this repo, and a host that does not need the work
   hop (most non-workstation hosts) can stay logged out — `system/deploy`
   still leaves tailscaled running but inactive, which is the intended
   posture for `shiori`. **mari is not reached over Tailscale** — see
   `docs/darwin-remote-builder.md`;
5. on exit, however it exits: runs `install/finalize-ssh.sh` (disables sshd
   unless a working ssh-oidc gate is installed), then removes the
   `99-bootstrap` sudoers drop-in from step 3.

Then log in again (`exec zsh -l` on a console that predates the switch) and
`start-hyprland`.

## 5. Afterwards

- Seal the gnome-keyring passphrase to this host's TPM. Nothing in the repo can
  do it for you — the sealed blob is per-machine by construction — and until it
  exists there is no Secret Service worth the name:

  ```bash
  gnome-keyring-tpm-seal
  systemctl --user restart gnome-keyring
  ```

  No secret goes in and none comes out: it generates 32 random bytes, seals them
  to this host's TPM, and verifies the round-trip. The passphrase is escrowed
  nowhere on purpose, which is what makes "useless off this machine" true — it was
  briefly kept in `secrets.yaml`, where one KMS-decryptable value opened every
  host's keyring file and a fresh box could not enrol until it could decrypt
  someone else's secret.

  The price is that there is no recovery: a cleared TPM leaves the keyring
  unreadable for good. That is cheap here because of *what* it holds — a Bitwarden
  refresh token, a huggingface token, fj's store — all replaced by signing in
  again. So `gnome-keyring-tpm-seal` refuses when a `login.keyring` already exists
  rather than silently making it unopenable. `--force` is the "yes, I am losing
  those secrets" flag: it seals, then moves the old keyring to
  `login.keyring.superseded-<timestamp>`, because sealing while leaving it in place
  would give the daemon a collection it cannot unlock and bring the prompts
  straight back. It moves `user.keystore` too — that is the PKCS#11 half, its unlock
  secret lives *inside* the login keyring, and the daemon runs both components.

  On a host enrolled **before** this changed, none of the above has happened yet:
  its sealed blob still holds the old shared passphrase, and that value is still in
  git history where its recipients open it. Such a host only gets the per-host
  property by re-enrolling — `gnome-keyring-tpm-seal --force`, then the restart —
  which costs it the keyring, so do it when signing back into Bitwarden and the git
  credential helper is convenient rather than in the middle of something.

  What it looks like when this step is skipped: `gnome-keyring-tpm` logs
  `no sealed passphrase … starting daemon WITHOUT TPM unlock` and degrades to the
  stock daemon, no login collection is ever created, and every attempt to store a
  persistent secret needs a prompt that no prompter answers — so it fails with
  `DBus error Prompt was dismissed`. Downstream that surfaces as symptoms that
  name the wrong component: Bitwarden silently offers no biometric-unlock option
  and logs `storing refresh token in secure storage failed`, and the git
  credential helper cannot persist anything.

  Then check *which* daemon answers, because sealing is not sufficient on a box
  that has been up a while:

  ```bash
  busctl --user list | grep org.freedesktop.secrets   # note the PID
  cat /proc/<pid>/cgroup                              # want gnome-keyring.service
  ```

  A daemon started by D-Bus activation can be squatting on
  `org.freedesktop.secrets` from an `app-dbus-*` scope. Masking Arch's
  `gnome-keyring-daemon.{socket,service}` in `system/deploy` does nothing about
  that path — both that file and `home.nix` say so — and the window is this
  runbook's own ordering: anything that asks for a secret between the switch and
  the seal above activates the TPM wrapper, which finds no sealed passphrase, falls
  back to `gnome-keyring-daemon --start` with no unlock, and then holds the bus name
  with no login collection. So the squatter usually *postdates* the masking; don't
  go looking for an unmasked unit. The symptom is `secret-tool` reporting
  `Object does not exist at path "/org/freedesktop/secrets/collection/login"` while
  `Collections` lists exactly that path, and `systemctl --user restart
  gnome-keyring` cannot dislodge it because the squatter is not that unit's child.
  Rebooting is the clean fix (a fresh session starts the unit at
  `graphical-session-pre.target` before anything can activate a daemon);
  `kill <that pid>` followed by restarting the unit is the impatient one.

  Verify functionally rather than by introspection, which lies here:

  ```bash
  printf 'x\n' | secret-tool store --label=probe probe check \
    && secret-tool lookup probe check && secret-tool clear probe check
  ```

- Enroll a fingerprint if the box has a reader: `fprintd-enroll` (right index
  only — other fingers need `-f left-index-finger` and so on), then
  `fprintd-list $USER` to confirm. The host side of the enrollment is root state
  in `/var/lib/fprint`, per host, and nothing in this repo can carry it over —
  and on a match-on-chip reader that is only half the story, since the template
  itself lives on the sensor (see the duplicate case below). It unlocks hyprlock
  and the polkit/Bitwarden prompt; `sudo` and TTY login stay password-only on
  purpose.

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

  That check only distinguishes *no* agent from a *broken* one by the absence of
  a message, so when it stays quiet and enrollment still fails, the next command
  is `coredumpctl list hyprpolkitagent`. An agent that dies mid-challenge is
  reported by polkitd as the operator failing to authenticate, which reaches you
  as the identical `PermissionDenied`, and `RestartSec` has the unit back to
  `active (running)` five seconds later — so `systemctl --user status` will lie
  to you here and the core dump will not. `journalctl --user -u
  hyprpolkitagent` around the attempt names the reason.

  The other way enrollment fails is `enroll-duplicate`, and it contradicts
  `fprintd-list` to your face: the reader is match-on-chip (shiori's is a
  `Goodix MOC Fingerprint Sensor`), so templates live *on the sensor*, not in
  `/var/lib/fprint`. The chip survives OS reinstalls, so a reinstalled box can hold
  a template the host database knows nothing about — `fprintd-list` says "no
  fingers enrolled" while the sensor says "already enrolled". Its records do carry
  a user id (the driver's error names it: "already enrolled as '<user_id>'"), but
  the duplicate check is not scoped to yours, so do not reason about it as a
  per-user namespace.

  Note what does *not* fix it: `fprintd-delete $USER` checks the **host** store
  first and returns `net.reactivated.Fprint.Error.NoEnrolledPrints` when it is
  empty, without touching the device — so in exactly the state above it is a
  no-op, not a way to clear the sensor. (It also needs `device.enroll`, so it
  wants the polkit agent too.) Deleting *other users'* prints is what helped here,
  because fprintd runs its pre-enroll duplicate check against every user's prints,
  not just yours — a print belonging to root is enough to refuse yours:

  ```bash
  sudo fprintd-delete root   # if a root print exists -- see the sudo warning below
  fprintd-enroll
  ```

  If that still refuses, the stale template is one nothing on this host has a
  record of, and clearing it is not something the fprintd CLI exposes. Enrolling a
  different finger (`fprintd-enroll -f left-index-finger`) is the cheap way past
  it. The exact conditions under which libfprint clears on-chip storage are not
  established here, so do not assume a command wipes the sensor unless you have
  watched it do so.

  Do **not** reach for `sudo fprintd-enroll` when the unprivileged one is refused.
  The print it creates cannot be moved to your user — both the on-chip record and
  the host file are bound to root — and per the duplicate check above, a root print
  then blocks *your* enrollment until it is deleted, so it actively makes the
  problem worse. It does **not** hand root a way past admin prompts, despite how it
  looks: polkit's admin identity here is `unix-group:wheel`
  (`/usr/share/polkit-1/rules.d/50-default.rules`) and root is not a member, so
  `auth_admin` authenticates as `sauyon` either way.

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
