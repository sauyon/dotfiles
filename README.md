# dotfiles

Home Manager configuration for `sauyon`.

## New machine setup

For a blank machine (ISO, disk, base Arch install), see
[docs/new-host.md](docs/new-host.md). The steps below assume Arch is already
running.

```bash
# 1. Clone into ~/devel/dotfiles
mkdir -p ~/devel
git clone https://github.com/sauyon/dotfiles ~/devel/dotfiles

# 2. If this is a new host, add a homeConfigurations entry for it in flake.nix
#    (hostname, gui, gpu). Otherwise the matching entry already exists.

# 3. Install Nix
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix \
  | sudo sh -s -- install --no-confirm

# 4. Source Nix and trust mise
. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
mise trust ~/devel/dotfiles/mise.toml

# 5. Apply (flake attr matches hostname)
nix run github:nix-community/home-manager -- switch --flake ~/devel/dotfiles#$HOST
```

After the first switch, `home-manager switch` is available directly.

## Applying changes: `hms`

`hms` is the normal way to switch. It does **not** build locally by default — it
pushes, waits for the commit's CI run, then switches, which turns the switch into
a download of the closure the runner already built and pushed to attic.

```bash
hms              # push, wait for the run, then switch
hms --local      # skip CI and build here (the old `home-manager switch` alias)
```

Things it deliberately refuses rather than works around:

- **A dirty tree.** CI builds a pushed commit, so an uncommitted switch is one CI
  can never reproduce. Commit, or use `--local`.
- **A stale checkout.** If `origin/master` has commits you do not, it says so and
  stops — rebasing is your call, and waiting on a run for a SHA that is not
  `origin/master` would be waiting on the wrong build.

It falls back to a local build, with a note, when the host has no CI job (mari)
or when the commit touched nothing `nix-home.yml`'s `paths:` filter matches. On a
red run it prints the job's actual `error:` lines, not just a status.

Why this is worth a wait: see `docs/ci-nix-home.md`. The short version is that
the runner has far more of everything than these boxes, and its output is
bit-identical to what a local build would produce.

## Tests

Nothing here has a suite; the exception is anything whose logic is a model of
someone else's, where a comment claiming the model is right proves nothing.

```bash
./tests/hyprlock-faillock.sh     # builds the script, then drives 33 cases
./tests/system-packages.sh       # sources system/pacman.sh, drives 21 cases
./tests/thermald-setup.sh        # drives 8 cases against system/thermald-setup
./tests/ghostty-p10k-prompt.sh   # drives 13 cases against the live generated zsh config
./tests/steam-ui-scaling.sh      # evaluates 3 hosts, drives 4 cases
```

`hyprlock-faillock` (in `home.nix`) reproduces pam_faillock's two tally windows
to tell the lock screen whether the account is locked out. The cases drive the
real built script through its `FAILLOCK_BIN` / `FAILLOCK_CONF` / `FAILLOCK_USER`
seams, so a stub reader supplies synthetic tally records -- no real failed
logins, no waiting out a real ten-minute lockout.

`system/pacman.sh` holds the parts of `system/deploy`'s package convergence that
model someone else's rules: the list format plus the per-host overlay, and the
single `Include` line appended to `/etc/pacman.conf` (whole-line matched, so the
stock config's commented-out examples don't read as "already enabled"). The
cases source it and run against a temp tree through its `SUDO` seam, then hand a
copy of this host's real `pacman.conf` to `pacman-conf` to confirm pacman does
glob an `Include` and does register a `[multilib]` section reached through one --
the assumption the whole drop-in design rests on. Nothing writes to `/etc`, and
no case needs root.

`system/thermald-setup` (run from `system/deploy`) is the *enable* half that a
package list cannot express: `pacman -S` installs a unit, it does not start one.
Which hosts want thermald is decided upstream of it, by `thermald` appearing in
`system/packages.<host>` — so the script's own gate is just "is it installed",
and a host whose list never asked for it is one it never touches. The cases drive
the real script with stub `pacman`/`sudo`/`systemctl` on `PATH`, asserting both
halves: a host without the package does nothing at all, and a converged host
escalates zero times.

The `_ghostty_saved_ps1` priming in `zsh.nix` is a model of ghostty's
`ghostty-integration`: it pre-sets variables private to that script so its own
`ps1_changed` guard fires on the first precmd, which is what stops the PS1
rewrite that corrupts powerlevel10k's `PROMPT` into a literal `}}`. Rename those
variables upstream and nix still builds, `hms` still succeeds, and the only
symptom is the artifact coming back. The cases render the *live generated* config
in a pty -- both the injected and plain startup paths -- and assert the `}}` is
gone with the line and still returns without it, so a workaround that has quietly
stopped working fails out loud, and so does one that has become unnecessary.

`STEAM_FORCE_DESKTOPUI_SCALING` (in `home.nix`) models Steam's side of a bargain
Hyprland can't enforce: `xwayland.force_zero_scaling` hands X11 clients real
pixels and no DPI hint, and Steam's CEF UI reads neither `Xft.dpi` nor
`GDK_DPI_SCALE`, so on a scaled panel it draws tiny until told its own factor.
The cases evaluate three hosts and derive every expectation from the config
itself — the var must equal the scale that host's eDP-1 monitor rule asks for,
and be unset where there is no such rule — so the pair can't drift apart
silently, which is the only way this fails. A fourth case asserts a scaled host
still exists, since otherwise all three would pass vacuously.

## System config

Files under `system/` mirror `/` and require root to deploy:

```bash
system/deploy
```

`system/packages` is the pacman list every Arch host must satisfy;
`system/packages.<hostname>` is layered on it for one box (shiori's `steam` and
its 32-bit drivers). Deploy converges pacman onto both on every run, and enables
multilib via `system/etc/pacman.d/conf.d/multilib.conf` plus one `Include` in
`/etc/pacman.conf`. A newly enabled repo has no sync db, so the first deploy
after this asks for a `sudo pacman -Syu` rather than refreshing behind your back
(`-Sy` without the `-u` is the partial-upgrade footgun).

## Storage tuning

One-shot tuning for the btrfs-on-LUKS-on-loop-on-ext4 home stack: sets LUKS
workqueue-bypass flags, `noatime` on host `/home`, grows btrfs into LUKS
device slack. Idempotent. The LUKS step prompts for the passphrase.

```bash
system/storage-tuning.sh
```

## Secrets (sops-nix)

Secrets are decrypted using `~/.ssh/id_ed25519` as an age identity. On a new machine, either copy your existing key or generate one and add it as a sops recipient:

```bash
# Add new machine key as recipient
cd ~/devel/dotfiles
ssh-keygen -t ed25519 -f ~/.ssh/id_ed25519
mise run sops -- updatekeys secrets.yaml
```
