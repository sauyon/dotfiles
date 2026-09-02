# dotfiles

Home Manager configuration for `sauyon`.

## New machine setup

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

## System config

Files under `system/` mirror `/` and require root to deploy:

```bash
system/deploy
```

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
