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
./tests/ghostty-p10k-prompt.sh   # drives 13 cases against the live zsh config
./tests/steam-ui-scaling.sh      # evaluates 3 hosts, drives 4 cases
./tests/hms-ci-poll.sh           # drives 7 cases against the built hms
./tests/hyprland-zen-popup.sh    # drives 7 cases against the live generated hyprland.lua
./tests/polkit-agent.sh          # evaluates 5 hosts + a synthetic one, 17 cases
./tests/insecure-packages.sh     # 2 cases per host, plus mari's darwin system
./tests/steam-env.sh             # builds the steam wrapper, drives 46 cases
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
in a pty and assert the `}}` is gone with the line and still returns without it,
so a workaround that has quietly stopped working fails out loud, and so does one
that has become unnecessary.

Which of the two shell startup paths is involved is the subtle part, and the
cases pin it down rather than assuming: the artifact only appears when the
integration is sourced from `.zshrc`, after p10k, and never when ghostty hands
the shell over via `ZDOTDIR` before it. Driving that second path takes
`SHELL=/bin/sh`, because `script -c` otherwise runs the command through zsh and
that outer shell quietly eats the handoff -- so two cases check the paths are
still distinct before the rest trusts them.

`STEAM_FORCE_DESKTOPUI_SCALING` (in `home.nix`) models Steam's side of a bargain
Hyprland can't enforce: `xwayland.force_zero_scaling` hands X11 clients real
pixels and no DPI hint, and Steam's CEF UI reads neither `Xft.dpi` nor
`GDK_DPI_SCALE`, so on a scaled panel it draws tiny until told its own factor.
The cases evaluate three hosts and derive every expectation from the config
itself — the var must equal the scale that host's eDP-1 monitor rule asks for,
and be unset where there is no such rule — so the pair can't drift apart
silently, which is the only way this fails. A fourth case asserts a scaled host
still exists, since otherwise all three would pass vacuously.

`hyprpolkitagent` (in `home.nix`) is the unit whose absence is silent: polkit has
no prompt of its own, so with no agent registered it refuses every `auth_self`
action outright — no dialog, nothing in polkitd's journal. That is what makes
`fprintd-enroll` fail on a fresh box and what would make Bitwarden's "unlock with
system authentication" fail the same way, which in turn means
`system/etc/pam.d/polkit-1` is never reached and its fingerprint wiring reads as
broken when it is only unreachable. `pkexec` hid this for a long time by carrying
its own text agent. The cases evaluate the flake's home configs rather than the
running host: that the unit exists on a `gui = true` host, that its `ExecStart`
goes through both `nixGL` and the `withHostNss` join, that `Install.WantedBy` is
set so a `.wants` link actually gets made, that `Restart`/`RestartSec`/
`StartLimitIntervalSec` are pinned as one trio, and that headless and Darwin hosts
get no unit at all. The trio is arithmetic, not taste: systemd's 100ms default
burns this host's five-attempt budget in half a second and gives up for the
session, while a 5s delay overcorrects so far that the limiter becomes
*unreachable* and a permanently broken agent restarts forever reading
`active (running)`. Both ends of that reproduce the very silence the unit removes,
so the window is widened until five failures stick. An eval that *fails* aborts
the run rather than being read as "the unit is absent", which is what the gating
cases would otherwise have called a pass.

Which GL stack a desktop host's nix GUI apps get is one decision, `glWrapper`, with
two consumers that are reached by different code paths: `targets.genericLinux.nixGL.defaultWrapper`
for everything going through `config.lib.nixGL.wrap` (ghostty, hyprlock, hyprpaper,
cumora), and the `nixGL` shim for hyprpolkitagent's `ExecStart` and the `nixGL` on
PATH. It is derived from `machine.gpu` and **refuses** a value it has no wrapper
for. nixgl's mesa wrapper is *named* `nixGLIntel` but covers AMD too (radeonsi
ships in mesa's `lib/dri`), so utsuho is correctly served by it and has a case
pinning that literally. A proprietary driver is not, and quietly handing it mesa
reproduces the EGL abort above — invisible until someone tries to render. So a
**desktop** host declaring `gpu = "nvidia"` fails to evaluate, naming the gpu and
the host, until both consumers are wired up; a headless one is unaffected, since
nothing forces `glWrapper` there (fujiwara already carries `gpu = "amd"` with
`gui = false`).

Testing that refusal takes a host that does not exist, and the obvious shortcut is
vacuous: asserting `defaultWrapper = "mesa"` on a real host passes whether or not
`home.nix` sets it, because `"mesa"` *is* home-manager's default — so the case
cannot tell the wired-up state from the drift it was written to catch. Instead the
cases inject a synthetic `machine` through `extendModules`, overriding the
`extraSpecialArgs` `flake.nix` passes in, and require `gpu = "nvidia"` to be refused
by **both** consumers, with a `gpu = "amd"` control so a refusal can't pass just
because evaluation is broken. Verified by deleting the `defaultWrapper` assignment:
the old pins stayed green, the linkage case goes red.

`insecure-packages` is the odd one out: it models nothing, it holds a claim
`home.nix` makes by omission. There is no `permittedInsecurePackages` entry in
`nixpkgs.config` because nothing needs one -- and the entry that used to be there
is why this is a test rather than a comment. It was scoped to `electron-39.8.10`
so that a bitwarden-desktop bump onto a different Electron would re-raise the
flag for review; the bump happened, the comment kept describing the old version,
and the permit sat on as a dangling exception that read like a live dependency on
an EOL Electron. nixpkgs raises its insecure error while *evaluating* the flagged
derivation, so forcing each home configuration's `activationPackage.drvPath` --
and `mari`'s darwin system, which carries its own `nixpkgs.config` -- is the claim
rather than a proxy for it. Both host lists are read out of the flake, so a new
host is covered the day it lands.

Its teeth are a second case per host, because "every host evaluates" is also true
of a config that has switched the check off, and `allowInsecurePredicate` switches
it off wholesale -- `check-meta.nix` short-circuits on it before it ever consults
the permit list. So a package nixpkgs still flags is evaluated through each host's
*own* `pkgs`, and has to fail with nixpkgs' "marked as insecure" specifically. A
permit or a predicate reappearing in `home.nix` stops it throwing; a typo or a
renamed attr fails it for the wrong reason and says so. Not to be confused with
`.forgejo/workflows/vulnix-scan.yml`, which scans the realised closure for CVEs
weekly and never fails -- that one is a report, this is a gate.

The `nixGL` half of that `ExecStart` is why this shipped broken once. The agent
builds its Qt Quick dialog only when a challenge arrives, and nix-built Qt
resolves libEGL/GBM/DRI out of the store, which has no driver for this GPU. So the
unit started clean, stayed `active` for hours, and then SIGABRTed on the first
prompt with "EGL not available" — which polkitd records as the operator *failing
to authenticate*, so the caller sees the identical bare `PermissionDenied` it sees
with no agent at all, and `RestartSec` brings the unit back looking healthy. A
green eval says the unit is shaped right, not that a prompt can be drawn; the
check that answers that is `coredumpctl list hyprpolkitagent` after trying one.

The `steam` wrapper (in `home.nix`) and `home/steam-desktop-override` model the two
places nix's profile and a pacman-installed app collide. Steam shells out to
`xdg-user-dir`, `~/.nix-profile/bin` precedes `/usr/bin`, and nix's loader can't
satisfy what Arch's `libc.so.6` leaves undefined — `__pointer_chk_guard`; and
`GIO_EXTRA_MODULES` points Steam's steamrt3c runtime (`steamrt64/pv-runtime`, glib
2.66.8) at a gvfs module its older glib can't load. The wrapper prepends the host's
directories rather than sanitising nix away, because `xdg-open` exists *only* in the
profile here and is how Steam opens a link — though only CEF resolves it through
`PATH`; `steamclient.so` hardcodes an absolute `/usr/bin/xdg-open` that isn't
installed. So the cases assert both directions: a fix that satisfies one and breaks
the other looks correct from either side alone. Five of them are there because the
obvious assertion passes with the bug still in place — the `xdg-user-dir` case reads
through `readlink` (profile entries are symlinks, so the unresolved name is never a
store path); the `xdg-open` case *executes* it under a Steam-shaped
`LD_LIBRARY_PATH` rather than resolving it (a `command -v` check can only fail when
the case above it already has); a first case asserts the `/usr/bin`-vs-profile
collision still exists at all, without which the rest can pass having measured
nothing; the wiring case compares argument *order*, since checking that each path
merely appears somewhere passes a src/dst transposition; and the fixture's wrapper
path deliberately does not end in `-steam`, so the cleanup marker can't be satisfied
by luck. The `nix eval` cases cover the wiring, since
the wrapper and the script can both be correct while the activation entry passes the
wrong paths — including that the entry keeps its `|| warnEcho`, because activation
runs under `set -eu` and a bare failure here would abort the entries after it.

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
