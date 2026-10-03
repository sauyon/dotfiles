# forgejo-cli overlay: wrap pkgs.forgejo-cli so `fj`'s bare invocations and the
# `git-credential-fj` shim serialize their refresh-token spends through one
# flock.  Bare `fj` previously bypassed the bash helper's lock and could spend
# a refresh_token that the helper was about to use; Forgejo treats reuse as
# compromise evidence, which is the whole "credential suddenly correct →
# token-already-used → no recovery without an interactive re-login" failure
# mode recorded in the project memory.
#
# What we actually change
# -----------------------
# `patches/forgejo-cli-keys-flock.patch` wraps `KeyInfo::get_api` in an
# exclusive `fs2::FileExt::lock_exclusive` across `keys.json`'s read-refresh-
# write cycle, with the lock at the same path the bash helper takes
# (`$XDG_RUNTIME_DIR/git-credential-fj.lock`, falling back through `$TMPDIR`
# and `/tmp`). `fs2 0.4.3` is added to `Cargo.toml`; the matching `[fs2]`
# entry is added to `Cargo.lock` (../forgejo-cli-Cargo.lock alongside this
# file). The lock path honors `FJ_LOCK_FILE` if set, so tests can pin it.
#
# Why we have to ship our own Cargo.lock
# --------------------------------------
# nixpkgs builds forgejo-cli from nixpkgs's vendored cargoDeps, which were
# resolved against *upstream*'s Cargo.lock.  Once our patch touches
# `Cargo.toml` to add `fs2`, the upstream lockfile no longer matches the
# manifest and the build would refuse to start.  `rustPlatform.importCargoLock`
# rebuilds cargoDeps from the supplied lockfile, so the patch goes in via
# `patches` and the lockfile goes in via `override { cargoDeps }` — the two
# stay in lockstep without us having to precompute a `cargoHash` SRI for a
# tarball nixpkgs has never vendored.
#
# Single source of truth: any home that consumes `pkgs.forgejo-cli` through
# this overlay pulls the same binary, so `forge-api.sh`'s per-request
# `git-credential-fj` re-mint and any bare `fj whoami` see one another.
# `base` is the package this one is derived from, passed separately from `pkgs`
# on purpose.  When this file is instantiated from an overlay, `pkgs` is the
# overlay's `final`, and `final.forgejo-cli` is *this* expression -- so taking
# the base from `pkgs` would define the package in terms of itself and eval
# would hit "infinite recursion" at the first site that forces it (which is
# home.nix's `home.packages`, not here).  The overlay therefore hands us
# `prev.forgejo-cli`; a direct `import` with plain nixpkgs can keep the default.
{ pkgs, base ? pkgs.forgejo-cli }:

let
  cargoDeps = pkgs.rustPlatform.importCargoLock {
    lockFile = ../forgejo-cli-Cargo.lock;
  };
in
base.overrideAttrs (_: {
  # `cargoDeps` is auto-derived by buildRustPackage from `cargoHash`
  # against `src/Cargo.lock`.  Replacing it here short-circuits that
  # derivation: our lockfile already includes the `fs2` crate the patch
  # hits, so cargoDeps hashes match without us having to precompute the
  # `cargoHash` SRI for a tarball nixpkgs has never vendored.
  #
  # `cargoHash` is left untouched on purpose.  Passing an explicit
  # `cargoDeps` shadows the cargoHash path in buildRustPackage; clearing
  # it would do nothing useful and risks the next nixpkgs bump changing
  # the upstream hash and breaking the override through fallback logic.
  cargoDeps = cargoDeps;
  # The patch mutates `Cargo.toml` (to declare fs2) and `src/keys.rs`.
  # It does NOT touch `Cargo.lock` — that lives at the dotfiles root as
  # `forgejo-cli-Cargo.lock` and is overlaid below, after the patch
  # phase, by a copy step.  One canonical source of truth: the lockfile
  # in this directory.
  patches = [ ../patches/forgejo-cli-keys-flock.patch ];
  # Override the source's Cargo.lock with ours.  cargoSetupHook runs
  # `cargo --frozen`, which refuses to mutate Cargo.lock; if `src` and
  # `cargoDeps` disagree on the lockfile contents the build fails with
  # "the lock file ... would have been modified".  Copying after the
  # patch phase keeps the two in lockstep.
  postPatch = ''
    cp ${../forgejo-cli-Cargo.lock} Cargo.lock
  '';
})
