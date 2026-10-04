# forgejo-cli (binary: `fj`), built from our fork instead of nixpkgs' v0.6.0.
#
# The fork (forge.ko.ag/sauyon/forgejo-cli, merged into its `main`) carries two
# changes upstream does not have yet:
#
#   * `KeyInfo::get_api` takes an advisory lock around the whole
#     load-refresh-save cycle and re-reads keys.json under it.  Forgejo issues
#     one-shot refresh tokens and treats a reused one as compromise evidence,
#     so two fj processes refreshing at once leave one login dead with no
#     recovery but an interactive `fj auth login`.  The wait is bounded (60s,
#     FJ_LOCK_WAIT_SECS) because a holder that stalls -- a suspended laptop
#     mid-refresh -- must not wedge every other invocation.
#   * `fj git-credential <get|store|erase>`, a real git credential helper, which
#     is what lets home.nix's shim be a one-line `exec` instead of 55 lines of
#     jq-and-flock.  See the comment there for why that mattered.
#
# Why a fork rather than a patch: the first attempt patched Cargo.toml to add
# `fs2` for flock(2), which forced a hand-maintained Cargo.lock and a
# patch file that silently rots against every nixpkgs bump.  std's
# `File::try_lock` (stable since 1.89, and nixpkgs' rustc is 1.98) removes the
# dependency entirely, and the fork keeps the change as rebaseable history.
#
# Bumping `rev`, all three in one commit or the build fails:
#   1. rev below
#   2. hash:  nix-prefetch-url --unpack \
#        https://forge.ko.ag/sauyon/forgejo-cli/archive/<rev>.tar.gz
#      then: nix hash to-sri --type sha256 <base32-output>
#   3. cp the fork's Cargo.lock to ../forgejo-cli-Cargo.lock -- cargoSetupHook
#      runs `cargo --frozen`, which refuses to reconcile a lockfile that
#      disagrees with the vendored tree.
#
# `base` is the package we derive from, passed separately from `pkgs` on
# purpose: instantiated from an overlay, `pkgs` is that overlay's `final`, and
# `final.forgejo-cli` is *this* expression, so taking the base from `pkgs` would
# define the package in terms of itself.  Eval then dies with "infinite
# recursion" at the first site that forces it, which is home.nix's
# `home.packages` rather than anywhere near here.  The overlay hands us
# `prev.forgejo-cli`; a direct import with plain nixpkgs keeps the default.
{ pkgs, base ? pkgs.forgejo-cli }:

let
  rev = "b19e20882cb62af896be73f01e8267e29190705f";
in
base.overrideAttrs (old: {
  src = pkgs.fetchFromGitea {
    domain = "forge.ko.ag";
    owner = "sauyon";
    repo = "forgejo-cli";
    inherit rev;
    hash = "sha256-7yxwxSFKG7jU4HXMDnHa7FvITFTZFxl4SmwPnn1rLh8=";
  };

  # nixpkgs derives cargoDeps from its own cargoHash against v0.6.0's lockfile.
  # The fork is 72 commits past that tag and its lock differs by ~400 lines, so
  # the vendored tree has to be rebuilt from the fork's own lock, copied in
  # beside this file.  Passing cargoDeps explicitly also shadows the cargoHash
  # path in buildRustPackage, so the stale hash is harmless.
  cargoDeps = pkgs.rustPlatform.importCargoLock {
    lockFile = ../forgejo-cli-Cargo.lock;
  };

  # tests/git_credential.rs redirects the key store with XDG_DATA_HOME, which is
  # how `directories::ProjectDirs` finds keys.json on Linux. macOS resolves data
  # dirs through Apple's standard paths (~/Library/Application Support) and
  # ignores XDG_DATA_HOME, so on darwin every case reads an empty store: the
  # known-host case prints no token, and the two lock cases "give up and serve
  # the stored token" with nothing to serve. The subcommand itself is fine on
  # darwin -- a real `fj git-credential get` against the live keys.json serves
  # the token -- so skip these three here (CI builds Linux and runs the full
  # suite) and lean on installCheckPhase below, whose checks are path-agnostic.
  checkFlags = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
    "--skip=get_emits_username_and_token_for_a_known_host"
    "--skip=get_gives_up_on_a_wedged_lock_and_serves_the_stored_token"
    "--skip=get_waits_for_another_process_to_leave_the_refresh_lock"
  ];

  # Build-time facts about $out belong here rather than in tests/ (see README's
  # "## Tests").  Both assertions below are about the contract home.nix depends
  # on, and both would have caught shipping nixpkgs' fj by mistake.
  doInstallCheck = true;
  installCheckPhase = (old.installCheckPhase or "") + ''
    runHook preInstallCheck

    # 1. The subcommand exists at all. Without it the credential wrapper in
    #    home.nix execs into a clap error and every git push to forge.ko.ag
    #    fails authentication.
    $out/bin/fj git-credential --help > /dev/null \
      || { echo "fj has no git-credential subcommand -- wrong source?" >&2; exit 1; }

    # 2. With no key store, `get` exits 0 and says nothing. This is git's rule,
    #    not ours: a helper that exits nonzero aborts the whole operation
    #    instead of falling through, and anything on stdout that is not
    #    key=value corrupts the protocol.
    export HOME="$TMPDIR/fj-check"
    export XDG_DATA_HOME="$HOME/share"
    mkdir -p "$XDG_DATA_HOME"
    # Not named `out`: that is stdenv's own variable for the output path, and
    # clobbering it here would break every later phase.
    answer=$(printf 'protocol=https\nhost=forge.invalid\n\n' \
      | $out/bin/fj git-credential get) \
      || { echo "git-credential get exited nonzero with no key store" >&2; exit 1; }
    [ -z "$answer" ] \
      || { echo "git-credential get printed with no key store: $answer" >&2; exit 1; }

    runHook postInstallCheck
  '';
})
