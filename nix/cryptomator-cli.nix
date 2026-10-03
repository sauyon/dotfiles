# Self-contained wrapper for cryptomator-cli: same upstream derivation, with the
# two libraries jFuse dlopens at runtime promoted from /usr/lib to Nix-built
# paths so the wrapper survives a reboot (today /run/user/1000/ldshim, a tmpfs
# shim symlinking the host liburing + libnuma, is what keeps it alive).
#
# Why the wrapper needs more than just fuse
# -----------------------------------------
# `pkgs.cryptomator-cli`'s $out/bin/cryptomator-cli already sets
# LD_LIBRARY_PATH to its bundled fuse-3.18.2/lib so that jFuse's libfuse.so.3
# resolves. jFuse then dlopens liburing.so.2 (libfuse uses io_uring for the
# splice/notify paths) and libnuma.so.1 (NUMA-aware thread placement). Neither
# is in the upstream bundle, so dlopen falls through to the loader's default
# search: the host's /usr/lib via /etc/ld.so.cache. That works, but it relies
# on the host happening to ship ABI-compatible versions AND on something
# keeping the loader's view stable across reboots.
#
# The shim (commit c8349a in this repo's history) was the "something": a tmpfs
# directory of symlinks at /run/user/1000/ldshim, re-created after every
# reboot. tmpfs is fine until the next reboot where the symlinks went away and
# nobody noticed until a vault operation failed.
#
# Why not just LD_LIBRARY_PATH=/usr/lib
# -------------------------------------
# A Nix-built binary pins a glibc version at build time. Anything that
# transitively dlopens from /usr/lib gets the host glibc and its symbols,
# which do not match, and the binary dies with a confusing error about
# glibc/libc.so.6 not found. Never widen the loader path to /usr/lib;
# pin only the libraries cryptomator actually needs.
#
# Why this helper exists in two consumers
# ---------------------------------------
# flake.nix exposes it as `packages.x86_64-linux.cryptomator-cli-wrapped` so
# tests can build it directly without evaluating a full home-manager
# configuration. home.nix uses the same derivation through its home.packages
# list, so the wrapper that ships to /home/sauyon/.nix-profile/bin is the
# exact same one the test asserts against. Single source of truth.
{ pkgs }:

pkgs.cryptomator-cli.overrideAttrs (old: {
  # pkgs.makeWrapper is a setup-hook whose `wrapProgram` shell helper wraps an
  # existing $out/bin/<name> in place. Native build input, not a build-time
  # binary call -- a `lib.getExe` invocation against it returns a non-existent
  # path. Matching the waypipe override (home.nix:1116) which uses the same
  # hook.
  nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
  # Upstream's wrapper sets LD_LIBRARY_PATH to $fuseLib and prepends ${fuseLib}
  # in front of whatever the caller passed. With our --prefix additions, the
  # inner wrapper sees <something-we-prepended>:liburing:numactl, does its
  # fuse substitution, and ends up with <fuseLib>:liburing/lib:numactl/lib in
  # that order. ld.so's dlopen walks left-to-right, so liburing and libnuma
  # still resolve from Nix paths even though fuse is listed first.
  postFixup = (old.postFixup or "") + ''
    wrapProgram $out/bin/cryptomator-cli \
      --prefix LD_LIBRARY_PATH : ${pkgs.liburing}/lib:${pkgs.numactl}/lib
  '';

  # Every claim above is a fact about $out, so it is checked here rather than
  # from outside. tests/cryptomator-cli.sh used to do this; it could only ever
  # run against a store path somebody had already built and installed, which is
  # the one situation where a broken wrap has already shipped.
  #
  # Nothing here runs the binary: cryptomator-cli wants a vault and a
  # passphrase, and the failure this guards against is a dlopen at the moment a
  # vault operation happens, not at startup. So the assertions are about what
  # the loader will be able to find when that dlopen comes.
  doInstallCheck = true;
  installCheckPhase = (old.installCheckPhase or "") + ''
    runHook preInstallCheck

    bin=$out/bin/cryptomator-cli
    inner=$out/bin/.cryptomator-cli-wrapped

    # The two libraries jFuse dlopens and the upstream bundle omits. Read out of
    # the wrapper rather than interpolated again, so this checks what shipped.
    # `|| true` because a no-match grep is exactly the failure being checked
    # for, and stdenv runs with `set -e`: without it the phase dies on the
    # assignment and prints none of the messages below.
    uring=$(grep -o '/nix/store/[^:"]*liburing[^:"]*/lib' "$bin" | head -1 || true)
    numa=$(grep -o '/nix/store/[^:"]*numactl[^:"]*/lib' "$bin" | head -1 || true)
    [ -n "$uring" ] || { echo "wrapper names no store liburing path" >&2; exit 1; }
    [ -n "$numa" ]  || { echo "wrapper names no store numactl path" >&2; exit 1; }
    [ -e "$uring/liburing.so.2" ] \
      || { echo "no liburing.so.2 under $uring" >&2; exit 1; }
    [ -e "$numa/libnuma.so.1" ] \
      || { echo "no libnuma.so.1 under $numa" >&2; exit 1; }

    # The "Why not just LD_LIBRARY_PATH=/usr/lib" trap, asserted. A Nix binary
    # that reaches /usr/lib picks up the host glibc and dies on a missing
    # symbol, so the loader path must never widen to it.
    if grep -q '/usr/lib' "$bin"; then
      echo "wrapper mentions /usr/lib:" >&2
      grep -n '/usr/lib' "$bin" >&2
      exit 1
    fi

    # wrapProgram must have WRAPPED upstream's wrapper, not replaced it:
    # upstream substitutes its bundled fuse into LD_LIBRARY_PATH, and a clobber
    # leaves libfuse.so.3 unresolvable while both checks above still pass.
    grep -q '/nix/store/[^:"]*fuse[^:"]*' "$inner" \
      || { echo "inner wrapper $inner names no store fuse path" >&2; exit 1; }

    runHook postInstallCheck
  '';
})
