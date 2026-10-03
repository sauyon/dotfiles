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
})
