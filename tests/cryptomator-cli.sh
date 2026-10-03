#!/usr/bin/env bash
# Cases for the cryptomator-cli wrapper -- nix/cryptomator-cli.nix, exposed as
# `packages.x86_64-linux.cryptomator-cli-wrapped` in flake.nix and as
# `home.packages.cryptomator-cli` in home.nix. Read both this script and the
# top of nix/cryptomator-cli.nix together if either ends up stale: the wrapper
# is two lines, and its every test exists because each line corresponds to one
# way the wrap is easy to get wrong.
#
# Why this exists
# ---------------
# cryptomator-cli's jFuse Linux native bundle dlopens three libraries at runtime:
# libfuse.so.3 (bundled, the upstream wrapper pre-loads it), and liburing.so.2 +
# libnuma.so.1 (NOT bundled). Without the overwrite, dlopen of the latter two
# falls through to the loader's default search, the host's /usr/lib via
# /etc/ld.so.cache. /usr/lib on a Nix-built binary's loader path pins the host
# glibc into Nix binaries that have no business loading it, and they die with
# "version `GLIBC_X.Y' not found". Today the symptom is masked by a tmpfs shim
# at /run/user/1000/ldshim that symlinks the host liburing and libnuma into a
# directory dlopen walks first -- a workaround that dies on reboot and has no
# reason to be re-created by anything but the operator noticing an unmounted
# vault.
#
# Five things have to hold at once. Four are visible in the wrapper text; the
# fifth connects this script to the rest of the repo.
#
#   1. /nix/store references for liburing and libnuma appear in the wrapper.
#      Without these jFuse dlopen()s of those two fall through to /usr/lib.
#   2. /usr/lib does NOT appear anywhere in the wrapper. That's the trap
#      the comment in nix/cryptomator-cli.nix warns about twice.
#   3. Those /nix/store paths actually contain the SONAME files jFuse loads
#      (liburing.so.2 and libnuma.so.1). A path can satisfy (1) and be useless
#      if the package no longer ships the SONAME -- nixpkgs has done some
#      SONAME bumps over the years.
#   4. The inner upstream wrapper still points at fuse-3.18.2/lib after our
#      re-wrap. If postFixup clobbered it instead of replacing the binary,
#      libfuse would fail to load from anywhere. makeWrapper renames the
#      original to .cryptomator-cli-wrapped; that file is the one to inspect.
#   5. .#homeConfigurations.<host>.config.home.packages resolves
#      cryptomator-cli to the same outPath as .#cryptomator-cli-wrapped.
#      They come from the same helper file today but a future refactor could
#      route one through a different override and silence 1..4.
#
# `kyuusaku` not `shiori` for the live resolution: kyuusaku is not in
# secretsHosts, so this script does not need a working KMS chain to run, and
# we have no reason to bind a test about the wrapper to KMS availability.
#
#   ./tests/cryptomator-cli.sh                       # builds and runs all cases
#   ./tests/cryptomator-cli.sh /nix/store/...        # cases against that store path
set -u

repo="$(cd "$(dirname "$0")/.." && pwd)"
# lib-attribute on the wrapper matches the original cryptomator-cli derivation; do
# not let nixpkgs bump autoconfigure a different one and silently pass these tests.
want_pname=cryptomator-cli

# Build the wrapper, resolve to its /nix/store path.
if [ "${1:-}" ]; then
  pkg="$1"
else
  pkg=$(cd "$repo" && nix --extra-experimental-features 'nix-command flakes' \
        build .#cryptomator-cli-wrapped --no-link --print-out-paths 2>/dev/null) || {
    echo "$0: nix build .#cryptomator-cli-wrapped failed" >&2
    exit 1
  }
fi
bin="$pkg/bin/cryptomator-cli"

fails=0; n=0
ok()  { n=$((n+1)); printf 'ok %d - %s\n' "$n" "$1"; }
bad() { n=$((n+1)); fails=$((fails+1)); printf 'FAIL %d - %s\n' "$n" "$1"; }

# Sanity: if the wrapper is missing, every check below becomes a passing
# nothing-burger. Reject early and loudly. Misses (1)-(5) applied to a missing
# wrapper would all read as "matches an empty file".
if [ ! -x "$bin" ]; then
  echo "FAIL 0 - $bin does not exist or is not executable; the cases below would pass vacuously" >&2
  echo; echo "0 cases, 1 failed"; exit 1
fi

# Wrappers makeWrapper generates carry the script line 'exec -a "$0" "<out>/bin/.<name>-wrapped" "$@"'.
# Grepping the wrapper for that pattern is brittle; the cleaner check is to look
# for the inner script at the same path as the wrapper but prefixed with a dot.
inner="$pkg/bin/.cryptomator-cli-wrapped"

# (1) /nix/store references for liburing and libnuma. Used to be a single check;
# split into two because the failure messages tell the reader which library the
# override dropped, which is the most useful pointer when something breaks.
if grep -qE '/nix/store/[^[:space:]]+-liburing-[^/]+/lib' "$bin"; then
  ok "wrapper references a /nix/store liburing/ path"
else
  hit=$(grep -oE '/liburing[^[:space:]]*' "$bin" || true)
  bad "wrapper references a /nix/store liburing/ path"$'\n'"      got: ${hit:-<none>}"
fi
if grep -qE '/nix/store/[^[:space:]]+-numactl-[^/]+/lib' "$bin"; then
  ok "wrapper references a /nix/store numactl/ path"
else
  hit=$(grep -oE '/numactl[^[:space:]]*' "$bin" || true)
  bad "wrapper references a /nix/store numactl/ path"$'\n'"      got: ${hit:-<none>}"
fi

# (2) /usr/lib absent. This is the trap the handoff calls out by name. Printing
# the offending line makes "who added this" cheap to track down.
if grep -q '/usr/lib' "$bin"; then
  bad "wrapper does not mention /usr/lib (the host glibc trap)"$'\n'"      offender: $(grep -n '/usr/lib' "$bin" | head -1)"
else
  ok "wrapper does not mention /usr/lib"
fi

# (3) SONAME files exist at the paths the wrapper names. Uses the first match in
# each wrapper for stability across makeWrapper versions.
uring_path=$(grep -oE '/nix/store/[^[:space:]]+-liburing-[^/]+/lib' "$bin" | head -1)
numa_path=$(grep -oE '/nix/store/[^[:space:]]+-numactl-[^/]+/lib' "$bin" | head -1)
# strings(1) on stores is cross-checking the path doesn't have a trailing byte
# we'd be tempted to compare; not load-bearing for these names.
if [ -z "$uring_path" ] || [ ! -f "$uring_path/liburing.so.2" ]; then
  bad "liburing.so.2 exists at the wrapper's liburing path" \
      $'\n      '"path: ${uring_path:-<not found>}"
else
  ok "liburing.so.2 exists at the wrapper's liburing path ($uring_path/liburing.so.2)"
fi
if [ -z "$numa_path" ] || [ ! -f "$numa_path/libnuma.so.1" ]; then
  bad "libnuma.so.1 exists at the wrapper's numactl path" \
      $'\n      '"path: ${numa_path:-<not found>}"
else
  ok "libnuma.so.1 exists at the wrapper's numactl path ($numa_path/libnuma.so.1)"
fi

# (4) The inner wrapper the outer execs (.cryptomator-cli-wrapped) still
# points at a fuse lib. If postFixup accidentally clobbered instead of
# wrapping, makeWrapper would never have produced the hidden file and this
# check would find neither.
if [ -f "$inner" ] && grep -qE '/nix/store/[^[:space:]]+-fuse-[^/]+/lib' "$inner"; then
  ok "inner wrapper still points at a /nix/store fuse/ path (wrap not clobber)"
else
  bad "inner wrapper still points at a /nix/store fuse/ path" \
      $'\n      '"inner: ${inner:-<missing>}"
fi

# (5) The same wrapper is what home.nix installs on a non-sops host. kyuusaku
# is the smallest such host (`!isSecretsHost`, `!isDarwin`, hence includes the
# `lib.optionals (!isDarwin)` cryptomator-cli). The whole test suite is
# pointless if the live profile ships a different binary.
home_drv=$(cd "$repo" && nix --extra-experimental-features 'nix-command flakes' \
          eval --raw ".#homeConfigurations.kyuusaku.config.home.packages" \
          --apply "ps: (builtins.head (builtins.filter (p: (p.pname or \"\") == \"$want_pname\") ps)).drvPath" \
          2>/dev/null) || home_drv=""
home_pkg=$(cd "$repo" && nix-store --realise "$home_drv" 2>/dev/null | tail -1) || home_pkg=""
if [ -n "$home_pkg" ] && [ "$home_pkg" = "$pkg" ]; then
  ok "home.packages' cryptomator-cli is the wrapped package ($pkg)"
elif [ -z "$home_pkg" ]; then
  bad "home.packages' cryptomator-cli is the wrapped package" \
      $'\n      '"could not resolve .#homeConfigurations.kyuusaku.config.home.packages"
else
  bad "home.packages' cryptomator-cli is the wrapped package" \
      $'\n      '"wanted: $pkg"$'\n'"      got:    $home_pkg"
fi

echo
if [ "$fails" = 0 ]; then echo "$n cases, all good"; else echo "$n cases, $fails failed"; fi
[ "$fails" = 0 ]
