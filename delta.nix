{ lib, pkgs, ... }:

# Delta — Zed's agent-workspace app (delta.dev), CLI `delta` plus the GPUI
# desktop app `delta-app`. Not in nixpkgs, so we package the upstream release
# tarball the same way `cursor-agent.nix` does.
#
# Named `zed-delta`, not `delta`: nixpkgs already has `delta` (the git pager),
# and shadowing a top-level attr in an overlay is the kind of thing that
# surfaces three hosts away as the wrong binary on PATH.
#
# To bump: the release API hands out the version and URL per platform —
#
#   curl -fsSL "https://delta.dev/api/releases/stable/latest/asset?asset=delta&os=linux&arch=x86_64"
#   nix store prefetch-file --json <url>   # .hash is already SRI
#
# Linux x86_64/aarch64 only. Upstream ships macOS as a signed Delta.app.zip,
# which is a different packaging shape (bundle, not tarball + patchelf); add it
# the day a darwin host actually wants Delta rather than blind.
let
  version = "0.20.1";

  platformSpec = system:
    {
      x86_64-linux = {
        arch = "x86_64";
        hash = "sha256-97/XaIOQo5ni077fRErzfN2IK5jh5PjxFE1vK5Z7Oco=";
      };
      aarch64-linux = {
        arch = "aarch64";
        hash = "sha256-zhA5/m2bYOM8tpOpctPvH2JdojqDjWR8KrCNkKCPQ9w=";
      };
    }.${system} or null;
in
{
  nixpkgs.overlays = [
    (final: prev:
      let
        inherit (prev.stdenvNoCC.hostPlatform) system;
        spec = platformSpec system;
      in
      {
        zed-delta = prev.stdenv.mkDerivation {
          pname = "zed-delta";
          inherit version;

          src =
            if spec == null then
              throw "zed-delta: unsupported system ${system}"
            else
              prev.fetchurl {
                url = "https://releases.delta.dev/releases/stable/${version}/delta-linux-${spec.arch}.tar.gz";
                inherit (spec) hash;
              };

          nativeBuildInputs = [
            prev.autoPatchelfHook
            prev.makeWrapper
          ];

          # Everything `ldd bin/delta-app` lists beyond libc/libm. The tarball
          # also bundles these under Delta/lib/ for bare distros; we leave
          # those behind and let autoPatchelf point at nixpkgs' instead.
          # libunwind is LLVM's, not the savannah one: the binary wants
          # libunwind.so.1, which only llvmPackages.libunwind ships.
          buildInputs = [
            prev.llvmPackages.libunwind
            prev.libbsd
            prev.libmd
            prev.libxau
            prev.libxdmcp
            prev.libxcb
            prev.libxkbcommon
          ];

          installPhase = ''
            runHook preInstall

            # bin/delta (CLI/daemon entry point) and bin/delta-app (the GUI)
            # stay side by side: the CLI locates the app relative to its own
            # path, and the desktop file Execs the CLI.
            install -Dm755 bin/delta $out/libexec/delta/bin/delta
            install -Dm755 bin/delta-app $out/libexec/delta/bin/delta-app

            # ldd is not the whole story: GPUI dlopens its graphics and
            # wayland stack (libvulkan.so.1, libEGL.so.1, libwayland-*.so)
            # rather than linking it, so autoPatchelf never sees those. The
            # wrapper puts the loaders on LD_LIBRARY_PATH; the mesa drivers
            # behind them come from the nixGL wrap in home.nix, the same as
            # ghostty/parsec.
            makeWrapper $out/libexec/delta/bin/delta $out/bin/delta \
              --prefix LD_LIBRARY_PATH : ${
                lib.makeLibraryPath [
                  prev.vulkan-loader
                  prev.libglvnd
                  prev.wayland
                ]
              }

            # Desktop entry + icons, mirroring upstream's install.sh: it
            # rewrites Exec=delta to the absolute CLI path, which here is the
            # wrapper (and stays correct through the nixGL wrap, which
            # rewrites bin/ in place).
            install -Dm644 share/applications/dev.zed.Delta.desktop \
              $out/share/applications/dev.zed.Delta.desktop
            substituteInPlace $out/share/applications/dev.zed.Delta.desktop \
              --replace-fail 'Exec=delta open %U' "Exec=$out/bin/delta open %U"
            cp -r share/icons $out/share/

            runHook postInstall
          '';

          # installCheck below can only drive the CLI -- launching delta-app
          # headless proves nothing -- so don't let strip rewrite the one
          # binary nobody smoke-tests. The store cost is real but the failure
          # mode it removes is a GUI that dies on first launch.
          dontStrip = true;

          doInstallCheck = true;
          installCheckPhase = ''
            test "$($out/bin/delta --version)" = "delta ${version}"
            # The dlopened Vulkan loader is the runtime dep most likely to go
            # missing silently (see installPhase); assert the wrapper carries it.
            grep -q '${prev.vulkan-loader}/lib' $out/bin/delta
          '';

          meta = {
            description = "Delta, Zed's collaborative agent workspace";
            homepage = "https://delta.dev";
            license = lib.licenses.unfree;
            mainProgram = "delta";
            platforms = lib.platforms.linux;
            sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
          };
        };
      })
  ];
}
