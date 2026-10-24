{ config, lib, pkgs, machine, ... }:

let
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  isDesktop = machine.gui or true;

  # Orca (onorca.dev) — the agent IDE: parallel CLI agents, each in its own
  # worktree. Closed-source Electron app, not in nixpkgs (nixpkgs' `orca` is
  # the GNOME screen reader), so this packages the official Linux AppImage.
  # The electron-updater feed in each GitHub release, latest-linux.yml, is
  # the source of truth for version + sha512 when bumping: the yml's sha512
  # is the AppImage's, checked against
  # the download with `nix hash to-sri --type sha512 <sha512sum>`.
  # https://github.com/stablyai/orca/releases/latest/download/latest-linux.yml
  pname = "orca";
  version = "1.4.238";
  src = pkgs.fetchurl {
    url = "https://github.com/stablyai/orca/releases/download/v${version}/orca-linux.AppImage";
    hash = "sha512-J1hApmTOvc91BmhiplyCNjI3UMJ99awjQgVlgpj4azkH/sHSKlfJa4c1O2MCcdJhLrxCVm/NVKT+JtghzxoEsQ==";
  };

  # Wrap the AppImage (not autoPatchelf of the .deb) so the Electron stack
  # runs in appimageTools' FHS env, which works on non-NixOS hosts. Electron
  # safeStorage wants libsecret at runtime.
  orca = pkgs.appimageTools.wrapType2 {
    inherit pname version src;
    extraPkgs = pkgs: [ pkgs.libsecret ];
    # buildFHSEnv binds the host's /etc/fonts in, assuming NixOS, where its
    # conf.d links point into /nix/store. Arch's point into
    # /usr/share/fontconfig, which the sandbox's /usr replaces, so every one
    # dangled and the UI fell through to Montserrat Thin. Bind the host's copy
    # over the rootfs's (which exists, so bwrap has a mount point). -try: a
    # NixOS host has no such dir and needs none. tests/sandbox-fonts.sh.
    extraBwrapArgs = [ "--ro-bind-try /usr/share/fontconfig /usr/share/fontconfig" ];
    # The AppImage ships bin/orca and an orca-ide.desktop; upstream's .deb
    # installs the CLI as `orca-ide`, and that is what the desktop Exec and
    # meta.mainProgram use below. appimageTools keeps the AppImage's own bin
    # name, so the wrapper is renamed by hand.
    #
    # --ozone-platform=wayland: Orca's startup gate picks X11 when neither
    # --ozone-platform nor ELECTRON_OZONE_PLATFORM_HINT says otherwise, and on
    # a Hyprland-only session there is no X server to stat — measured: the
    # bare launch exits 1 with "needs a usable display server", and the
    # explicit wayland switch keeps it running (timeout kill at 20s, exit 124).
    # An env var would work from a shell but not a desktop-file launch, so it
    # is a flag on the command line.
    #
    # The flag lands in extraInstallCommands, not postFixup: wrapType2 is a
    # buildFHSEnv, whose buildCommand short-circuits genericBuild's phase
    # list, so postFixup never runs. The bin/orca symlink from buildCommand
    # is replaced with a wrapper that prepends the flag and passes "$@"
    # through. buildFHSEnv's builder has no makeWrapper on PATH, so its setup
    # hook is sourced by hand; and the wrapper must not target its own path
    # (renaming to orca-ide and wrapping orca-ide in place makes it exec
    # itself), so the bwrap launcher moves aside to .orca-ide-unwrapped.
    extraInstallCommands = ''
      source ${pkgs.makeWrapper}/nix-support/setup-hook
      mv $out/bin/orca $out/bin/.orca-ide-unwrapped
      makeShellWrapper $out/bin/.orca-ide-unwrapped $out/bin/orca-ide \
        --add-flags '--ozone-platform=wayland'
      install -Dm444 ${appimageContents}/orca-ide.desktop \
        $out/share/applications/orca-ide.desktop
      for size in 16 24 32 48 64 128 256 512; do
        install -Dm444 \
          ${appimageContents}/usr/share/icons/hicolor/''${size}x''${size}/apps/orca-ide.png \
          $out/share/icons/hicolor/''${size}x''${size}/apps/orca-ide.png
      done
      substituteInPlace $out/share/applications/orca-ide.desktop \
        --replace-fail 'Exec=AppRun' 'Exec=orca-ide'
    '';
    meta.mainProgram = "orca-ide";
  };
  # Extracted once, shared by the wrapper's extraInstallCommands below;
  # let bindings are recursive, so position in the block does not matter.
  appimageContents = pkgs.appimageTools.extract { inherit pname version src; };
in
{
  home.packages =
    lib.optionals (!isDarwin && isDesktop) [
    # nixGL wrap: without it the FHS env resolves GBM/DRI via the NixOS-only
    # /run/opengl-driver path and falls back to software rendering — the
    # same reason parsec gets it.
      (config.lib.nixGL.wrap orca)
    ];
}
