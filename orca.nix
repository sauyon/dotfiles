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
    # The sandbox gets its own fontconfig, the way every other nix GUI app here
    # gets a nix-generated FONTCONFIG_FILE, instead of the distro's.
    # buildFHSEnv binds the host's /etc/fonts in, assuming NixOS; Arch's conf.d
    # links point into /usr/share/fontconfig, which the sandbox's /usr replaces,
    # so all of them dangled and every family fell to Montserrat Thin. See
    # fontsConf below. tests/sandbox-fonts.sh.
    extraBwrapArgs = [ "--setenv FONTCONFIG_FILE ${fontsConf}" ];
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
  # Hermetic: nixpkgs' own conf.d (store paths, the full standard alias set),
  # whose 50-user.conf pulls in ~/.config/fontconfig -- home-manager's font
  # dirs, i.e. the nix-profile fonts every other app here uses. Plus DejaVu:
  # Orca's Linux terminalFontFamily default is exactly "DejaVu Sans Mono" with
  # no fallback, and Chromium refuses fontconfig's alias to another mono font,
  # so without it the terminal lays a proportional fallback out on a mono grid.
  # Scoped to orca rather than the profile because only orca hardcodes it.
  fontsConf = pkgs.writeText "orca-fonts.conf" ''
    <?xml version="1.0"?>
    <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
    <fontconfig>
      <include ignore_missing="no">${pkgs.fontconfig.out}/etc/fonts/conf.d</include>
      <dir>${pkgs.dejavu_fonts}/share/fonts</dir>
      <cachedir prefix="xdg">fontconfig</cachedir>
    </fontconfig>
  '';
  # Extracted once, shared by the wrapper's extraInstallCommands below;
  # let bindings are recursive, so position in the block does not matter.
  appimageContents = pkgs.appimageTools.extract { inherit pname version src; };

  # Mac-style shortcuts, the same Super-for-Cmd scheme as gtk's mac-bindings
  # and ghostty's super+c/v in home.nix: Orca's own macOS defaults, with Cmd
  # spelled out, which Orca binds to Super (event.metaKey) on Linux. Ctrl then
  # goes back to the terminal: Ctrl+C is SIGINT again, not copy.
  #
  # Generated from the app's own keybinding definitions at build time
  # (nix/orca-mac-keybindings.js), so a version bump picks up new actions.
  # Chords Hyprland binds are skipped and those actions keep their Linux Ctrl
  # default -- the compositor would eat them, leaving a dead key. That costs
  # Super+F (find), Super+P (go to file), Super+N and Super+B, which are
  # Hyprland's emacs-style focus moves; the build log lists every one.
  hyprlandChords = let s = config.wayland.windowManager.hyprland.settings; in
    lib.concatMap
      (k: map (b: if builtins.isAttrs b && b ? _args then builtins.head b._args else b) s.${k})
      (builtins.filter (lib.hasPrefix "bind") (builtins.attrNames s));
  macKeybindings = pkgs.runCommand "orca-mac-keybindings" {
    nativeBuildInputs = [ pkgs.nodejs pkgs.asar pkgs.makeWrapper ];
  } ''
    asar extract ${appimageContents}/resources/app.asar app
    mkdir -p $out/share $out/bin
    node ${./nix/orca-mac-keybindings.js} app/out/shared \
      ${pkgs.writeText "hyprland-chords.json" (builtins.toJSON hyprlandChords)} \
      > $out/share/orca-mac-keybindings.json
    makeWrapper ${mergeKeybindings} $out/bin/orca-mac-keybindings \
      --set OWNED $out/share/orca-mac-keybindings.json
  '';
  # ~/.orca/keybindings.json is Orca's read-write state: its settings UI saves
  # into the same platforms.linux section, by atomic rename, so a home.file
  # symlink would be replaced on the first edit. So the generated keys are
  # merged over whatever Orca last wrote -- they win, every other action's UI
  # edit survives -- and the file is replaced via .new + mv. A file jq cannot
  # parse (Orca's loader would reject it too) is left for Orca to report.
  # tests/orca-mac-keybindings.sh.
  mergeKeybindings = pkgs.writeShellScript "orca-mac-keybindings-merge" ''
    DEST="$HOME/.orca/keybindings.json"
    mkdir -p "$HOME/.orca"
    [ -f "$DEST" ] || echo '{"version":1,"keybindings":{},"platforms":{"darwin":{},"linux":{},"win32":{}}}' > "$DEST"
    if ${pkgs.jq}/bin/jq --slurpfile owned "$OWNED" \
        '.version = 1 | .keybindings //= {} | .platforms //= {} | .platforms.linux = ((.platforms.linux // {}) + $owned[0])' \
        "$DEST" > "$DEST.new"; then
      mv "$DEST.new" "$DEST"
    else
      echo "orca: $DEST is not valid JSON; leaving it for Orca to report" >&2
      rm -f "$DEST.new"
    fi
  '';
in
{
  home.packages =
    lib.optionals (!isDarwin && isDesktop) [
    # nixGL wrap: without it the FHS env resolves GBM/DRI via the NixOS-only
    # /run/opengl-driver path and falls back to software rendering — the
    # same reason parsec gets it.
      (config.lib.nixGL.wrap orca)
    ];

  home.activation.orcaMacKeybindings = lib.mkIf (!isDarwin && isDesktop)
    (lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ${macKeybindings}/bin/orca-mac-keybindings
    '');
}
