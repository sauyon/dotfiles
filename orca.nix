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

  # The AppImage's own tree, unsandboxed: its ELFs patched to store paths with
  # autoPatchelf and the binary wrapped with makeWrapper, the way nixpkgs
  # packages slack or vscode. A patched binary runs on Arch as well as NixOS.
  # This replaced appimageTools.wrapType2, whose bwrap FHS env everything Orca
  # spawns (its terminals, the agents in them) inherited: no /etc/hostname,
  # `date` in UTC (/etc/localtime points into a /usr the env replaced), fonts
  # dangling, and bwrap between every caller and Electron, forwarding no
  # signals. tests/orca-gui-unsandboxed.sh.
  #
  # Three trees stay byte-identical: what orcad slots are built from, each
  # file sha256-pinned in resources/orcad-template/orcad-template.json and
  # checked when a slot is installed -- patched or stripped, the check fails
  # and Orca downloads stock copies instead. resources/node-runtime's node is
  # the slot's runtime (pinned by the template's .runtime-node);
  # resources/orcad-template holds per-target builds (arm64, musl) that Orca
  # ships to remote hosts; resources/ripgrep is the slots' rg
  # (packagedRipgrep). All run unpatched on a glibc host with an FHS loader,
  # which Arch is. The AppImage's own usr/lib (libnotify, libappindicator, ...) is
  # left out for nixpkgs' copies, below.
  #
  # chrome-sandbox ships without the setuid bit (the store cannot hold one);
  # with unprivileged user namespaces, which Arch has, Chromium uses its
  # namespace sandbox and never needs it.
  orcaRuntimeDeps = with pkgs; [
    libGL systemd libpulseaudio pipewire libva libsecret libnotify
    libappindicator-gtk3 wayland gtk3
  ];
  orca = pkgs.stdenv.mkDerivation {
    inherit pname version;
    src = appimageContents;
    nativeBuildInputs = [ pkgs.autoPatchelfHook pkgs.makeWrapper ];
    buildInputs = with pkgs; [
      alsa-lib at-spi2-atk cairo cups dbus expat glib gtk3 libdrm libgbm
      libxkbcommon nspr nss pango stdenv.cc.cc.lib systemd
      libx11 libxcomposite libxdamage libxext libxfixes libxrandr libxcb
    ];
    # dlopened, so autoPatchelf cannot see them, and nix's loader has no
    # /usr/lib to fall back on: GL and udev for the GPU process, pulse and
    # pipewire for audio and screen capture, libva for video decode,
    # libsecret for Electron's safeStorage, notify and appindicator for
    # notifications and the tray; wayland-client and gtk3 for
    # resources/bin/orca-wayland-clipboard, which links only libc.
    runtimeDependencies = map lib.getLib orcaRuntimeDeps;
    dontConfigure = true;
    dontBuild = true;
    # Kept off the pinned trees: autoPatchelf by running it by hand below,
    # strip by not running it at all -- it rewrites the pinned node as surely
    # as patchelf. installCheckPhase holds the trees to that.
    dontAutoPatchelf = true;
    dontStrip = true;
    installPhase = ''
      runHook preInstall
      mkdir -p $out/lib/orca $out/share/applications
      cp -r --no-preserve=ownership . $out/lib/orca
      chmod -R u+w $out/lib/orca
      # AppRun must stay out: Orca's CLI adds --no-sandbox whenever it finds
      # AppRun beside the executable and `unshare -Ur true` fails (or it runs
      # as root) -- out/cli/runtime/electron-sandbox-switch.js.
      rm -r $out/lib/orca/{AppRun,.DirIcon,usr,orca-ide.desktop,orca-ide.png}
      install -Dm444 orca-ide.desktop $out/share/applications/orca-ide.desktop
      for size in 16 24 32 48 64 128 256 512; do
        install -Dm444 usr/share/icons/hicolor/''${size}x''${size}/apps/orca-ide.png \
          $out/share/icons/hicolor/''${size}x''${size}/apps/orca-ide.png
      done
      # The AppImage ships bin/orca and an orca-ide.desktop; upstream's .deb
      # names the app orca-ide, and that is what the desktop Exec and
      # meta.mainProgram use.
      substituteInPlace $out/share/applications/orca-ide.desktop \
        --replace-fail 'Exec=AppRun' 'Exec=orca-ide'
      runHook postInstall
    '';
    postFixup = ''
      targets=()
      for p in $out/lib/orca/* $out/lib/orca/resources/*; do
        case $p in
          */resources|*/resources/node-runtime|*/resources/orcad-template|*/resources/ripgrep) ;;
          *) targets+=("$p") ;;
        esac
      done
      autoPatchelf -- "''${targets[@]}"
      # runtimeDependencies only reach executables, but Chromium's bundled GL
      # (ANGLE, SwiftShader) dlopens libEGL/libGL itself, and a dlopen
      # searches the calling library's RUNPATH. Without this they find GL
      # only through nixGL's LD_LIBRARY_PATH -- as nixpkgs' electron-bin does.
      patchelf --add-rpath ${lib.makeLibraryPath orcaRuntimeDeps} \
        $out/lib/orca/{libEGL.so,libGLESv2.so,libvk_swiftshader.so}

      # --ozone-platform=wayland: Orca's startup gate picks X11 when neither
      # --ozone-platform nor ELECTRON_OZONE_PLATFORM_HINT says otherwise, and
      # on a Hyprland-only session there is no X server -- a bare launch exits
      # 1 with "needs a usable display server". A flag, not an env var, so a
      # desktop-file launch gets it too. Not in node mode: node rejects it
      # ("bad option", exit 9), and Orca's CLI probes the app that way
      # (ORCA_APP_EXECUTABLE, with a script as argv).
      #
      # FONTCONFIG_FILE: Orca's Linux terminalFontFamily default is exactly
      # "DejaVu Sans Mono", which the host's fontconfig lacks; see fontsConf.
      #
      # A plain exec: signals and exit codes reach Electron directly.
      makeWrapper $out/lib/orca/orca-ide $out/bin/orca-ide \
        --set FONTCONFIG_FILE ${fontsConf} \
        --run '[ -n "''${ELECTRON_RUN_AS_NODE-}" ] || set -- --ozone-platform=wayland "$@"'
    '';
    # Checked where it gates the build. The pinned trees: byte for byte, and
    # exec bit for exec bit, the AppImage's (fixup's patchShebangs and patchELF still
    # walk them; today they change nothing), and every pin in orcad's
    # manifest holding -- ripgrep/* there is resources/ripgrep
    # (packagedRipgrep). And no way to an unsandboxed Chromium: AppRun gone
    # (see installPhase), and the desktop Exec exactly the wrapper, no
    # --no-sandbox carried over from upstream's line; orca:// still handled.
    doInstallCheck = true;
    nativeInstallCheckInputs = [ pkgs.jq ];
    installCheckPhase = ''
      runHook preInstallCheck
      res=$out/lib/orca/resources
      # Exec bits and file types; write bits differ by design (chmod u+w).
      lsx() { cd $1 && find . \( -perm -u+x -printf 'x %y %p\n' \) -o -printf '- %y %p\n' | sort; }
      for d in node-runtime orcad-template ripgrep; do
        diff -r --no-dereference $src/resources/$d $res/$d
        (lsx $src/resources/$d) > src.lsx
        (lsx $res/$d) > out.lsx
        diff src.lsx out.lsx
      done
      t=$res/orcad-template
      jq -r '(.commonSha256 | to_entries[] | "\(.value)  \(.key)"),
             (.targets | to_entries[] | .key as $t | .value.files | to_entries[]
               | "\(.value)  targets/\($t)/\(.key)")' $t/orcad-template.json \
        | while read -r sha p; do
            f=$t/$p
            case $p in targets/*/ripgrep/*) [ -e "$f" ] || f=$res/''${p#targets/*/} ;; esac
            echo "$sha  $f"
          done | sha256sum -c --quiet --strict
      pin=$(tr -d '[:space:]' < $t/targets/linux-x64-glibc/.runtime-node)
      echo "$pin  $res/node-runtime/bin/node" | sha256sum -c
      [ ! -e $out/lib/orca/AppRun ]
      [ "$(grep '^Exec=' $out/share/applications/orca-ide.desktop)" = 'Exec=orca-ide %U' ]
      grep -q '^MimeType=.*x-scheme-handler/orca;' $out/share/applications/orca-ide.desktop
      runHook postInstallCheck
    '';
    meta.mainProgram = "orca-ide";
  };
  # The app's fontconfig: the host's own, plus DejaVu. Orca's Linux
  # terminalFontFamily default is exactly "DejaVu Sans Mono" with no
  # fallback, and Chromium refuses fontconfig's alias to another mono font,
  # so without it the terminal lays a proportional fallback out on a mono
  # grid; the host does not have it. Scoped to orca rather than the profile
  # because only orca hardcodes it.
  #
  # The host's, not a hermetic nixpkgs conf.d: FONTCONFIG_FILE is inherited
  # by everything Orca spawns, which, unsandboxed, is the host's own programs,
  # and they must keep seeing the host's fonts (pacman's included) -- this
  # adds one family to what they see and takes nothing away. The host's
  # conf.d links into /usr/share/fontconfig resolve now; under bwrap they
  # dangled, which is why this used to be nixpkgs' conf.d. Its 50-user.conf
  # still pulls in ~/.config/fontconfig, home-manager's font dirs.
  fontsConf = pkgs.writeText "orca-fonts.conf" ''
    <?xml version="1.0"?>
    <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
    <fontconfig>
      <include ignore_missing="no">/etc/fonts/fonts.conf</include>
      <dir>${pkgs.dejavu_fonts}/share/fonts</dir>
    </fontconfig>
  '';
  # Extracted once, shared by orca and macKeybindings; let bindings
  # are recursive, so position in the block does not matter.
  appimageContents = pkgs.appimageTools.extract { inherit pname version src; };

  # Mac-style shortcuts, the same Super-for-Cmd scheme as gtk's mac-bindings
  # and ghostty's super+c/v in home.nix: Orca's own macOS defaults, with Cmd
  # spelled out, which Orca binds to Super (event.metaKey) on Linux. The point
  # is that Ctrl and Meta stay the terminal's -- emacs keys reach the shell --
  # so no Ctrl- or Alt-only chord is left: not the Mac table's own
  # (Ctrl+1..9 tab select, Alt+Z word wrap, Ctrl+PageUp/Down), not a Linux
  # default. Only Ctrl+Tab stays, which the terminal receives as a bare Tab,
  # and the few orcaPinned below puts back on purpose.
  #
  # Generated from the app's own keybinding definitions at build time
  # (nix/orca-mac-keybindings.js), so a version bump picks up new actions.
  # Chords Hyprland binds would be dead keys, so those actions are unbound --
  # on shiori 19 of them, Super+F (find) and Super+P (go to file) among them,
  # plus Super+B/N/E, the digits, and their Shift forms. orcaPinned below
  # rebinds the ones worth having elsewhere. The build log lists every one.
  #
  # A running Orca reads the file once; after a switch, restart it (or run its
  # keybindings reload) for the new table to apply. Resetting a generated
  # shortcut in Orca's settings re-applies the Mac chord at the next switch:
  # Orca stores a reset, and a pick of its own Linux default, as the same
  # deletion, so neither sticks -- set some other chord, or unbind, instead.
  #
  # Find goes on Ctrl+S, emacs' isearch, everywhere but the terminal; there
  # C-s is the shell's own search, so terminal search is left unbound.
  #
  # Previous/next worktree, in list order, on Alt+Up/Down: the Mac chord,
  # Super+Shift+Up/Down, is Hyprland's. Orca catches it before any widget, so
  # this takes M-<up>/<down> from the terminal and the editor's move-line too,
  # deliberately.
  orcaPinned = {
    "editor.find" = [ "Ctrl+S" ];
    "browser.find" = [ "Ctrl+S" ];
    "chat.find" = [ "Ctrl+S" ];
    "settings.search" = [ "Ctrl+S" ];
    "worktree.navigateUp" = [ "Alt+ArrowUp" ];
    "worktree.navigateDown" = [ "Alt+ArrowDown" ];
  };
  hyprlandChords = let s = config.wayland.windowManager.hyprland.settings; in
    lib.concatMap
      (k: map (b: if builtins.isAttrs b && b ? _args then builtins.head b._args else b) s.${k})
      # bind, bindl, binde, ...: the lists. `binds` is a config section, a set.
      (builtins.filter (k: lib.hasPrefix "bind" k && builtins.isList s.${k}) (builtins.attrNames s));
  macKeybindings = pkgs.runCommand "orca-mac-keybindings" {
    nativeBuildInputs = [ pkgs.nodejs pkgs.asar pkgs.makeWrapper pkgs.jq ];
  } ''
    asar extract ${appimageContents}/resources/app.asar app
    mkdir -p $out/share $out/bin
    node ${./nix/orca-mac-keybindings.js} app/out/shared \
      ${pkgs.writeText "hyprland-chords.json" (builtins.toJSON hyprlandChords)} \
      ${pkgs.writeText "orca-pinned.json" (builtins.toJSON orcaPinned)} \
      > $out/share/orca-mac-keybindings.json
    # The merge runs Orca's conflict check on the merged file at activation,
    # so the keybinding module (and its one sibling import) ships alongside.
    mkdir -p $out/lib/shared
    cp -r app/out/shared/keybindings.js app/out/shared/keybindings \
      app/out/shared/tui-agent-display-names.js $out/lib/shared/
    makeWrapper ${pkgs.nodejs}/bin/node $out/bin/orca-mac-keybindings \
      --add-flags "${./nix/orca-mac-keybindings-merge.js} $out/lib/shared $out/share/orca-mac-keybindings.json"
    # One real merge, so an Orca bump that moves an import or an export the
    # merge uses fails here, not as a warning at every activation. The seed
    # is a string (comma-list) value, so the shorthand parser runs too; it
    # sits on a New-agent-tab action, unbound on every platform, so the
    # table never claims it whatever Hyprland binds. The check is that every
    # generated entry landed as generated, and the seed survived.
    mkdir -p $TMPDIR/home/.orca
    echo '{"version":1,"keybindings":{},"platforms":{"linux":{"tab.newAgent.claude":"Alt+F12, Alt+Shift+F12"}}}' \
      > $TMPDIR/home/.orca/keybindings.json
    HOME=$TMPDIR/home $out/bin/orca-mac-keybindings 2> merge.err
    if [ -s merge.err ] \
       || ! jq -e --slurpfile o $out/share/orca-mac-keybindings.json '
            .platforms.linux as $l
            | ($o[0] | to_entries | all(.value == $l[.key]))
              and $l["tab.newAgent.claude"] == "Alt+F12, Alt+Shift+F12"' \
            $TMPDIR/home/.orca/keybindings.json >/dev/null \
       || [ ! -s $TMPDIR/home/.orca/keybindings.nix-owned.json ]; then
      cat merge.err >&2; echo "orca: build-time merge did not apply the table" >&2; exit 1
    fi
  '';
  # ~/.orca/keybindings.json is Orca's read-write state: its settings UI saves
  # into the same platforms.linux section, by atomic rename, so a home.file
  # symlink would be replaced on the first edit. So the generated keys are
  # merged into whatever Orca last wrote, owning only what the last merge
  # wrote (a sidecar records it), and a UI edit outranks them -- the rules
  # are at the top of nix/orca-mac-keybindings-merge.js.
  # tests/orca-mac-keybindings.sh.
in
{
  home.packages =
    lib.optionals (!isDarwin && isDesktop) [
    # nixGL wrap: without it GBM/DRI resolve via the NixOS-only
    # /run/opengl-driver path and fall back to software rendering — the
    # same reason parsec gets it.
      (config.lib.nixGL.wrap orca)
    ];

  home.activation.orcaMacKeybindings = lib.mkIf (!isDarwin && isDesktop)
    (lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ${macKeybindings}/bin/orca-mac-keybindings
    '');
}
