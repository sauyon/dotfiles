{
  config,
  lib,
  pkgs,
  sops-nix,
  walker,
  nixgl,
  explore-mcp,
  drovr,
  hunk,
  mattpocock-skills,
  machine,

  system,
  ...
}:

let
  isDarwin = pkgs.stdenv.isDarwin;
  hostname = machine.hostname;
  isDesktop = machine.gui or true;
  gpu = machine.gpu or null;
  # Secret Service provider, keyed off one axis so the two halves cannot drift:
  # desktops get gnome-keyring, headless hosts get pass-secret-service (see
  # services.pass-secret-service below). Gating these on different axes — gui vs
  # hostname — made "no provider at all" representable, which fails silently in
  # the libsecret consumers (git credential helper, huggingface).
  gnomeKeyringHost = !isDarwin && isDesktop;

  # Emacs is NOT part of the desktop stack: it runs headless as a daemon and is
  # reached over tty/SSH with `emacsclient -t` (zsh.nix's non_gui branch already
  # assumes exactly that). Only the graphical *frame* needs a GUI, so pick the
  # build by isDesktop rather than dropping emacs on headless hosts — dropping it
  # takes $EDITOR, git core.editor and the edit/sedit helpers down with it.
  emacsPkg = if isDesktop then pkgs.emacs30-pgtk else pkgs.emacs30-nox;

  btopPkg =
    if gpu == "amd" then pkgs.btop-rocm
    else if gpu == "nvidia" then pkgs.btop-cuda
    else pkgs.btop;

  hidpi = let
    scale = if hostname == "setsuna" || hostname == "fujiwara" then 1.25 else 1.0;
    enabled = scale != 1.0;
  in {
    inherit scale enabled;
    qtFontDpi = builtins.floor (96.0 * scale);
    cursorSize = if enabled then 48 else 24;
    waybarFontSize = if enabled then 20 else 17;
    waybarBarHeight = if enabled then 48 else 42;
    ghosttyFontSize = 14;
  };

  edgeGap = if hostname == "fujiwara" then 20 else 0;
  noDpmsOutputs = [
    "HDMI-A-1"
  ];

  nixGL =
    if isDarwin || !isDesktop then
      null
    else
      pkgs.writeShellScriptBin "nixGL" ''
        exec ${nixgl.packages.${system}.nixGLIntel}/bin/nixGLIntel "$@"
      '';
  # hunk builds on all four systems, so no darwin guard needed.
  hunk-pkg = hunk.packages.${system}.default;
  # explore-mcp builds on all four systems (pure JS), so no darwin guard.
  explore-mcp-pkg = explore-mcp.packages.${system}.default;
  # drovr — Rust CLI, buildRustPackage on all unix systems; pairs with herdr.
  drovr-pkg = drovr.packages.${system}.default;

  # herdr — pinned to my fork's rev with both focus-steal fixes (pane/workspace
  # close 1df7636a + API-close f044ae8e, refs upstream #1621), rebased onto
  # upstream master dc2506ea 2026-07-26. Being ahead of v0.7.4 bumped
  # Cargo.lock/zig deps/version to 0.7.5, hence the fresh hashes and version bump
  # (for versionCheckHook). Drop for `pkgs.herdr` once the fixes ship in nixpkgs.
  herdr-pkg = pkgs.herdr.overrideAttrs (old: rec {
    version = "0.7.5";
    src = pkgs.fetchFromGitHub {
      owner = "sauyon";
      repo = "herdr";
      rev = "f044ae8ecde271b099b3444b6bb0a2dfb23e088b";
      hash = "sha256-iYVk3xWKCgVcSS1qr5Ewuu2YBHCQO9T60G6BaeUHGfs=";
    };
    cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
      inherit src;
      name = "herdr-${version}-vendor";
      hash = "sha256-Ja7fKsLWwCi6oy6zANltlFncbDVK+kgOhpr+bJtZyzg=";
    };
    zigDeps = pkgs.zig_0_15.fetchDeps {
      pname = "herdr";
      inherit version;
      src = "${src}/vendor/libghostty-vt";
      fetchAll = true;
      hash = "sha256-PnM+hZIlLyQwK8vJgd/Bhjt1lNIz06T8FahwliRmMrY=";
    };
  });

  # denoland's security firewall for agents. Not in nixpkgs and its `make` build
  # pulls Go/Node/Swift, so fetch the prebuilt linux-amd64 binary (sha from the
  # release SHA256SUMS). Only referenced under the fujiwara gate, never forced
  # on other hosts.
  clawpatrol = pkgs.stdenv.mkDerivation rec {
    pname = "clawpatrol";
    version = "0.2.11";
    src = pkgs.fetchurl {
      url = "https://github.com/denoland/clawpatrol/releases/download/v${version}/clawpatrol-linux-amd64";
      sha256 = "b6f8e017c65e51f7b538306a64965c1112154b970b37da8c61d669237e1fec22";
    };
    dontUnpack = true;
    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    installPhase = ''
      runHook preInstall
      install -Dm755 $src $out/bin/clawpatrol
      runHook postInstall
    '';
    meta.mainProgram = "clawpatrol";
  };

  # Cumora (cumora.ai) — closed-source, invite-only desktop chat app, not in
  # nixpkgs. The electron-updater feed at https://updates.cumora.ai/latest-linux.yml
  # is the source of truth for version + sha512 when bumping. Wrap the AppImage
  # (not autoPatchelf the deb) so the Electron stack runs in appimageTools' FHS
  # env, which works on non-NixOS hosts.
  cumora =
    let
      pname = "cumora";
      version = "0.1.61";
      src = pkgs.fetchurl {
        url = "https://updates.cumora.ai/Cumora-${version}.AppImage";
        hash = "sha512-+VSifBxRjeu9Y4kFVowhid1uF/htuHo2Mv5UVNiGXLgVLOFapvVd3xeKk8Cv5fgZo0yjPrAzq7EQ5PEsOQjgvA==";
      };
      appimageContents = pkgs.appimageTools.extract { inherit pname version src; };
    in
    pkgs.appimageTools.wrapType2 {
      inherit pname version src;
      # Electron safeStorage/keytar wants libsecret at runtime.
      extraPkgs = pkgs: [ pkgs.libsecret ];
      extraInstallCommands = ''
        install -Dm444 ${appimageContents}/cumora.desktop \
          $out/share/applications/cumora.desktop
        install -Dm444 ${appimageContents}/usr/share/icons/hicolor/1024x1024/apps/cumora.png \
          $out/share/icons/hicolor/1024x1024/apps/cumora.png
        substituteInPlace $out/share/applications/cumora.desktop \
          --replace-fail 'Exec=AppRun' 'Exec=cumora'
      '';
      meta.mainProgram = "cumora";
    };

  # nix's glibc ships no libnss_systemd.so.2 and only searches the nix store, so
  # getpwnam on a systemd-homed user (not in /etc/passwd) fails from nix-built
  # binaries on Arch. Symlink the host's plugin into a private dir; `withHostNss`
  # wraps a package's binaries with a narrowly-scoped LD_LIBRARY_PATH pointing at
  # it. Apply to any nix package that must resolve the current user. Inert
  # without the host file (the dangling symlink fails to dlopen and NSS skips it).
  hostNssDir = pkgs.runCommand "host-libnss-systemd" { } ''
    mkdir -p $out/lib
    ln -s /usr/lib/libnss_systemd.so.2 $out/lib/libnss_systemd.so.2
  '';

  withHostNss = drv: pkgs.symlinkJoin {
    name = "${drv.name or "pkg"}-host-nss";
    paths = [ drv ];
    # Propagate meta (notably meta.mainProgram) so lib.getExe on the wrapped
    # package (e.g. services.gpg-agent's getExe programs.gpg.package) doesn't
    # fall back to the deprecated name-guessing path. Override outputsToInstall:
    # the symlinkJoin has a single `out`, so inheriting the source's multi-output
    # list (e.g. ["out" "man"]) breaks home-manager-path.
    meta = (drv.meta or { }) // {
      outputsToInstall = [ "out" ];
    };
    nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
    postBuild = ''
      # Wrap top-level bin/ and libexec/ executables to preload the host
      # libnss_systemd.so.2.
      for d in bin libexec; do
        [ -d "$out/$d" ] || continue
        for f in "$out/$d"/*; do
          [ -L "$f" ] || continue
          tgt=$(readlink -f "$f")
          [ -f "$tgt" ] && [ -x "$tgt" ] || continue
          rm "$f"
          makeWrapper "$tgt" "$f" \
            --prefix LD_LIBRARY_PATH : ${hostNssDir}/lib
        done
      done
      # Service files (systemd + dbus) embed the unwrapped store path in
      # ExecStart=/Exec=; rewrite them so activation hits the wrappers above.
      for dir in share/systemd/user share/dbus-1/services share/dbus-1/system-services; do
        [ -d "$out/$dir" ] || continue
        for f in "$out/$dir"/*; do
          [ -L "$f" ] || continue
          tgt=$(readlink -f "$f")
          rm "$f"
          sed "s|${drv}|$out|g" "$tgt" > "$f"
        done
      done
    '';
  };

  # Dispatch dpms only to outputs that should sleep, keeping capture targets
  # like JetKVM alive when the screen idles or the lid closes.
  hyprDpmsPhysical = pkgs.writeShellScript "hypr-dpms-physical" ''
    set -eu
    ${pkgs.hyprland}/bin/hyprctl monitors all -j \
      | ${pkgs.jq}/bin/jq -r --argjson no_dpms '${builtins.toJSON noDpmsOutputs}' \
          '.[] | select(.name as $name | $no_dpms | index($name) | not) | .name' \
      | while read -r mon; do
          ${pkgs.hyprland}/bin/hyprctl dispatch "hl.dsp.dpms({ mode = \"$1\", monitor = \"$mon\" })"
        done
  '';

  # Recover Hyprland after hyprlock dies with the session still locked
  # (ext_session_lock_v1 keeps the screen locked when the client disappears).
  # Run from another TTY or SSH; relies on misc:allow_session_lock_restore so a
  # fresh hyprlock can take over the orphaned lock.
  hypr-unstuck-lock = pkgs.writeShellScriptBin "hypr-unstuck-lock" ''
    set -eu

    RUN="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
    HIS="$(ls "$RUN/hypr" 2>/dev/null | head -1 || true)"
    if [ -z "$HIS" ]; then
      echo "no hyprland instance under $RUN/hypr" >&2
      exit 1
    fi
    WD="$(ls "$RUN" 2>/dev/null | grep -E '^wayland-[0-9]+$' | head -1 || true)"
    if [ -z "$WD" ]; then
      echo "no wayland socket under $RUN" >&2
      exit 1
    fi

    if pgrep -u "$(id -u)" -x hyprlock >/dev/null 2>&1; then
      echo "hyprlock already running"
      exit 0
    fi

    # nixpkgs pam_unix.so hardcodes /run/wrappers/bin/unix_chkpwd; recreate the
    # symlink (doesn't survive reboot) or the new hyprlock can't auth.
    if [ ! -e /run/wrappers/bin/unix_chkpwd ] && [ -x /usr/sbin/unix_chkpwd ]; then
      echo "restoring /run/wrappers/bin/unix_chkpwd (sudo)..."
      sudo mkdir -p /run/wrappers/bin
      sudo ln -sf /usr/sbin/unix_chkpwd /run/wrappers/bin/unix_chkpwd
    fi

    HYPRLAND_INSTANCE_SIGNATURE="$HIS" \
      ${pkgs.hyprland}/bin/hyprctl keyword misc:allow_session_lock_restore 1 >/dev/null

    echo "launching hyprlock in transient user.slice unit..."
    exec ${pkgs.systemd}/bin/systemd-run --user --collect --quiet \
      --unit="hyprlock-rescue-$$" \
      --description="hyprlock rescue" \
      -E HYPRLAND_INSTANCE_SIGNATURE="$HIS" \
      -E WAYLAND_DISPLAY="$WD" \
      -- ${config.programs.hyprlock.package}/bin/hyprlock
  '';

  caffeine = pkgs.writeShellScriptBin "caffeine" ''
    set -eu
    PIDFILE="''${XDG_RUNTIME_DIR:-/tmp}/caffeine.pid"
    is_on() { [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; }
    case "''${1:-toggle}" in
      toggle)
        if is_on; then
          kill "$(cat "$PIDFILE")" 2>/dev/null || true
          rm -f "$PIDFILE"
        else
          systemd-inhibit --what=idle --who=caffeine --why="user toggle" \
            sleep infinity & disown
          echo $! > "$PIDFILE"
        fi
        pkill -RTMIN+10 waybar 2>/dev/null || true
        ;;
      waybar)
        if is_on; then
          echo '{"text":"󰛊","class":"on","tooltip":"Idle inhibited (caffeine on)"}'
        else
          echo '{"text":"󰒲","class":"off","tooltip":"Idle enabled"}'
        fi
        ;;
    esac
  '';

  # Force Zoom onto native Wayland Qt. Zoom's ZoomLauncher (/usr/bin/zoom ->
  # /opt/zoom/ZoomLauncher) hard-sets QT_QPA_PLATFORM=xcb, but the Hyprland
  # session has no usable Xauth (XAUTHORITY empty) so the bundled xcb plugin
  # can't reach Xwayland — Qt qFatal()s in createPlatformIntegration and SIGABRTs
  # ~1s into launch. Bypass the launcher and exec the main binary with platform
  # forced to wayland and the same LD_LIBRARY_PATH it would set for the Qt/CEF libs.
  zoom = pkgs.writeShellScriptBin "zoom" ''
    export QT_QPA_PLATFORM=wayland
    export LD_LIBRARY_PATH=/opt/zoom/Qt/lib:/opt/zoom/cef:/opt/zoom
    exec /opt/zoom/zoom "$@"
  '';

  hypr-fullscreen-inhibit = pkgs.writeShellScriptBin "hypr-fullscreen-inhibit" ''
    set -u
    PIDFILE="''${XDG_RUNTIME_DIR:-/tmp}/hypr-fullscreen-inhibit.pid"

    is_on()    { [ -f "$PIDFILE" ] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; }
    has_full() { ${pkgs.hyprland}/bin/hyprctl clients -j | ${pkgs.jq}/bin/jq -e 'any(.fullscreen != 0)' >/dev/null; }

    start_lock() {
      is_on && return
      systemd-inhibit --what=idle --who=hypr-fullscreen \
        --why="fullscreen window" sleep infinity & disown
      echo $! > "$PIDFILE"
    }
    stop_lock() {
      is_on || { rm -f "$PIDFILE"; return; }
      kill "$(cat "$PIDFILE")" 2>/dev/null || true
      rm -f "$PIDFILE"
    }
    sync() { if has_full; then start_lock; else stop_lock; fi; }

    trap 'stop_lock; exit 0' INT TERM EXIT

    sync
    SOCK="''${XDG_RUNTIME_DIR}/hypr/''${HYPRLAND_INSTANCE_SIGNATURE}/.socket2.sock"
    ${pkgs.socat}/bin/socat -u "UNIX-CONNECT:$SOCK" - | while IFS= read -r ev; do
      case "$ev" in
        fullscreen*|closewindow*|openwindow*|workspace*) sync ;;
      esac
    done
  '';


  # ── gnome-keyring, unlocked from the TPM ────────────────────────────────────
  # The login collection can only be unlocked *at daemon startup*: running
  # `gnome-keyring-daemon --unlock` against an already-running daemon exits 0
  # but leaves the collection locked (measured 2026-08-03; `--unlock` at startup
  # and PAM's `--login` both work). So this wrapper *is* the daemon -- it unseals
  # the passphrase and hands it to gnome-keyring-daemon on stdin.
  #
  # The passphrase is 32 random bytes sealed to the TPM's owner hierarchy. Only
  # seal.pub/seal.priv are kept; the parent is re-derived from a fixed template
  # on every start, so there is no persistent handle to allocate or clean up.
  # Threat model: any process running as this user can unseal it, exactly like
  # a passphraseless keyring. What it buys over an empty passphrase is that the
  # keyring file is useless off this machine -- nothing more. Prompts stop
  # either way; this is the cheaper-to-lose-a-laptop version.
  #
  # A copy is escrowed in secrets.yaml as `gnomeKeyringPassphrase`. That copy is
  # deliberately NOT wired into sops.secrets: if it were, sops-nix would decrypt
  # it to /run on every activation and the TPM would be pointless. It is cold
  # storage for re-sealing only (see gnome-keyring-tpm-seal below).
  gnome-keyring-tpm = pkgs.writeShellScriptBin "gnome-keyring-tpm" ''
    set -uo pipefail

    # The nixpkgs tpm2-tools build defaults to tcti-abrmd, a resource-manager
    # daemon this host does not run; talk to the kernel RM device instead.
    # Reading it needs the `tss` group (granted in system/deploy).
    export TPM2TOOLS_TCTI="device:/dev/tpmrm0"

    SEAL="''${XDG_DATA_HOME:-$HOME/.local/share}/gnome-keyring-tpm"
    RUN="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
    GKD="${pkgs.gnome-keyring}/bin/gnome-keyring-daemon"
    COMPONENTS="pkcs11,secrets"

    # Degrade to the stock daemon rather than leaving the session with no Secret
    # Service at all. Both fallbacks log loudly: the symptom is the unlock popup
    # coming back, and `journalctl --user -u gnome-keyring` says why.
    fallback() {
      echo "gnome-keyring-tpm: $1; starting daemon WITHOUT TPM unlock (expect unlock prompts)" >&2
      exec "$GKD" --start --foreground --components="$COMPONENTS"
    }

    [ -r "$SEAL/seal.pub" ] && [ -r "$SEAL/seal.priv" ] \
      || fallback "no sealed passphrase at $SEAL"

    WORK="$(${pkgs.coreutils}/bin/mktemp -d "$RUN/gnome-keyring-tpm.XXXXXX")"
    trap '${pkgs.coreutils}/bin/rm -rf "$WORK"' EXIT

    unseal() {
      ${pkgs.tpm2-tools}/bin/tpm2_createprimary -C o -g sha256 -G ecc \
        -c "$WORK/primary.ctx" >/dev/null 2>&1 || return 1
      ${pkgs.tpm2-tools}/bin/tpm2_load -C "$WORK/primary.ctx" \
        -u "$SEAL/seal.pub" -r "$SEAL/seal.priv" -c "$WORK/seal.ctx" >/dev/null 2>&1 || return 1
      ${pkgs.tpm2-tools}/bin/tpm2_unseal -c "$WORK/seal.ctx" 2>/dev/null || return 1
    }

    # Shell variable, never exported and never a command argument, so it shows up
    # in neither /proc/*/environ nor /proc/*/cmdline.
    PW="$(unseal)" \
      || fallback "TPM unseal failed (TPM cleared, /dev/tpmrm0 unreadable, or a tpm2-tools template change) -- re-seal with gnome-keyring-tpm-seal"

    ${pkgs.coreutils}/bin/printf '%s' "$PW" \
      | "$GKD" --unlock --foreground --components="$COMPONENTS"
  '';

  gnomeKeyringDbusService = busName: ''
    [D-BUS Service]
    Name=${busName}
    Exec=${gnome-keyring-tpm}/bin/gnome-keyring-tpm
  '';

  # One-time enrolment, and recovery after a TPM clear. Reads the passphrase on
  # stdin so it never lands in argv:
  #   sops -d --extract '["gnomeKeyringPassphrase"]' secrets.yaml | gnome-keyring-tpm-seal
  # Then restart the daemon: systemctl --user restart gnome-keyring
  gnome-keyring-tpm-seal = pkgs.writeShellScriptBin "gnome-keyring-tpm-seal" ''
    set -euo pipefail

    export TPM2TOOLS_TCTI="device:/dev/tpmrm0"
    SEAL="''${XDG_DATA_HOME:-$HOME/.local/share}/gnome-keyring-tpm"
    RUN="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"

    WORK="$(${pkgs.coreutils}/bin/mktemp -d "$RUN/gnome-keyring-seal.XXXXXX")"
    trap '${pkgs.coreutils}/bin/rm -rf "$WORK"' EXIT
    ${pkgs.coreutils}/bin/mkdir -p -m700 "$SEAL"

    ${pkgs.coreutils}/bin/cat > "$WORK/pw"
    [ -s "$WORK/pw" ] || { echo "gnome-keyring-tpm-seal: empty passphrase on stdin" >&2; exit 1; }

    ${pkgs.tpm2-tools}/bin/tpm2_createprimary -C o -g sha256 -G ecc -c "$WORK/primary.ctx" >/dev/null
    ${pkgs.tpm2-tools}/bin/tpm2_create -C "$WORK/primary.ctx" -g sha256 -i "$WORK/pw" \
      -u "$SEAL/seal.pub" -r "$SEAL/seal.priv" >/dev/null
    ${pkgs.coreutils}/bin/chmod 600 "$SEAL/seal.pub" "$SEAL/seal.priv"

    # Prove the blob round-trips before trusting it, from a freshly re-derived
    # parent -- that is the path the daemon will actually take at next start.
    ${pkgs.tpm2-tools}/bin/tpm2_createprimary -C o -g sha256 -G ecc -c "$WORK/verify.ctx" >/dev/null
    ${pkgs.tpm2-tools}/bin/tpm2_load -C "$WORK/verify.ctx" \
      -u "$SEAL/seal.pub" -r "$SEAL/seal.priv" -c "$WORK/vseal.ctx" >/dev/null
    ${pkgs.tpm2-tools}/bin/tpm2_unseal -c "$WORK/vseal.ctx" -o "$WORK/verify"
    ${pkgs.diffutils}/bin/cmp -s "$WORK/pw" "$WORK/verify" \
      || { echo "gnome-keyring-tpm-seal: seal/unseal round-trip MISMATCH, not trusting this blob" >&2; exit 1; }

    echo "gnome-keyring-tpm-seal: sealed to $SEAL and verified"
  '';

  # git built with the libsecret credential helper (git-credential-libsecret),
  # used for HTTPS auth to hosts that have no CLI-managed token store.
  # gitFull ships git-credential-libsecret and is cached by Hydra; the
  # withLibsecret override wasn't, so it recompiled git on every nixpkgs bump.
  gitWithLibsecret = pkgs.gitFull;

  # Git credential helper backed by fj's own login store, so fj is the single
  # place a Forgejo token lives (forge.ko.ag is HTTPS-only via a Cloudflare
  # tunnel). fj has no `git-credential` subcommand, so this shim reads keys.json
  # directly.
  #
  # Both hosts are logged in with `fj auth add-token` (LoginInfo::Application),
  # not `fj auth login` (OAuth). Application tokens have no expiry, so nothing
  # here has to trigger a refresh. Keep it that way: fj's OAuth refresh rotates
  # the token and rewrites keys.json with no flock and a truncate-in-place write
  # (upstream src/keys.rs save()), and a refresh whose new token never lands on
  # disk wedges the login permanently with "token was already used". Re-auth
  # then needs `fj auth login`, which shells out to xdg-open — useless on a
  # headless box over SSH.
  git-credential-fj = pkgs.writeShellScriptBin "git-credential-fj" ''
    set -euo pipefail

    # Only `get` is ours to answer — fj owns store/erase.
    [ "''${1:-}" = "get" ] || exit 0

    host=""
    while IFS='=' read -r key value; do
      [ -n "$key" ] || break
      case "$key" in
        host) host="$value" ;;
      esac
    done
    [ -n "$host" ] || exit 0

    keys="''${XDG_DATA_HOME:-$HOME/.local/share}/forgejo-cli/keys.json"
    [ -r "$keys" ] || exit 0

    # A nonzero exit aborts git's whole operation rather than falling through
    # to a prompt, so a half-written keys.json degrades to "no credential".
    token=$(${lib.getExe pkgs.jq} -r --arg h "$host" '.hosts[$h].token // empty' "$keys" 2>/dev/null) || exit 0
    [ -n "$token" ] || exit 0

    # Forgejo ignores the basic-auth username when the password is a token.
    printf 'username=oauth2\npassword=%s\n' "$token"
  '';

  claude-prof = pkgs.writeShellScriptBin "claude-prof" ''
    set -euo pipefail
    CONFIG_HOME="''${XDG_CONFIG_HOME:-$HOME/.config}"

    cmd="''${1:-help}"
    shift || true

    profile_dir() { echo "$CONFIG_HOME/claude-$1"; }

    case "$cmd" in
      list|ls)
        found=0
        for d in "$CONFIG_HOME"/claude-*/; do
          [ -d "$d" ] || continue
          basename "$d" | sed 's/^claude-//'
          found=1
        done
        [ "$found" = 1 ] || echo "No profiles."
        ;;
      rm|delete)
        name="''${1:?usage: claude-prof rm <name>}"
        dir="$(profile_dir "$name")"
        [ -d "$dir" ] || { echo "error: profile '$name' not found"; exit 1; }
        rm -rf "$dir"
        echo "Deleted profile: $name"
        ;;
      run)
        name="''${1:?usage: claude-prof run <name> [claude-args...]}"
        shift
        dir="$(profile_dir "$name")"
        mkdir -p "$dir"

        # settings.json is a nix-rendered store symlink (see home.file
        # entries above) — do NOT touch it here. settings.local.json is left
        # unmanaged so Claude Code can write to it.
        # CLAUDE.md / commands/ / projects/ symlinks are set up by
        # home.activation.claudeProfiles. This script only ensures the
        # profile dir exists (for a brand-new profile before the first
        # home-manager switch) and applies per-profile env vars.

        # Route clz (zai) directly to Z.AI's Anthropic-compatible endpoint.
        # Z.AI uses Bearer auth (Authorization header), not x-api-key — the
        # gateway's split-token dance was unnecessary. See
        # https://docs.z.ai/devpack/tool/claude for the upstream config.
        if [ "$name" = "zai" ] && [ -r ~/.config/opencode/zai-key ]; then
          exec env \
            CLAUDE_CONFIG_DIR="$dir" \
            ANTHROPIC_BASE_URL="https://api.z.ai/api/anthropic" \
            ANTHROPIC_AUTH_TOKEN="$(tr -d '\n' < ~/.config/opencode/zai-key)" \
            ANTHROPIC_MODEL="glm-5.2" \
            ANTHROPIC_DEFAULT_OPUS_MODEL="glm-5.2" \
            ANTHROPIC_DEFAULT_SONNET_MODEL="glm-5.2" \
            ANTHROPIC_DEFAULT_HAIKU_MODEL="glm-4.7" \
            CLAUDE_CODE_AUTO_COMPACT_WINDOW="1000000" \
            claude "$@"
        else
          exec env CLAUDE_CONFIG_DIR="$dir" claude "$@"
        fi
        ;;
      help|--help|-h)
        echo "Usage: claude-prof <command> [args]"
        echo ""
        echo "Commands:"
        echo "  list               list profiles"
        echo "  run <name> [args]  run claude with the named profile"
        echo "  rm <name>          delete a profile"
        ;;
      *)
        echo "error: unknown command '$cmd'. Try 'claude-prof help'." >&2
        exit 1
        ;;
    esac
  '';

  args = { inherit config lib pkgs; };

  # The local auto-mode classifier's PreToolUse entry. Currently registered by no
  # profile — add it back to `claudeBaseSettings.hooks.PreToolUse` to re-enable
  # (the plugin's files are still installed by home.file below).
  localAutoModeHook = {
    matcher = ".*";
    hooks = [ {
      type = "command";
      command = "python3 ${config.home.homeDirectory}/.claude/plugins/local-auto-mode/classifier.py";
      timeout = 15;
    } ];
  };

  # Base Claude Code settings shared by every profile. Only `model` differs per
  # profile (see `claudeProfiles`); everything else is rendered identically into
  # each profile's settings.json by the home.file entries below, so per-profile
  # state is fully declarative.
  claudeBaseSettings = {
    hooks = {
      PreToolUse = [
        {
          matcher = "Edit|Write|NotebookEdit";
          hooks = [ {
            type = "command";
            # Refuse edits to a repo's PRIMARY checkout while it sits on the
            # default branch. Sustained agent work there is invisible until it
            # goes wrong: the tree moves under the agent while it reads, HEAD
            # advances past the commit it is reviewing, and a test count gets
            # reported about a state nobody has any more. drovr's `worktree =
            # true` fixes that for `drovr new` runs, but inline work -- which is
            # most of it -- creates no run and so binds to nothing.
            #
            # Detection is structural, not by path: a LINKED worktree has .git
            # as a file pointing at the real gitdir, the primary checkout has a
            # directory. So .drovr/wt/* and .claude/worktrees/* pass untouched
            # and need no allowlist to maintain.
            #
            # CLAUDE_ALLOW_MAIN_EDIT=1 is the way through for the one-line fix
            # drovr:worktrees explicitly says not to isolate. It has to be an
            # env var rather than a prompt so that skipping isolation is a
            # deliberate act and shows up in the transcript.
            #
            # `.claude/allow-main-edit` at a repo root opts that repo out
            # permanently. Only for trees small enough to read in one pass.
            command = ''
              [ -n "$CLAUDE_ALLOW_MAIN_EDIT" ] && exit 0
              f=$(${pkgs.jq}/bin/jq -r '.tool_input.file_path // empty')
              [ -n "$f" ] || exit 0
              # Walk up to the nearest EXISTING directory: a Write may create
              # both the file and the directories above it.
              d=$f; while [ ! -d "$d" ] && [ "$d" != "/" ]; do d=$(dirname "$d"); done
              root=$(${pkgs.git}/bin/git -C "$d" rev-parse --show-toplevel 2>/dev/null) || exit 0
              [ -d "$root/.git" ] || exit 0
              [ -e "$root/.claude/allow-main-edit" ] && exit 0
              br=$(${pkgs.git}/bin/git -C "$root" rev-parse --abbrev-ref HEAD 2>/dev/null)
              case "$br" in main|master) ;; *) exit 0 ;; esac
              printf '%s' '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"This is the primary checkout on its default branch. Work in a worktree instead: `drovr new <run> --worktree` then EnterWorktree, or EnterWorktree on an existing one. For a genuine one-line fix you will finish and commit yourself, re-run with CLAUDE_ALLOW_MAIN_EDIT=1 set."}}'
            '';
          } ];
        }
        {
          matcher = "Bash";
          hooks = [ {
            type = "command";
            # Block `gh pr create` outside the quite-app worktree. Inspect
            # tool_input.command from the stdin hook JSON ourselves, since
            # `matcher` only filters on tool name.
            command = ''
              case "$PWD" in ${config.home.homeDirectory}/devel/quite-app*) exit 0 ;; esac
              input=$(cat)
              case "$input" in *'"command":"gh pr create'*) ;; *) exit 0 ;; esac
              printf '%s' '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"Do not run `gh pr create`. Print a PR creation link instead (e.g. https://github.com/<owner>/<repo>/compare/<base>...<head>?expand=1, or https://github.com/<owner>/<repo>/pull/new/<branch>) and let the user create the PR themselves."}}'
            '';
          } ];
        }
        {
          matcher = "Bash";
          hooks = [ {
            type = "command";
            # Block `coder ssh` anywhere in a command: the ssh config already
            # proxies coder workspaces through plain ssh (coder.* / *.coder
            # blocks below), keeping known-hosts and config in one place.
            command = ''
              input=$(cat)
              case "$input" in *'coder ssh'*) ;; *) exit 0 ;; esac
              printf '%s' '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"Do not use `coder ssh`. The ssh config already proxies coder workspaces; use standard ssh instead: `ssh coder.<workspace>` (or `ssh <workspace>.coder`)."}}'
            '';
          } ];
        }
        {
          matcher = "mcp__github__create_pull_request";
          hooks = [ {
            type = "command";
            # Mirror the `gh pr create` hook for the GitHub MCP tool: block PR
            # creation outside the quite-app worktree. A flat permissions.deny
            # can't be scoped to a directory, so gate on $PWD here.
            command = ''
              case "$PWD" in ${config.home.homeDirectory}/devel/quite-app*) exit 0 ;; esac
              printf '%s' '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"deny","permissionDecisionReason":"Do not create pull requests here. Print a PR creation link instead (e.g. https://github.com/<owner>/<repo>/pull/new/<branch>) and let the user create the PR themselves."}}'
            '';
          } ];
        }
      ];
      PostToolUseFailure = [];
      # Per-session kcs KUBECONFIG isolation: mint a session id and point
      # KUBECONFIG at its kcs socket dir (mirrors zsh.nix `kcs init`), written to
      # $CLAUDE_ENV_FILE so it applies for the whole session. The base kubeconfig
      # is a prod-stripped copy of ~/.kube/config: every "prod" context is removed
      # and current-context unset, so Claude can never reach a prod cluster (bare
      # kubectl fails instead of inheriting the last-selected context).
      SessionStart = [
        {
          hooks = [ {
            type = "command";
            command = ''
              SESSION_ID="claude-$(openssl rand -hex 4)"
              KCS_DIR="''${XDG_RUNTIME_DIR:-$HOME/.local/run}/kcs/sessions"
              mkdir -p "$KCS_DIR"
              BASE="$KCS_DIR/$SESSION_ID-base"
              if cp "$HOME/.kube/config" "$BASE" 2>/dev/null; then
                chmod 600 "$BASE"
                ${pkgs.kubectl}/bin/kubectl --kubeconfig "$BASE" config get-contexts -o name | grep -i prod | while IFS= read -r c; do
                  ${pkgs.kubectl}/bin/kubectl --kubeconfig "$BASE" config delete-context "$c" >/dev/null
                done
                ${pkgs.kubectl}/bin/kubectl --kubeconfig "$BASE" config unset current-context >/dev/null
              fi
              echo "export KCS_SESSION=$SESSION_ID" >> "$CLAUDE_ENV_FILE"
              echo "export KUBECONFIG=$KCS_DIR/$SESSION_ID:$BASE" >> "$CLAUDE_ENV_FILE"
            '';
          } ];
        }
        {
          # herdr integration: report the Claude session identity to the local
          # herdr socket on session start so a herdr pane can restore it. No-op
          # unless HERDR_ENV=1 (inside a herdr pane), so inert outside herdr.
          # Vendored verbatim from `herdr integration install claude` (v7);
          # regenerate and bump if `herdr integration status` reports it outdated.
          hooks = [ {
            type = "command";
            command = "bash '${config.home.homeDirectory}/.claude/hooks/herdr-agent-state.sh' session";
            timeout = 10;
          } ];
        }
      ];
      # herdr integration: name this pane in the Agents panel after Claude's OSC
      # terminal title. Refreshed at turn start (UserPromptSubmit), during work
      # (PostToolUse — title reliably populated then), and turn end (Stop). No-op
      # unless HERDR_ENV=1. See home.file entry below.
      UserPromptSubmit = [
        {
          hooks = [ {
            type = "command";
            command = "bash '${config.home.homeDirectory}/.claude/hooks/herdr-agent-name.sh'";
            timeout = 5;
          } ];
        }
      ];
      PostToolUse = [
        {
          matcher = ".*";
          hooks = [ {
            type = "command";
            command = "bash '${config.home.homeDirectory}/.claude/hooks/herdr-agent-name.sh'";
            timeout = 5;
          } ];
        }
      ];
      Stop = [
        {
          hooks = [ {
            type = "command";
            command = "bash '${config.home.homeDirectory}/.claude/hooks/herdr-agent-name.sh'";
            timeout = 5;
          } ];
        }
      ];
    };
    permissions = {
      allow = [
        "Bash(mise run:*)"
        "Bash(home-manager switch)"
        "mcp__claude_ai_Slack__slack_read_channel"
        "mcp__claude_ai_Slack__slack_read_thread"
        "mcp__claude_ai_Slack__slack_read_canvas"
        "mcp__claude_ai_Slack__slack_read_user_profile"
        "mcp__claude_ai_Notion__notion-fetch"
        "mcp__claude_ai_Notion__notion-get-comments"
        "mcp__claude_ai_Notion__notion-search"
        "mcp__claude_ai_Notion__notion-query-data-sources"
        "mcp__claude_ai_Notion__notion-query-meeting-notes"
        "mcp__claude_ai_Notion__notion-get-teams"
        "mcp__claude_ai_Notion__notion-get-users"
        "mcp__claude_ai_Linear__get_issue"
        "mcp__claude_ai_Linear__get_project"
        "mcp__claude_ai_Linear__get_team"
        "mcp__claude_ai_Linear__get_user"
        "mcp__claude_ai_Linear__list_issues"
        "mcp__claude_ai_Linear__list_projects"
        "mcp__claude_ai_Linear__list_teams"
        "mcp__claude_ai_Linear__list_users"
        "mcp__claude_ai_Linear__list_comments"
        "mcp__claude_ai_Linear__get_document"
        "mcp__claude_ai_Linear__list_documents"
        "mcp__claude_ai_Linear__get_initiative"
        "mcp__claude_ai_Linear__list_initiatives"
        "mcp__claude_ai_Linear__get_milestone"
        "mcp__claude_ai_Linear__list_milestones"
        "mcp__claude_ai_Linear__get_status_updates"
        "mcp__claude_ai_Linear__list_cycles"
        "mcp__claude_ai_Linear__list_issue_labels"
        "mcp__claude_ai_Linear__list_issue_statuses"
        "mcp__claude_ai_Linear__list_project_labels"
        "mcp__claude_ai_Linear__get_authenticated_user"
        "mcp__claude_ai_Linear__get_attachment"
        "mcp__claude_ai_Linear__get_issue_status"
        "mcp__claude_ai_Linear__search_documentation"
        "mcp__github__get_commit"
        "mcp__github__get_copilot_job_status"
        "mcp__github__get_file_contents"
        "mcp__github__get_label"
        "mcp__github__get_latest_release"
        "mcp__github__get_me"
        "mcp__github__get_release_by_tag"
        "mcp__github__get_tag"
        "mcp__github__get_team_members"
        "mcp__github__get_teams"
        "mcp__github__issue_read"
        "mcp__github__list_branches"
        "mcp__github__list_commits"
        "mcp__github__list_issue_types"
        "mcp__github__list_issues"
        "mcp__github__list_pull_requests"
        "mcp__github__list_releases"
        "mcp__github__list_tags"
        "mcp__github__pull_request_read"
        "mcp__github__search_code"
        "mcp__github__search_issues"
        "mcp__github__search_pull_requests"
        "mcp__github__search_repositories"
        "mcp__github__search_users"
        "Skill(evaluate)"
      ];
      # PR creation is gated per-repo by the PreToolUse hooks above (denied
      # except in the quite-app worktree), not a blanket deny. A flat deny here
      # can't be scoped to a directory, and the auto-mode classifier reads it as
      # a global block — over-blocking quite-app.
      deny = [];
      defaultMode = "auto";
    };
    # Rules for the auto-mode classifier. It reads ~/.claude/settings.json
    # (rendered from these base settings via programs.claude-code below) and
    # unions every autoMode.allow it finds, so one list here covers all profiles
    # — its SETTINGS_PATHS never looks in the ~/.config/claude-<name>/ dirs.
    autoMode = {
      allow = [
        "$defaults"
        # PR creation is fine inside quite-app, so don't block the hook-allowed
        # path there. (cwd is in the classifier's prompt; outside quite-app the
        # hooks above still deny deterministically.)
        "Creating a pull request (`gh pr create`, a `gh api` POST to a repo's pulls endpoint, or mcp__github__create_pull_request) is ALLOWED when the working directory is under ${config.home.homeDirectory}/devel/quite-app. PR creation stays blocked in every other directory."
        "Git Push to Default Branch is allowed when the current working directory is under ${config.home.homeDirectory}/devel/kube. That repo is a personal single-maintainer GitOps tree where direct pushes to main are the intended workflow; no PR review applies."
      ];
    };
    # Declare marketplaces here instead of shelling out to `claude plugin
    # marketplace add` at activation: Claude Code registers every entry into
    # <config dir>/plugins/known_marketplaces.json on startup, overwriting a
    # stale same-name entry from this source. This makes the drovr pin
    # self-correcting on the first launch after a flake.lock bump, and applies to
    # every profile (base settings render into all) rather than only the ambient
    # CLAUDE_CONFIG_DIR during the switch. See
    # https://code.claude.com/docs/en/plugin-marketplaces.
    #
    # drovr is pinned to the flake.lock'd source tree (its repo root, with
    # skills/ hooks/ .claude-plugin/) rather than cloned from the GitHub default
    # branch: an anonymous clone can race a fresh push and land on a stale commit
    # predating hooks/, silently dropping the SessionStart reflex.
    extraKnownMarketplaces = {
      claude-plugins-official.source = {
        source = "github";
        repo = "anthropics/claude-plugins-official";
      };
      drovr.source = {
        source = "directory";
        path = drovr.outPath;
      };
    };
    enabledPlugins = {
      "rust-analyzer-lsp@claude-plugins-official" = true;
      "clangd-lsp@claude-plugins-official" = true;
      "slack@claude-plugins-official" = true;
      "pyright-lsp@claude-plugins-official" = true;
      "code-simplifier@claude-plugins-official" = true;
      "ralph-loop@claude-plugins-official" = true;
      "drovr@drovr" = true;
    };
    mcpServers = {
      unifi = {
        type = "stdio";
        command = "sh";
        args = [
          "-c"
          "UNIFI_API_KEY=$(cat ${config.home.homeDirectory}/.config/unifi/api-key) exec ${config.home.homeDirectory}/.local/share/mise/shims/uvx unifi-mcp-server"
        ];
        env = {
          UNIFI_API_TYPE = "local";
          UNIFI_LOCAL_HOST = "10.0.0.1";
          UNIFI_LOCAL_VERIFY_SSL = "false";
        };
      };
      explore-mcp = {
        type = "stdio";
        command = "${explore-mcp-pkg}/bin/explore-mcp";
        env = {
          EXPLORE_MCP_CONFIG = "${config.home.homeDirectory}/.config/explore-mcp/config.json";
        };
      };
    };
    # `model` intentionally omitted — declared per-profile in `claudeProfiles`.
    theme = "dark";
    editorMode = "normal";
    # Ghost-text next-prompt suggestions render in the composer's input line, so
    # a pane reads as though the text were already typed and pending submission.
    promptSuggestionEnabled = false;
    autoDreamEnabled = true;
    agentPushNotifEnabled = true;
    skipWorkflowUsageWarning = true;
    skipDangerousModePermissionPrompt = true;
    skipAutoPermissionPrompt = true;
    tui = "fullscreen";
    statusLine = {
      type = "command";
      command = "${config.home.homeDirectory}/.claude/statusline-command.sh";
      padding = 0;
    };
  };

  # Per-profile overrides. Add a profile here — the home.file entries and
  # home.activation.claudeProfiles below pick it up automatically. `zai` picks
  # its model via the ANTHROPIC_MODEL env var at exec time (see claude-prof run),
  # so its settings.json model field is just the default /model and /config show.
  claudeProfiles = {
    personal = {
      model = "claude-opus-5";
    };
    work = {
      model = "claude-opus-5";
    };
    zai = {
      model = "opus";
    };
  };

  # Per-profile full settings = base + per-profile overrides.
  claudeProfileSettings = lib.mapAttrs
    (_: overrides: claudeBaseSettings // overrides)
    claudeProfiles;

  # Rendered JSON files in the Nix store. Each per-profile settings.json also
  # gets `$schema` injected (the home-manager claude-code module adds it for
  # ~/.claude/settings.json, but per-profile files bypass that module).
  claudeProfileSettingsJson = lib.mapAttrs
    (name: settings:
      pkgs.writeText "claude-${name}-settings.json"
        (builtins.toJSON (settings // {
          "$schema" = "https://json.schemastore.org/claude-code-settings.json";
        })))
    claudeProfileSettings;

  newtabLinks = [
    { group = "Work"; links = [
      { name = "Gmail";       url = "https://mail.google.com"; }
      { name = "Google Docs"; url = "https://docs.google.com"; }
      { name = "GitHub";      url = "https://github.com"; }
      { name = "Notion";          url = "https://www.notion.so"; }
      { name = "Rippling";        url = "https://app.rippling.com"; }
      { name = "Cloud (prod)";    url = "https://console.modular.com"; }
      { name = "Cloud (staging)"; url = "https://mcloud-staging.bentoml.ai"; }
    ];}
    { group = "Infra"; links = [
      { name = "Okta";       url = "https://modular.okta.com"; }
      { name = "AWS";        url = "https://d-906789f3a0.awsapps.com"; }
      { name = "Datadog";    url = "https://app.datadoghq.com"; }
      { name = "ArgoCD";     url = "https://argocd.prod.modular-internal.com"; }
      { name = "BentoML ArgoCD"; url = "https://argocd.tail1beac.ts.net"; }
      { name = "Tailscale";  url = "https://login.tailscale.com"; }
      { name = "Cloudflare"; url = "https://dash.cloudflare.com"; }
      { name = "OpenShift";  url = "https://console.redhat.com"; }
    ];}
    { group = "Other"; links = [
      { name = "Reddit";       url = "https://www.reddit.com"; }
      { name = "YouTube";      url = "https://www.youtube.com"; }
      { name = "YT Music";     url = "https://music.youtube.com"; }
      { name = "Claude";       url = "https://claude.ai"; }
      { name = "Amazon";       url = "https://www.amazon.com"; }
      { name = "Zillow";       url = "https://www.zillow.com"; }
    ];}
  ];

  renderLink = l: ''<a href="${l.url}">${l.name}</a>'';
  renderGroup = g: ''
    <div class="group">
      <h2>${g.group}</h2>
      <div class="links">${lib.concatMapStrings renderLink g.links}</div>
    </div>'';

  newtabHtml = ''
    <!DOCTYPE html>
    <html lang="en">
    <head>
    <meta charset="utf-8">
    <title>New Tab</title>
    <style>
      * { margin: 0; padding: 0; box-sizing: border-box; }
      body {
        background: #1a1a1a;
        color: #e0e0e0;
        font-family: system-ui, -apple-system, sans-serif;
        display: flex;
        justify-content: center;
        padding-top: 15vh;
      }
      .container { max-width: 60vw; width: 100%; }
      h2 {
        font-size: 1.2vh;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #888;
        margin-bottom: 0.8vh;
        text-align: center;
      }
      .group { margin-bottom: 2.5vh; }
      .links { display: flex; flex-wrap: wrap; gap: 0.6vh; justify-content: center; }
      a {
        color: #c0c0c0;
        text-decoration: none;
        font-size: 1.6vh;
        padding: 0.6vh 1.2vh;
        border-radius: 0.5vh;
        background: #252525;
        transition: background 0.1s, color 0.1s;
      }
      a:hover { background: #333; color: #fff; }
    </style>
    </head>
    <body>
    <div class="container">
    ${lib.concatMapStrings renderGroup newtabLinks}
    </div>
    </body>
    </html>
  '';
in
{
  imports = [ sops-nix.homeManagerModules.sops walker.homeManagerModules.default ./antigravity.nix ./opencode.nix ./cursor-agent.nix ];

  home.stateVersion = "26.05";

  # ── sops-nix ────────────────────────────────────────────────────────────────
  sops.defaultSopsFile = ./secrets.yaml;
  # Decryption is GCP KMS (see .sops.yaml + GOOGLE_APPLICATION_CREDENTIALS below).
  # sops-nix still asserts *some* age/gpg key source, and sops-install-secrets
  # opens the configured keyFile at runtime — so declare an empty managed file
  # to satisfy both.
  home.file.".config/sops/age-unused.txt".text = "";
  sops.age.keyFile = "${config.home.homeDirectory}/.config/sops/age-unused.txt";
  sops.age.sshKeyPaths = [];
  sops.gnupg.sshKeyPaths = [];
  sops.environment.GOOGLE_APPLICATION_CREDENTIALS = "${config.home.homeDirectory}/.config/sops/gcp-key.json";

  # ── Modular API (local auto-mode classifier) ────────────────────────────────
  sops.secrets.modularApiKey = {
    path = "${config.home.homeDirectory}/.config/local-auto-mode/api-key";
    mode = "0600";
  };

  # ── ko.ag API (opencode provider + local-auto-mode classifier) ─────────────
  # This is now litellm's MASTER KEY, not the old CF AI Gateway token: ai.ko.ag
  # was deleted and both consumers dial the router's LAN address directly. The
  # same value must exist in the cluster as the `litellm-master-key` Secret in
  # the litellm / hakobiya / opencode namespaces — rotating here without
  # rotating there 401s everything. See the kube repo's docs/litellm-access.md.
  sops.secrets.koAgApiKey = {
    path = "${config.home.homeDirectory}/.config/opencode/ko-ag-key";
    mode = "0600";
  };

  # ── Z.AI API (opencode zai / zai-coding-plan providers) ───────────────────
  sops.secrets.zaiApiKey = {
    path = "${config.home.homeDirectory}/.config/opencode/zai-key";
    mode = "0600";
  };

  # ── Modular private endpoint base URL (opencode mcloud provider) ────────────
  # Kept in sops so the internal hostname never lands in the committed config.
  sops.secrets.modularApiUrl = {
    path = "${config.home.homeDirectory}/.config/opencode/mcloud-base-url";
    mode = "0600";
  };

  # ── UniFi API key (unifi-mcp-server) ───────────────────────────────────────
  sops.secrets.unifiApiKey = {
    path = "${config.home.homeDirectory}/.config/unifi/api-key";
    mode = "0600";
  };

  # ── Global Claude preferences (loaded into every conversation) ────────────
  home.file.".claude/CLAUDE.md".source = ./home/.claude/CLAUDE.md;

  # ── Claude statusline ──────────────────────────────────────────────────────
  home.file.".claude/statusline-command.sh" = {
    source = ./home/.claude/statusline-command.sh;
    executable = true;
  };

  # ── Claude skills ──────────────────────────────────────────────────────────
  home.file.".claude/skills/linear-flow/SKILL.md".source =
    ./home/.claude/skills/linear-flow/SKILL.md;
  home.file.".claude/skills/linear-flow/DESIGN.md".source =
    ./home/.claude/skills/linear-flow/DESIGN.md;
  home.file.".claude/skills/agy-review/SKILL.md".source =
    ./home/.claude/skills/agy-review/SKILL.md;
  # Transform helper the skill invokes ($SKILL_DIR/findings-to-agent-context.py):
  # maps agy's JSON findings to a Hunk --agent-context sidecar.
  home.file.".claude/skills/agy-review/findings-to-agent-context.py".source =
    ./home/.claude/skills/agy-review/findings-to-agent-context.py;
  # Symlinks the skill bundled with the `hunk` package (hunkdiff) into
  # ~/.claude/skills/ so Claude can drive live Hunk review sessions via the
  # `hunk session *` CLI.
  home.file.".claude/skills/hunk-review/SKILL.md".source =
    "${hunk-pkg}/skills/hunk-review/SKILL.md";
  # /teach — stateful tutor that treats the cwd as a learning workspace
  # (MISSION.md, lessons/, learning-records/). Whole-directory symlink: the
  # skill reads its own *-FORMAT.md siblings by relative path. Pinned via
  # flake.lock; `nix flake update mattpocock-skills` to bump.
  home.file.".claude/skills/teach".source =
    "${mattpocock-skills}/skills/productivity/teach";

  # Cursor discovers SKILL.md files recursively and follows directory symlinks.
  # Expose the same pinned drovr skills that Claude loads through its plugin.
  home.file.".cursor/skills/drovr".source =
    "${drovr-pkg}/share/drovr/skills";

  # The drovr marketplace source tree at the locked rev, as a stable GC-rooted
  # path: the plugin's repo root (skills/, hooks/, .claude-plugin/). Claude is
  # pointed here by `extraKnownMarketplaces` above; this symlink is the
  # human-legible handle on the same path (and a `claude plugin marketplace add`
  # target for a non-nix profile). Bump the pin = bump flake.lock.
  home.file.".local/share/drovr-marketplace".source = drovr.outPath;

  # ── Shared agent slash commands (claude / cursor / opencode) ──────────────
  # explain-diff prompt by Geoffrey Litt, from
  # https://gist.github.com/geoffreylitt/a29df1b5f9865506e8952488eac3d524
  # (no license declared; see attribution note in the file)
  home.file.".claude/commands/explain-diff.md".source =
    ./home/agent-commands/explain-diff.md;
  home.file.".cursor/commands/explain-diff.md".source =
    ./home/agent-commands/explain-diff.md;
  xdg.configFile."opencode/command/explain-diff.md".source =
    ./home/agent-commands/explain-diff.md;

  # review-diff: companion to explain-diff — an annotated reviewer's diff
  # (logical grouping, inline annotations, codebase context, severity findings).
  home.file.".claude/commands/review-diff.md".source =
    ./home/agent-commands/review-diff.md;
  home.file.".cursor/commands/review-diff.md".source =
    ./home/agent-commands/review-diff.md;
  xdg.configFile."opencode/command/review-diff.md".source =
    ./home/agent-commands/review-diff.md;

  # /agy-review slash command → drives the agy-review skill (agy CLI). Claude-only:
  # personal ~/.claude/skills/* aren't exposed as slash commands, so this wrapper
  # is what makes `/agy-review` available, pointing at the skill's SKILL.md pipeline.
  home.file.".claude/commands/agy-review.md".source =
    ./home/agent-commands/agy-review.md;

  # /hunk-review slash command → drives the hunk-review skill (bundled with the
  # hunk package). Claude-only: personal ~/.claude/skills/* aren't exposed as
  # slash commands, so this wrapper points at the skill's `hunk session *`
  # workflow for a live Hunk review session.
  home.file.".claude/commands/hunk-review.md".source =
    ./home/agent-commands/hunk-review.md;

  # ── herdr integration (Claude) ─────────────────────────────────────────────
  # SessionStart hook script referenced by claudeBaseSettings.hooks.SessionStart
  # above. Vendored verbatim from `herdr integration install claude`; no-op
  # outside a herdr pane. All profiles reference this one path.
  home.file.".claude/hooks/herdr-agent-state.sh" = {
    source = ./home/.claude/hooks/herdr-agent-state.sh;
    executable = true;
  };

  # Names each pane in herdr's Agents panel after Claude's OSC terminal title
  # (its conversation summary). Referenced by claudeBaseSettings.hooks
  # (UserPromptSubmit/PostToolUse/Stop); no-op unless HERDR_ENV=1.
  home.file.".claude/hooks/herdr-agent-name.sh" = {
    source = ./home/.claude/hooks/herdr-agent-name.sh;
    executable = true;
  };

  # ── herdr integration (Cursor) ─────────────────────────────────────────────
  # sessionStart hook script + hooks.json wiring for the cursor-agent CLI.
  # Vendored verbatim from `herdr integration install cursor` (v1); no-op unless
  # HERDR_ENV=1. hooks.json is generated here (not vendored) so the absolute
  # script path tracks homeDirectory. Regenerate and bump if `herdr integration
  # status` reports it outdated.
  home.file.".cursor/herdr-agent-state.sh" = {
    source = ./home/.cursor/herdr-agent-state.sh;
    executable = true;
  };
  home.file.".cursor/hooks.json".text = builtins.toJSON {
    hooks.sessionStart = [
      { command = "bash '${config.home.homeDirectory}/.cursor/herdr-agent-state.sh' session"; }
    ];
    version = 1;
  };

  # ── herdr integration (pi) ─────────────────────────────────────────────────
  # Extension auto-loaded by pi from ~/.pi/agent/extensions; reports lifecycle
  # state and session identity to the local herdr socket. Vendored verbatim from
  # `herdr integration install pi` (v7); self-contained (no config registration
  # needed) and inert unless HERDR_ENV=1. Regenerate and bump if `herdr
  # integration status` reports it outdated.
  home.file.".pi/agent/extensions/herdr-agent-state.ts".source =
    ./home/.pi/agent/extensions/herdr-agent-state.ts;

  # ── Claude plugins ─────────────────────────────────────────────────────────
  home.file.".claude/plugins/local-auto-mode/hooks.json".source =
    ./home/.claude/plugins/local-auto-mode/hooks.json;
  home.file.".claude/plugins/local-auto-mode/classifier.py".source =
    ./home/.claude/plugins/local-auto-mode/classifier.py;
  home.file.".claude/plugins/local-auto-mode/prompt.py".source =
    ./home/.claude/plugins/local-auto-mode/prompt.py;
  home.file.".claude/plugins/local-auto-mode/config.py".source =
    ./home/.claude/plugins/local-auto-mode/config.py;

  # ── Per-profile Claude settings.json (store symlinks, fully declarative) ───
  # force = true replaces any pre-existing regular files (the old runtime-copied
  # per-profile settings.json). home.activation.claudeProfiles also rm -f's them
  # before linkGeneration so the byte-identical cmp -s skip doesn't leave the old
  # file in place.
  home.file = {
    ".config/claude-personal/settings.json".source = claudeProfileSettingsJson.personal;
    ".config/claude-work/settings.json".source     = claudeProfileSettingsJson.work;
    ".config/claude-zai/settings.json".source      = claudeProfileSettingsJson.zai;
    ".config/claude-personal/settings.json".force  = true;
    ".config/claude-work/settings.json".force      = true;
    ".config/claude-zai/settings.json".force       = true;
  };

  # settings.local.json is deliberately NOT nix-managed. It used to hold the
  # kube-direct-push autoMode rule as a store symlink in all four config dirs,
  # but the classifier unions autoMode.allow across the files it reads, so the
  # rule works identically from claudeBaseSettings above — and the per-profile
  # copies were dead weight (classifier.py's SETTINGS_PATHS only reads ~/.claude/).
  # Leaving these unmanaged keeps them writable for Claude Code's own user-scope
  # "don't ask again" saves, which a read-only store symlink silently broke.

  # Per-profile setup. Runs after writeBoundary but before linkGeneration so the
  # rm step forces linkGeneration to create the store symlinks (it skips
  # identical regular files via cmp -s, leaving the runtime-copied file). Also
  # creates runtime symlinks for shared user-scope resources (CLAUDE.md,
  # commands/, projects/) — settings.json is nix-owned via home.file above so
  # untouched here, and settings.local.json is left unmanaged entirely.
  home.activation.claudeProfiles = lib.hm.dag.entryBefore [ "linkGeneration" ] ''
    for name in ${lib.concatStringsSep " " (lib.attrNames claudeProfiles)}; do
      # Remove pre-existing runtime-copied settings.json so linkGeneration
      # always creates the store symlink (otherwise an identical regular file
      # survives cmp -s). Force = true on the home.file entries handles the
      # pre-collision check; this handles the cmp -s skip.
      rm -f "$HOME/.config/claude-$name/settings.json"

      # Per-profile shared-resource symlinks (idempotent). CLAUDE.md,
      # commands/, and projects/ aren't in the store (CLAUDE.md is a single
      # nix-managed file, the others are runtime dirs Claude writes to) so
      # they get lazy-created here.
      dir="$HOME/.config/claude-$name"
      mkdir -p "$dir"
      [ -e "$HOME/.claude/CLAUDE.md" ] && [ ! -e "$dir/CLAUDE.md" ] && ln -sf "$HOME/.claude/CLAUDE.md" "$dir/CLAUDE.md"
      [ -d "$HOME/.claude/commands" ] && [ ! -e "$dir/commands" ] && ln -sf "$HOME/.claude/commands" "$dir/commands"
      [ -d "$HOME/.claude/projects" ] && [ ! -e "$dir/projects" ] && ln -sf "$HOME/.claude/projects" "$dir/projects"
      # hooks/ holds the herdr integration script (see home.file above). The
      # SessionStart command uses an absolute ~/.claude path so the hook fires
      # regardless, but `herdr integration status` probes CLAUDE_CONFIG_DIR
      # (a profile dir under claude-prof) for <dir>/hooks/herdr-agent-state.sh
      # — symlink it in so status reads "current" per profile too.
      [ -d "$HOME/.claude/hooks" ] && [ ! -e "$dir/hooks" ] && ln -sf "$HOME/.claude/hooks" "$dir/hooks"
      # skills/ holds the personal (non-plugin) skills rendered by home.file
      # above. Claude resolves user skills only under CLAUDE_CONFIG_DIR, so
      # without this link every profile sees an empty skill set and ~/.claude
      # is the sole profile that can run them.
      [ -d "$HOME/.claude/skills" ] && [ ! -e "$dir/skills" ] && ln -sf "$HOME/.claude/skills" "$dir/skills"
    done
  '';

  # Firefox 67+ keys profile-per-install via [Install<HASH>] sections in
  # profiles.ini (gated by `Version=2`), overriding `Default=1`. Every nix
  # firefox bump makes a new install hash, so Firefox creates a fresh
  # *.default-release profile and pins it, ignoring the home-manager one.
  # Dropping Version= makes Firefox honor Default=1 (like Darwin); rm the legacy
  # installs.ini backup so it can't re-seed the Install section on next launch.
  home.activation.firefoxInstallsIni = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD rm -f "$HOME/${config.programs.firefox.configPath}/installs.ini"
  '';

  xdg.userDirs.setSessionVariables = true;

  home.username = let v = builtins.getEnv "USER"; in if v != "" then v else "sauyon";
  home.homeDirectory = let v = builtins.getEnv "HOME"; in if v != "" then v else (if isDarwin then "/Users/sauyon" else "/home/sauyon");

  home.sessionVariables =
    import ./env.nix (
      args
      // {
        xdg = config.xdg;
        home = config.home.homeDirectory;
        inherit isDesktop;
      }
    )
    // (lib.optionalAttrs hidpi.enabled {
      QT_FONT_DPI = toString hidpi.qtFontDpi;
    })
    # setsuna scales GTK via dconf (text-scaling-factor); other HiDPI hosts have
    # no dconf D-Bus service, so use GDK_DPI_SCALE instead.
    // (lib.optionalAttrs (hidpi.enabled && hostname != "setsuna") {
      GDK_DPI_SCALE = toString hidpi.scale;
    });

  # TERMINFO_DIRS is already set under systemd by home-manager's generic-linux
  # module; exclude it here to avoid a conflicting definition.
  systemd.user.sessionVariables =
    lib.mkIf (!isDarwin) (removeAttrs config.home.sessionVariables [ "TERMINFO_DIRS" ]);

  # ── Emacs ──────────────────────────────────────────────────────────────────
  home.file.".emacs.d/init.el".source = ./home/emacs/init.el;
  home.file.".emacs.d/lisp/mode-init.el".source = ./home/emacs/lisp/mode-init.el;
  home.file.".emacs.d/lisp/pref-init.el".source = ./home/emacs/lisp/pref-init.el;
  home.file.".emacs.d/lisp/root-find.el".source = ./home/emacs/lisp/root-find.el;
  # grip-mode shells out to `grip`; pin the nix store path rather than rely on
  # PATH (the .el files aren't templated).
  home.file.".emacs.d/lisp/grip-path.el" = lib.mkIf (!isDarwin) {
    text = ''
      (setq grip-binary-path "${pkgs.python3Packages.grip}/bin/grip")
    '';
  };

  services.emacs = lib.mkIf (!isDarwin) {
    enable = true;
    package = withHostNss emacsPkg;
    client.enable = true;
  };

  # ── Scripts ────────────────────────────────────────────────────────────────
  # On darwin, .local/bin symlinks to the dotfiles repo; skip HM management.
  home.file.".local/bin/bootstrap.sh" = lib.mkIf (!isDarwin) { executable = true; source = ./home/scripts/bootstrap.sh; };
  home.file.".local/bin/mprisinfo" = lib.mkIf (!isDarwin) { executable = true; source = ./home/scripts/mprisinfo; };
  home.file.".local/bin/reyubikey" = lib.mkIf (!isDarwin) { executable = true; source = ./home/scripts/reyubikey; };
  home.file.".local/bin/upload" = lib.mkIf (!isDarwin) { executable = true; source = ./home/scripts/upload; };
  home.file.".local/bin/yank" = lib.mkIf (!isDarwin) { executable = true; source = ./home/scripts/yank; };

  # mosh-server wrapper that injects our fork's -T (COLORTERM=truecolor).
  #
  # -T exists only in sauyon/mosh, so no third-party client can pass it, and
  # scripts/mosh.pl only passes it when COLORTERM is already set client-side.
  # Blink builds its own mosh-server command line and reports `-c 256` even
  # though it renders semicolon truecolor fine, so those sessions lose 24-bit
  # colour that mosh delivers anyway.
  #
  # It has to be asserted here rather than defaulted in mosh-server, because
  # mosh does not adapt colour to the client: Renditions::sgr() emits
  # ";38;2;r;g;b" for any true-colour cell with no capability check, and Display
  # tracks no colour capability at all. So mosh forwards 24-bit verbatim to
  # whatever the far end is, and only the operator knows whether that terminal
  # can render it. Point Blink's Hosts > Mosh > Server at this path to say yes.
  #
  # -T is inserted immediately after the `new` verb: mosh-server runs getopt on
  # argv+1, so flags must follow `new`, and appending at the end would land
  # after any trailing `-- command`.
  home.file.".local/bin/mosh-server-tc" = lib.mkIf (!isDarwin) {
    executable = true;
    text = ''
      #!${pkgs.runtimeShell}
      set -eu
      real=${pkgs.mosh}/bin/mosh-server
      if [ "''${1-}" = "new" ]; then
        shift
        exec "$real" new -T "$@"
      fi
      exec "$real" "$@"
    '';
  };

  # ── Pulse ──────────────────────────────────────────────────────────────────
  xdg.configFile."pulse/client.conf" = lib.mkIf (!isDarwin) { text = "cookie-file = /.cache/pulse/cookie\n"; };

  # ── WirePlumber ────────────────────────────────────────────────────────────
  # Disable the AB13X USB headset adapter on fujiwara — unused, but keeps
  # grabbing default-sink when plugged in.
  xdg.configFile."wireplumber/wireplumber.conf.d/51-disable-ab13x.conf" = lib.mkIf (hostname == "fujiwara") {
    text = ''
      monitor.alsa.rules = [
        {
          matches = [
            { device.name = "alsa_card.usb-Generic_USB_Audio_20210726905926-00" }
          ]
          actions = {
            update-props = {
              device.disabled = true
            }
          }
        }
      ]
    '';
  };

  # ── psi-notify ─────────────────────────────────────────────────────────────
  #
  # There are deliberately NO io thresholds here. On these hosts io PSI does not
  # measure disk trouble at all:
  #
  #   - ghostty's libxev event loop parks one thread per io_uring ring in
  #     io_cqring_wait(), which sleeps via io_schedule() and therefore sets
  #     current->in_iowait. The kernel counts that as blocked-on-IO: it lands in
  #     /proc/stat procs_blocked and is flagged TSK_IOWAIT for PSI -- even though
  #     the ring holds only idle IORING_OP_POLL_ADD fd watches (SQEs 0, CQEs 0)
  #     and no disk IO is happening.
  #   - Every other thread in ghostty's cgroup is idle-sleeping, so PSI's "full"
  #     definition (all non-idle tasks stalled) reads ~100% for that scope, and
  #     it sums up the cgroup chain into user.slice and /proc/pressure/io.
  #
  # Measured 2026-08-28 on fujiwara: session-100.scope sat at full avg300=99.88
  # for the whole 88-day uptime while every disk had inflight=0 and an O_DIRECT
  # probe ran at 895 MB/s. psi-notify flapped "I/O alert: active" all night and
  # went inactive the moment ghostty died. Same shape on utsuho (kernel 7.1.5):
  # 4 io_uring rings, 4 threads in io_cqring_wait, procs_blocked exactly 4,
  # while Slack (9 procs) and Firefox (15 procs) both read 0.00 because they use
  # epoll rather than io_uring.
  #
  # The real fix is upstream: io_uring_enter takes IORING_ENTER_NO_IOWAIT (1<<7),
  # probed via IORING_FEAT_NO_IOWAIT (1<<17), both present in our kernel headers.
  # Until libxev passes it, any io threshold here can only ever fire false.
  # memory PSI is unaffected by this and stays.
  xdg.configFile."psi-notify" = lib.mkIf (!isDarwin && isDesktop) {
    text = ''
      update 5
      log_pressures false

      threshold memory some avg10 15.00
      threshold memory full avg10 5.00
    '';
  };

  # ── p10k ───────────────────────────────────────────────────────────────────
  xdg.configFile."zsh/.p10k.zsh".source = ./home/p10k.zsh;

  # ── Warp ───────────────────────────────────────────────────────────────────
  xdg.configFile."warp-terminal/keybindings.yaml" = lib.mkIf (!isDarwin && isDesktop) {
    source = ./home/warp/keybindings.yaml;
  };
  xdg.configFile."warp-terminal/user_preferences.json" = lib.mkIf (!isDarwin && isDesktop) {
    source = ./home/warp/user_preferences.json;
  };
  home.file.".local/share/warp-terminal/tab_configs/startup_config.toml" = lib.mkIf (!isDarwin && isDesktop) {
    source = ./home/warp/tab_configs/startup_config.toml;
  };

  # Override the packaged Zoom.desktop so the app launcher (elephant/walker) and
  # zoommtg: scheme handlers use the wayland `zoom` wrapper instead of
  # /usr/bin/zoom (which force-sets QT_QPA_PLATFORM=xcb and crashes — see the
  # wrapper comment above). MUST live in XDG_DATA_HOME (~/.local/share): elephant
  # orders ~/.nix-profile/share (where hm's xdg.desktopEntries lands) *below*
  # /usr/share so an entry there loses to the pacman one, but ~/.local/share wins
  # over everything. Mirrors /usr/share/applications/Zoom.desktop.
  xdg.dataFile."applications/Zoom.desktop" = lib.mkIf (!isDarwin && isDesktop) {
    text = ''
      [Desktop Entry]
      Name=Zoom Workplace
      GenericName=Zoom Workplace
      Exec=${zoom}/bin/zoom %U
      Icon=Zoom
      Terminal=false
      Type=Application
      StartupNotify=true
      StartupWMClass=zoom
      MimeType=x-scheme-handler/zoommtg;x-scheme-handler/zoomus;x-scheme-handler/tel;x-scheme-handler/callto;x-scheme-handler/zoomphonecall;x-scheme-handler/zoomphonesms;x-scheme-handler/zoomcontactcentercall;application/x-zoom;
    '';
  };

  # ── Herdr ───────────────────────────────────────────────────────────────────
  xdg.configFile."herdr/config.toml".source = ./home/herdr/config.toml;

  # ── drovr ───────────────────────────────────────────────────────────────────
  # Review panel runs on opencode, at the model pinned in opencode.nix.
  # serve_host: the review server has NO auth, so anything it is bound to can read
  # and act on every run. fujiwara binds its LAN address so the page is reachable
  # from any machine on the house network without a tailnet session; that is a
  # wider audience than the tailnet-only posture this used to have -- every device
  # on VLAN1, guests included. utsuho stays on its tailnet address.
  # worktree: every run gets .drovr/wt/<run> on its own branch, so a run in
  # flight leaves the invoking checkout free. The default was off, and the cost
  # showed up as an agent editing main while the tree moved under it: reads went
  # stale, HEAD advanced past the commit under review, and a test count was
  # reported from a tree that no longer existed. `--no-worktree` per run.
  xdg.configFile."drovr/config.toml".text = ''
    review_agent = "opencode"
    worktree = true
  '' + lib.optionalString (hostname == "fujiwara") ''
    serve_host = "10.0.7.100"
  '' + lib.optionalString (hostname == "utsuho") ''
    serve_host = "100.71.58.39"
  '';

  # ── forgejo-cli (fj) ────────────────────────────────────────────────────────
  # Only the client-id table is declarative — it is a public-client PKCE ID, not
  # a credential. The tokens `fj auth login` mints stay unmanaged in
  # $XDG_DATA_HOME/forgejo-cli/keys.json; see git-credential-fj above.
  xdg.configFile."forgejo-cli/client_ids".source = ./home/forgejo-cli/client_ids;

  home.file.".local/bin/hyprland-graceful-exit" = lib.mkIf (!isDarwin && isDesktop) {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      # Gracefully close all Hyprland windows, then optionally exit Hyprland.
      set -euo pipefail

      hyprctl clients -j | ${pkgs.jq}/bin/jq -r '.[].address' | while read -r addr; do
        hyprctl dispatch closewindow "address:$addr" || true
      done

      # Wait for windows to close (up to 5s)
      for i in $(seq 1 10); do
        count=$(hyprctl clients -j | ${pkgs.jq}/bin/jq 'length')
        [ "$count" -eq 0 ] && break
        sleep 0.5
      done

      if [ "''${1:-}" != "--no-exit" ]; then
        hyprctl dispatch exit
      fi
    '';
  };


  systemd.user.services.psi-notify = lib.mkIf (!isDarwin && isDesktop) {
    Unit = {
      Description = "Desktop notifications when system resources are under pressure";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };
    Service = {
      Type = "notify";
      ExecStart = "${withHostNss pkgs.psi-notify}/bin/psi-notify";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      Restart = "on-failure";
      RestartSec = 5;
      WatchdogSec = "2s";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  # Replaces home-manager's services.gnome-keyring (see the NOTE where that
  # module would have been configured). Keeps the same unit name and target so
  # ordering against graphical-session-pre.target is unchanged; the only
  # difference is that ExecStart unseals the login passphrase from the TPM.
  #
  # This unit claims the org.freedesktop.secrets bus name at
  # graphical-session-pre.target, before any app asks for it. Two other things on
  # this host can start a *locked* gnome-keyring-daemon, and both are shut off so
  # they cannot serve secrets instead:
  #   - Arch's gnome-keyring-daemon.{socket,service}, masked in system/deploy.
  #   - D-Bus activation, redirected to the TPM wrapper just below. Masking does
  #     nothing about this path, which is why it needs handling of its own.
  #
  # If unlock prompts ever come back, check `busctl --user status
  # org.freedesktop.secrets` -- if it names anything other than this unit,
  # something claimed the name earlier and that is the thing to chase.
  systemd.user.services.gnome-keyring = lib.mkIf gnomeKeyringHost {
    Unit = {
      Description = "GNOME Keyring (login collection unlocked from the TPM)";
      PartOf = [ "graphical-session-pre.target" ];
    };
    Service = {
      ExecStart = "${gnome-keyring-tpm}/bin/gnome-keyring-tpm";
      Restart = "on-abort";
    };
    Install.WantedBy = [ "graphical-session-pre.target" ];
  };

  # All three of gnome-keyring's D-Bus activation files in /usr/share ship
  # `Exec=gnome-keyring-daemon --start --components=secrets`, i.e. a daemon with
  # the login collection still locked. XDG_DATA_HOME is searched ahead of
  # /usr/share, so shadow each one to launch the TPM wrapper instead. Activation
  # only fires when the bus name is unowned, so this never races the systemd unit
  # above -- it is purely the on-demand fallback, and now it unlocks too.
  home.file.".local/share/dbus-1/services/org.freedesktop.secrets.service" =
    lib.mkIf gnomeKeyringHost { text = gnomeKeyringDbusService "org.freedesktop.secrets"; };
  home.file.".local/share/dbus-1/services/org.gnome.keyring.service" =
    lib.mkIf gnomeKeyringHost { text = gnomeKeyringDbusService "org.gnome.keyring"; };
  home.file.".local/share/dbus-1/services/org.freedesktop.impl.portal.Secret.service" =
    lib.mkIf gnomeKeyringHost { text = gnomeKeyringDbusService "org.freedesktop.impl.portal.Secret"; };

  systemd.user.services.hyprland-cleanup = lib.mkIf (!isDarwin && isDesktop) {
    Unit = {
      Description = "Gracefully close all Hyprland windows on session end";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
      # The ExecStop below closes every Hyprland window and fires whenever this
      # unit stops for *any* reason. When a home-manager switch changes a store
      # path in this unit, sd-switch would otherwise restart it and run ExecStop
      # mid-switch, closing all windows (see the 2026-07 firefox incident).
      # keep-old tells sd-switch to leave the running unit untouched during a
      # switch. Real logout still stops graphical-session.target, which stops
      # this unit via PartOf and runs ExecStop as intended.
      X-SwitchMethod = "keep-old";
    };
    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.coreutils}/bin/true";
      ExecStop = "${config.home.homeDirectory}/.local/bin/hyprland-graceful-exit --no-exit";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  # opencode leaks (upstream #16697, unfixed); this records how fast, so a
  # restart is an informed manual call. Never signals anything.
  # total_kb sums RSS, which double-counts shared pages: a growth curve, not a
  # usage figure.
  systemd.user.services.opencode-memwatch = lib.mkIf (!isDarwin) {
    Unit.Description = "Record opencode resident memory (observational; never signals)";
    Service = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "opencode-memwatch" ''
        set -eu
        PATH=${pkgs.coreutils}/bin:${pkgs.procps}/bin:${pkgs.gawk}/bin
        STATE="$HOME/.local/state/opencode-memwatch"
        LOG="$STATE/rss.log"
        mkdir -p "$STATE"
        # Match argv, not comm: the kernel truncates comm to `.opencode-wrapp`.
        ps -eo pid=,rss=,args= | awk -v ts="$(date -Is)" '
          $3 ~ /opencode/ && $0 !~ /memwatch/ {
            n++; total += $2;
            if ($2 > max) max = $2;
            procs = procs sprintf(" %s:%s", $1, $2);
          }
          END { printf "%s\tprocs=%d\ttotal_kb=%d\tmax_kb=%d\tpids=%s\n", \
                       ts, n+0, total+0, max+0, procs }
        ' >> "$LOG"
        # Keep the newest 10k samples (~5 weeks at this interval).
        if [ "$(wc -l < "$LOG")" -gt 10000 ]; then
          tail -n 10000 "$LOG" > "$LOG.tmp" && mv "$LOG.tmp" "$LOG"
        fi
      '';
    };
  };

  systemd.user.timers.opencode-memwatch = lib.mkIf (!isDarwin) {
    Unit.Description = "Sample opencode resident memory every 5 minutes";
    Timer = {
      OnBootSec = "5min";
      OnUnitActiveSec = "5min";
      AccuracySec = "1min";
    };
    Install.WantedBy = [ "timers.target" ];
  };

  # `clp rc` == `claude-prof run personal remote-control`: a persistent server
  # letting claude.ai/code and the Claude mobile app drive local sessions in a
  # project. Template unit keyed on the project path so any number can run
  # concurrently and start on the fly (see clp-rc/clp-rc-stop in zsh.nix):
  #   systemctl --user start claude-remote-control@$(systemd-escape -p /path/to/proj)
  # %I unescapes back to the absolute project path for WorkingDirectory. Verified
  # headless: claude bundles its own node, connects with stdin=null and no TTY,
  # and shuts down gracefully on SIGTERM. RC refuses to start in an untrusted
  # workspace, so clp-rc pre-accepts the trust dialog in the personal profile.
  systemd.user.services."claude-remote-control@" = lib.mkIf (!isDarwin) {
    Unit = {
      Description = "Claude Code Remote Control (personal profile) — %I";
      After = [ "network-online.target" ];
      Wants = [ "network-online.target" ];
    };
    Service = {
      Type = "simple";
      # `systemd-escape -p /abs/path` drops the leading slash, so %I unescapes to
      # a relative path (home/sauyon/…); prefix `/` to restore the absolute one.
      WorkingDirectory = "/%I";
      Environment = "PATH=${config.home.profileDirectory}/bin:/usr/bin:/bin";
      StandardInput = "null";
      # --spawn worktree: on-demand sessions each get their own git worktree (the
      # pre-created cwd session stays in the project dir). Needs a git repo.
      ExecStart = "${claude-prof}/bin/claude-prof run personal remote-control --spawn worktree";
      Restart = "on-failure";
      RestartSec = 10;
    };
    # No [Install]/WantedBy: a template can't be started bare (HM would try and
    # fail). Instances start on the fly with `clp-rc [dir]`. To autostart a
    # project at boot, add a wants symlink for that instance, e.g.
    #   xdg.configFile."systemd/user/default.target.wants/claude-remote-control@<esc>.service".
  };

  # `ca-rc` starts a Cursor Agent pool worker for a project dir so cloud/mobile
  # sessions can claim it one agent at a time. Template keyed on project path:
  #   systemctl --user start cursor-agent-worker@$(systemd-escape -p /path/to/proj)
  systemd.user.services."cursor-agent-worker@" = lib.mkIf (!isDarwin) {
    Unit = {
      Description = "Cursor Agent worker — %I";
      After = [ "network-online.target" ];
      Wants = [ "network-online.target" ];
    };
    Service = {
      Type = "simple";
      WorkingDirectory = "/%I";
      Environment = "PATH=${config.home.profileDirectory}/bin:/usr/bin:/bin";
      StandardInput = "null";
      ExecStart = "${pkgs.cursor-agent-cli}/bin/agent worker --pool start";
      Restart = "on-failure";
      RestartSec = 10;
    };
  };

  home.packages = [
    claude-prof
    herdr-pkg
  ]
  # Enrolment/recovery tool for the TPM-sealed keyring passphrase; the daemon
  # wrapper itself is referenced straight from its unit, so it stays off PATH.
  ++ lib.optional gnomeKeyringHost gnome-keyring-tpm-seal
  ++ (with pkgs; [
    bfs
    btopPkg
    google-fonts
    claude-agent-acp
    coder
    comma
    cosign
    entire  # git-hook layer checkpointing AI agent sessions alongside commits
    jq
    jujutsu
    lnav
    mosh
    opencode
    pi-coding-agent  # earendil-works/pi terminal coding agent (binary: pi)
    forgejo-cli  # Forgejo-native CLI (binary: fj) for Codeberg and forge.ko.ag
    bat
    rustup
    nixfmt
    kubectl
    kubelogin-oidc
    kube-capacity
    kubectx
    tmux
    unzip
    zip
    (emacsPackages.treesit-grammars.with-grammars (grammars: with grammars; [
      tree-sitter-tsx
      tree-sitter-typescript
    ]))
    hunk-pkg
    explore-mcp-pkg
    drovr-pkg
  ]) ++ lib.optionals (!isDarwin) [
    # What grip-mode shells out to; top-level `grip` is an unrelated CD ripper.
    pkgs.python3Packages.grip
    pkgs.cursor-agent-cli
    pkgs.cloudflare-warp
    pkgs.cryptomator-cli
  ] ++ lib.optionals (!isDesktop) [
    pkgs.ghostty.terminfo
  ] ++ lib.optionals (!isDarwin) [
    # Headless hosts get the -nox build; see emacsPkg. Deliberately outside the
    # isDesktop block: $EDITOR, git core.editor and edit/sedit all resolve
    # emacsclient, so gating this on the desktop stack breaks editing over SSH.
    #
    # withHostNss, not bare: an unwrapped nix emacs can't dlopen the host's
    # libnss_systemd.so.2, so getpwuid/getpwnam fail for a homed-only user with
    # no /etc/passwd entry. Emacs then sets init-file-user to "sauyon" instead
    # of "", can't resolve ~sauyon, and every startup warns "User sauyon has no
    # home directory" (startup.el's file-directory-p check). services.emacs
    # already wraps the daemon; the CLI on PATH needs it too.
    (withHostNss emacsPkg)
  ] ++ lib.optionals (!isDarwin && isDesktop) [
    caffeine
    # nixGL wrap: without it the FHS env resolves GBM/DRI via the NixOS-only
    # /run/opengl-driver path and falls back to software rendering.
    (config.lib.nixGL.wrap cumora)
    hypr-fullscreen-inhibit
    hypr-unstuck-lock
    nixGL

    pkgs.bitwarden-cli
    # Desktop app is the biometric backend the Firefox extension talks to over
    # native messaging (the extension can't unlock with biometrics on its own).
    # Pairs with the polkit action + pam_fprintd wiring in system/.
    pkgs.bitwarden-desktop
    pkgs.hyprpicker
    pkgs.psi-notify
    pkgs.pwvucontrol
    pkgs.slack
    zoom # wayland wrapper bypassing Zoom's xcb-forcing launcher; see above
    # Dropped: vesktop's build pulls pnpm-10.29.2, marked insecure in nixpkgs
    # (CVE-2026-48995, CVE-2026-50014). Re-add once nixpkgs ships a patched pnpm.
    # pkgs.vesktop
    (config.lib.nixGL.wrap (withHostNss pkgs.warp-terminal))
    pkgs.xauth
    pkgs.xdg-utils
  ] ++ lib.optionals (hostname == "fujiwara") [
    clawpatrol
  ];

  nixpkgs.config = {
    allowUnfree = true;
    sandbox = true;
    # bitwarden-desktop 2026.6.1 pins electron 39.8.10, flagged insecure only
    # because that Electron branch is EOL (no active CVE, unlike vesktop/pnpm
    # above). Scoped to the exact version so a future bitwarden-desktop bump onto
    # a newer Electron re-raises the flag for review.
    permittedInsecurePackages = [ "electron-39.8.10" ];
  };

  nixpkgs.overlays = [
    (final: prev: {
      nur = import (builtins.fetchTarball {
        url = "https://github.com/nix-community/NUR/archive/4b22de075887985d445668c4634ae148618c6a41.tar.gz";
        sha256 = "1fkb8bv1qfls4gvvim91pgxms6vidm093ycc3vwnacygjgbv5hqh";
      }) {
        nurpkgs = prev;
        pkgs = prev;
      };
    })
    (final: prev: {
      # hyprlock links Nix's libpam, whose pam_unix.so hardcodes the unix_chkpwd
      # helper path to /run/wrappers/bin/unix_chkpwd (a NixOS-ism — see
      # linux-pam/package.nix). On this Arch host nothing creates that path and
      # /run is tmpfs, so after every reboot password auth silently fails
      # (fingerprint still works, masking it) until the symlink is recreated by
      # hand. Build hyprlock against a pam pointing pam_unix at Arch's own setuid
      # helper instead, so password auth survives reboots with no /run/wrappers shim.
      hyprlock = (prev.hyprlock.override {
        pam = prev.pam.overrideAttrs (old: {
          postPatch = (old.postPatch or "") + ''
            substituteInPlace modules/module-meson.build \
              --replace-fail "'/run/wrappers/bin/unix_chkpwd'" "'/usr/bin/unix_chkpwd'"
          '';
        });
      }).overrideAttrs (old: {
        patches = (old.patches or []) ++ [
          ./patches/hyprlock-skip-dtors-on-early-fail.patch
        ];
      });
    })
    (final: prev: {
      # Warp dlopens libwayland-client.so.0 via winit, but the nixpkgs build
      # omits wayland from RUNPATH — autoPatchelfHook only sees linked deps, not
      # dlopen'd ones. Without this, warp falls back to X11 even with
      # WARP_ENABLE_WAYLAND=1.
      warp-terminal = prev.warp-terminal.overrideAttrs (old: {
        runtimeDependencies = (old.runtimeDependencies or []) ++ [
          final.wayland
        ];
      });
    })
    (final: prev: {
      # kubelogin blocks silently on ~/.kube/cache/oidc-login/*.lock while another
      # kubectl completes Dex login (token-cache flock since v1.30). Upstream knows
      # the UX gap for the older port lock (#851, open) but not this path. Patch
      # prints one stderr line before waiting.
      kubelogin-oidc = prev.kubelogin-oidc.overrideAttrs (old: {
        patches = (old.patches or []) ++ [
          ./patches/kubelogin-waiting-on-token-cache-lock.patch
        ];
      });
    })
    (final: prev: {
      mosh = prev.mosh.overrideAttrs (old: {
        version = "1.4.0-blink-master";
        src = prev.fetchFromGitHub {
          owner = "sauyon";
          repo = "mosh";
          rev = "91b48f1061072e910cdb8ecd672988628cfa05ed";
          sha256 = "00f1v6xm53gr0hfsnmdhgqbdnfkdbd0sv6sdkhqrln3acrcsrwzh";
        };
        # nixpkgs cherry-picks an upstream macOS compile fix already in our base
        # — drop it to avoid "patch already applied".
        patches = builtins.filter
          (p: !(prev.lib.hasInfix "eee1a8cf" (toString p)))
          old.patches;
      });
    })
    (final: prev: {
      # Pin coder to match the RDE server (rde.modular.com runs v2.31.10);
      # nixpkgs ships the older stable 2.28.6 and the CLI warns on every
      # invocation about the client/server mismatch. The nixpkgs derivation just
      # fetches a prebuilt release tarball, so bumping is a version + per-system
      # hash swap (no Go/frontend rebuild).
      coder = prev.coder.overrideAttrs (old: rec {
        version = "2.31.10";
        # Drop the terraform PATH wrapper: terraform is unfree (never cached) and
        # only wraps coder to run provisioners locally, which the client never does.
        postInstall = "";
        src = prev.fetchurl {
          url =
            let
              systemName = {
                x86_64-linux = "linux_amd64";
                aarch64-linux = "linux_arm64";
                x86_64-darwin = "darwin_amd64";
                aarch64-darwin = "darwin_arm64";
              }.${prev.stdenvNoCC.hostPlatform.system};
              ext = if prev.stdenvNoCC.hostPlatform.isDarwin then "zip" else "tar.gz";
            in
            "https://github.com/coder/coder/releases/download/v${version}/coder_${version}_${systemName}.${ext}";
          hash = {
            x86_64-linux = "sha256-9ZhLKf0lNIX391BqzsqltiuMwDVJ8J7daRNowrkW4fE=";
            aarch64-linux = "sha256-DcfCWUcyru3tAbNhaL5qT4okV6eu5/IJS+YhPwBAMqs=";
            x86_64-darwin = "sha256-Pdd7mgWTexr2eWDMIixe//eFihUyYQszBFPScIaCciI=";
            aarch64-darwin = "sha256-qYFLcyTXjgWMPjmsThxDQngklT1x36MEkCTtMzn6E6k=";
          }.${prev.stdenvNoCC.hostPlatform.system};
        };
      });
    })
    (final: prev: {
      claude-agent-acp = prev.buildNpmPackage rec {
        pname = "claude-agent-acp";
        version = "0.33.1";
        src = prev.fetchFromGitHub {
          owner = "agentclientprotocol";
          repo = "claude-agent-acp";
          rev = "v${version}";
          hash = "sha256-FwcIJf/tfH6prDFKtOo7X1mTocibf4Ne6JHOS9ITG8U=";
        };
        npmDepsHash = "sha256-y795LyNjSJjTpIqtA5bC/AgeFLghM0yU5xQRD3m+Ajs=";
        dontNpmPrune = true;
      };
    })
  ];

  home.pointerCursor = lib.mkIf (!isDarwin && isDesktop) {
    enable = true;
    package = pkgs.yaru-theme;
    name = "Yaru";
    size = hidpi.cursorSize;
    gtk.enable = true;
  };

  gtk = lib.optionalAttrs (!isDarwin && isDesktop) {
    enable = true;
    colorScheme = "dark";
    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
    gtk3.extraConfig.gtk-key-theme-name = "Emacs";
    gtk3.extraCss = ''
      @binding-set mac-bindings {
        bind "<Super>x" { "cut-clipboard" () };
        bind "<Super>c" { "copy-clipboard" () };
        bind "<Super>v" { "paste-clipboard" () };
        bind "<Super>a" { "select-all" (true) };
        bind "<Super>z" { "undo" () };
        bind "<Super><Shift>z" { "redo" () };
      }
      * { -gtk-key-bindings: mac-bindings; }
    '';
    gtk4.extraConfig.gtk-key-theme-name = "Emacs";
    gtk4.extraCss = ''
      @binding-set mac-bindings {
        bind "<Super>x" { "cut-clipboard" () };
        bind "<Super>c" { "copy-clipboard" () };
        bind "<Super>v" { "paste-clipboard" () };
        bind "<Super>a" { "select-all" (true) };
        bind "<Super>z" { "undo" () };
        bind "<Super><Shift>z" { "redo" () };
      }
      * { -gtk-key-bindings: mac-bindings; }
    '';
    theme = {
      name = "Plano";
      package = pkgs.plano-theme;
    };
    gtk4.theme = config.gtk.theme;
    iconTheme = {
      name = "Yaru-dark";
      package = pkgs.yaru-theme;
    };
    font = {
      name = "NotoSans Nerd Font";
      package = pkgs.nerd-fonts.noto;
    };
  };

  qt = lib.optionalAttrs (!isDarwin && isDesktop) {
    enable = true;
    platformTheme.name = "gtk2";
  };

  programs.walker = lib.optionalAttrs (!isDarwin && isDesktop) {
    enable = true;
    runAsService = true;
  };

  services = {
    hyprpaper = {
      enable = !isDarwin && isDesktop;
      package = config.lib.nixGL.wrap pkgs.hyprpaper;
      settings = {
        path = "${config.home.homeDirectory}/images/wallpapers/${hostname}.png";
      };
    };

    kanshi = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      settings = [
        {
          output = {
            criteria = "BOE NE160QDM-NZ6 Unknown";
            mode = "2560x1600";
            position = "0,0";
            scale = 2.0;
            transform = "normal";
            alias = "UTSUHO";
          };
        }
        {
          output = {
            criteria = "BOE 0x095F Unknown";
            mode = "2256x1504";
            position = "0,0";
            scale = 1.0;
            transform = "normal";
            alias = "SETSUNA";
          };
        }
        {
          profile = {
            name = "setsuna";
            outputs = [
              { criteria = "$SETSUNA"; status = "enable"; scale = 1.0; }
            ];
          };
        }
        {
          profile = {
            name = "utsuho";
            outputs = [
              { criteria = "$UTSUHO"; status = "enable"; scale = 1.0; }
            ];
          };
        }
        {
          profile = {
            name = "home";
            outputs = [
              { criteria = "GIGA-BYTE TECHNOLOGY CO., LTD. AORUS FO48U 21170B001458"; mode = "3840x2160"; position = "0,0"; scale = 2.0; }
              { criteria = "eDP-1"; status = "disable"; }
            ];
          };
        }
        {
          profile = {
            name = "Modular";
            outputs = [
              { criteria = "Dell Inc. DELL P3424WEB F2VTM04"; mode = "3440x1440"; position = "-528,-1440"; transform = "normal"; scale = 1.0; }
              { criteria = "$UTSUHO"; status = "enable"; scale = 1.0; }
            ];
          };
        }
        {
          profile = {
            name = "fujiwara";
            outputs = [
              { criteria = "Samsung Electric Company S90F 0x01000E00"; mode = "3840x2160"; position = "0,0"; scale = 1.0; }
            ];
          };
        }
      ];
    };

    gpg-agent = lib.optionalAttrs (!isDarwin) {
      enable = true;
      # SSH support handled by ssh-tpm-agent (below), which falls back here for
      # non-TPM keys via the fallback socket arg.
      enableSshSupport = false;
      defaultCacheTtl = 600;
      maxCacheTtl = 1200;
      pinentry.package = if isDesktop then pkgs.pinentry-gnome3 else pkgs.pinentry-curses;
    };

    ssh-tpm-agent = lib.optionalAttrs (!isDarwin) {
      enable = true;
    };

    # fujiwara is driven headlessly over tty/SSH, where the graphical login
    # keyring is never unlocked and gnome-keyring has no prompter to CREATE the
    # `login` collection — so its Secret Service is unusable (libsecret clients
    # like woodpecker-cli block on the missing collection). fujiwara uses
    # pass-secret-service (below) instead; other desktops keep gnome-keyring.
    # NOTE: services.gnome-keyring is deliberately NOT used. Its unit runs
    # `gnome-keyring-daemon --start` with no stdin, and the login collection can
    # only be unlocked at daemon startup -- so there is nowhere for the module to
    # put the passphrase. The replacement unit is systemd.user.services
    # .gnome-keyring below, whose ExecStart is the gnome-keyring-tpm wrapper.

    # Headless-friendly Secret Service for fujiwara: backs libsecret onto a
    # GPG-encrypted `pass` store (~/.password-store, key in ~/.gnupg), so it
    # works in any tty/SSH session with no graphical unlock. Mutually exclusive
    # with gnome-keyring (module assertion). The GPG key is passphraseless, so the
    # store is protected by file perms + FDE only.
    # Gated on !isDesktop, the same axis as gnomeKeyringHost, so the two are
    # exhaustive: every Linux host gets exactly one Secret Service and neither
    # "both" nor "neither" is representable. Keying this on hostname instead let
    # a future gui = false host land with no provider at all.
    pass-secret-service = lib.optionalAttrs (!isDarwin && !isDesktop) {
      enable = true;
    };

    hypridle = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      settings = {
        general = {
          lock_cmd = "pidof hyprlock || ${config.programs.hyprlock.package}/bin/hyprlock";
          before_sleep_cmd = "loginctl lock-session";
          after_sleep_cmd = "${hyprDpmsPhysical} on";
        };
        listener = [
          {
            timeout = 300;
            on-timeout = "${hyprDpmsPhysical} off";
            on-resume = "${hyprDpmsPhysical} on";
          }
          {
            timeout = 600;
            on-timeout = "loginctl lock-session";
          }
        ];
      };
    };

    mako = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      settings = {
        background-color = "#1a1b26e6";
        text-color = "#c0caf5";
        border-color = "#7aa2f7";
        border-size = 2;
        border-radius = 8;
        default-timeout = 5000;
        font = "NotoSans Nerd Font 11";
        padding = "10";
        margin = "8";
        max-visible = 5;
        anchor = "top-right";
        "urgency=high" = {
          border-color = "#f7768e";
          default-timeout = 0;
        };
        "urgency=low" = {
          border-color = "#565f89";
        };
      };
    };
  };

  # services.gpg-agent generates its systemd unit from programs.gpg.package; wrap
  # so gpg-agent's getpwnam/getpwuid hits the host's libnss_systemd.
  programs.gpg.package = lib.mkIf (!isDarwin) (withHostNss pkgs.gnupg);

  targets.genericLinux.enable = !isDarwin;
  targets.genericLinux.nixGL.packages = lib.mkIf (!isDarwin && isDesktop) nixgl.packages.${system};

  wayland.windowManager.hyprland = lib.optionalAttrs (!isDarwin && isDesktop) (import ./hyprland.nix { inherit pkgs config edgeGap hyprDpmsPhysical; });

  dconf = {
    enable = hostname == "setsuna";
    settings = lib.optionalAttrs (hostname == "setsuna") {
      "org/gnome/desktop/interface" = {
        text-scaling-factor = hidpi.scale;
      };
    };
  };

  fonts.fontconfig.enable = true;


  programs = {
    hyprlock = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      # withHostNss: hyprlock runs under nix glibc, whose NSS can't load the host
      # libnss_systemd.so.2, so getpwuid fails for a systemd-userdb user (sauyon,
      # uid 60006, not in /etc/passwd). hyprlock then has no username to hand PAM
      # and silently rejects EVERY password (fingerprint uses a separate path,
      # masking it). Wrap NSS *outside* nixGL so the LD_LIBRARY_PATH prefix
      # propagates through to the real binary.
      package = withHostNss (config.lib.nixGL.wrap pkgs.hyprlock);
      settings = {
        general = {
          hide_cursor = true;
        };

        background = [
          {
            monitor = "";
            # path = "screenshot";   # disabled to debug deadlock
            blur_passes = 3;
            blur_size = 8;
          }
        ];

        auth = {
          pam.module = "login";
          fingerprint.enabled = true;
        };

        input-field = [
          {
            monitor = "";
            size = "300, 50";
            position = "0, -80";
            halign = "center";
            valign = "center";
            outline_thickness = 2;
            dots_size = 0.33;
            dots_spacing = 0.15;
            dots_center = true;
            outer_color = "rgb(151515)";
            inner_color = "rgb(200, 200, 200)";
            font_color = "rgb(10, 10, 10)";
            fade_on_empty = true;
            placeholder_text = "<i>Password...</i>";
            hide_input = false;
            check_color = "rgb(204, 136, 34)";
            fail_color = "rgb(204, 34, 34)";
            fail_text = "<i>$FAIL <b>($ATTEMPTS)</b></i>";
            capslock_color = "rgb(170, 0, 255)";
          }
        ];

        label = [
          {
            monitor = "";
            text = ''cmd[update:1000] echo "$(date +"%H:%M:%S")"'';
            font_size = 64;
            font_family = "NotoSans Nerd Font";
            position = "0, 80";
            halign = "center";
            valign = "center";
            color = "rgba(255, 255, 255, 0.9)";
          }
          {
            monitor = "";
            text = ''cmd[update:60000] echo "$(date +"%A, %B %-d")"'';
            font_size = 24;
            font_family = "NotoSans Nerd Font";
            position = "0, 10";
            halign = "center";
            valign = "center";
            color = "rgba(255, 255, 255, 0.7)";
          }
          {
            monitor = "";
            text = " $FPRINTPROMPT";
            font_size = 14;
            font_family = "NotoSans Nerd Font";
            position = "0, -140";
            halign = "center";
            valign = "center";
            color = "rgba(255, 255, 255, 0.7)";
          }
        ];
      };
    };
    waybar = let
      fontSize = hidpi.waybarFontSize;
      barHeight = hidpi.waybarBarHeight;
      shared = {
        layer = "top";
        position = "top";
        height = barHeight;
        spacing = 0;

        "hyprland/workspaces" = {
          format = "{id}";
          on-click = "activate";
          sort-by-number = true;
        };
        "hyprland/window" = {
          format = "{title}";
          max-length = 60;
          separate-outputs = true;
        };
        mpris = {
          format = "{player_icon} {dynamic}";
          format-paused = "{status_icon} {dynamic}";
          player-icons.default = "";
          status-icons.paused = "";
          dynamic-len = 40;
        };
        wireplumber = {
          format = "{icon} {volume}%";
          format-muted = "󰝟";
          format-icons = [ "" "" "" ];
          on-click = "${pkgs.pwvucontrol}/bin/pwvucontrol";
          scroll-step = 5;
        };
        network = {
          format-wifi = "  {essid}";
          format-ethernet = " {ifname}";
          format-disconnected = "󰖪 offline";
          tooltip-format = "{ifname}: {ipaddr}";
          max-length = 30;
          on-click = "ghostty -e nmtui";
        };
        bluetooth = {
          format = " {status}";
          format-disabled = "󰂲";
          format-connected = " {device_alias}";
          format-connected-battery = " {device_alias} {device_battery_percentage}%";
          tooltip-format = "{controller_alias}\n{num_connections} connected";
        };
        tray = {
          spacing = 8;
          icon-size = 18;
        };
        memory = {
          format = "󰍛 {percentage}%";
          interval = 2;
        };
        battery = {
          states = { warning = 30; critical = 15; };
          format = "{icon} {capacity}%";
          format-charging = "󰂄 {capacity}% (+{time})";
          format-discharging = "{icon} {capacity}% (-{time})";
          format-plugged = "󰚥 {capacity}%";
          format-full = "󰁹 {capacity}%";
          format-time = "{H}:{M:02}";
          format-icons = [ "" "" "" "" "" ];
        };
        clock = {
          format = "{:%a %m-%d %H:%M:%S}";
          interval = 1;
          tooltip-format = "<tt>{calendar}</tt>";
        };
        "custom/notifications" = {
          exec = ''makoctl mode | grep -qx do-not-disturb && echo '{"text":"󰂛","class":"dnd"}' || echo '{"text":"󰂚"}' '';
          return-type = "json";
          interval = 2;
          on-click = "makoctl dismiss --all";
          on-click-right = "makoctl mode -t do-not-disturb";
        };
        "custom/caffeine" = {
          exec = "${caffeine}/bin/caffeine waybar";
          return-type = "json";
          interval = 5;
          signal = 10;
          on-click = "${caffeine}/bin/caffeine toggle";
        };
      };
    in {
      enable = !isDarwin && isDesktop;
      systemd.enable = !isDarwin && isDesktop;
      # Released waybar (0.15.0, 2026-02) predates the fix for Hyprland's Lua IPC
      # dispatch protocol, so workspace clicks silently no-op under
      # `configType = "lua"`. Pin to the master commit with Alexays/waybar PR
      # #5013, which probes the socket and emits `hl.dsp.focus({ workspace })`.
      # Drop once a release > 0.15.0 ships the fix.
      # cavaSupport=false: master vendors a newer libcava than nixpkgs 0.15.0
      # pins, so the cava subproject can't resolve offline. We don't use cava, so
      # disable it rather than vendor the matching libcava.
      package = (pkgs.waybar.override { cavaSupport = false; }).overrideAttrs (old: {
        version = "0.15.0-unstable-2026-05-04";
        src = pkgs.fetchFromGitHub {
          owner = "Alexays";
          repo = "waybar";
          rev = "05945748dccce28bf96d26d8f64a9e69a8dd49ba";
          hash = "sha256-51R3mIt8cLNvh/X5qe9vOqeJCj0U9KRyemVE5y+OhiU=";
        };
        # master's binary still self-reports v0.15.0, so the nixpkgs
        # versionCheckHook (asserts --version matches `version`) fails.
        doInstallCheck = false;
      });
      settings = [
        (shared // {
          output = [ "eDP-1" ];
          modules-left = [ "hyprland/workspaces" "hyprland/window" ];
          modules-center = [ "mpris" ];
          modules-right = [ "wireplumber" "network" "battery" "tray" "memory" "custom/caffeine" "clock" "custom/notifications" ];
        })
        (shared // {
          output = [ "!eDP-1" "*" ];
          margin-top = edgeGap;
          margin-left = edgeGap;
          margin-right = edgeGap;
          modules-left = [ "hyprland/workspaces" "hyprland/window" ];
          modules-center = [ "mpris" ];
          modules-right = [ "wireplumber" "network" "bluetooth" "tray" "memory" "custom/caffeine" "clock" "custom/notifications" ];
        })
      ];
      style = ''
        @define-color bg          #1a1b26;
        @define-color bg-darker   #16161e;
        @define-color bg-lighter  #24283b;
        @define-color fg          #c0caf5;
        @define-color fg-dim      #a9b1d6;
        @define-color comment     #565f89;
        @define-color border      #292e42;
        @define-color red         #f7768e;
        @define-color orange      #ff9e64;
        @define-color yellow      #e0af68;
        @define-color green       #9ece6a;
        @define-color cyan        #7dcfc2;
        @define-color blue        #7aa2f7;
        @define-color purple      #bb9af7;

        * {
          font-family: "NotoSans Nerd Font", sans-serif;
          font-size: ${toString fontSize}px;
          border: none;
          border-radius: 0;
          min-height: 0;
        }

        window#waybar {
          background: alpha(@bg, 0.95);
          color: @fg;
          border-bottom: 1px solid @border;
        }

        #workspaces button {
          background: transparent;
          color: @fg-dim;
          padding: 0 12px;
          margin: 0;
          border-bottom: 6px solid transparent;
          transition: color 150ms, border-color 150ms;
        }
        #workspaces button:hover {
          background: alpha(#a695d0, 0.12);
          color: @fg;
          box-shadow: none;
        }
        #workspaces button.active {
          color: @fg;
          border-bottom: 6px solid #a695d0;
        }
        #workspaces button.urgent {
          color: @red;
          border-bottom: 6px solid @red;
        }

        #window { padding: 0 12px; color: @fg-dim; }
        window#waybar.empty #window { background: transparent; }

        #mpris { padding: 0 12px; color: @purple; }

        #wireplumber,
        #network,
        #bluetooth,
        #tray,
        #memory,
        #battery,
        #clock,
        #custom-caffeine,
        #custom-notifications { padding: 0 10px; }

        #custom-caffeine.off { color: @comment; }
        #custom-caffeine.on { color: @yellow; }

        #wireplumber { color: @cyan; }
        #wireplumber.muted { color: @comment; }
        #network { color: @green; }
        #network.disconnected { color: @red; }
        #bluetooth { color: @blue; }
        #bluetooth.disabled, #bluetooth.off { color: @comment; }
        #memory { color: @orange; }
        #battery { color: @green; }
        #battery.warning:not(.charging) { color: @yellow; }
        #battery.critical:not(.charging) { color: @red; }
        #battery.charging { color: @cyan; }
        #clock { color: @fg; font-weight: 600; }
        #custom-notifications { color: @yellow; }
        #custom-notifications.dnd { color: @comment; }

        tooltip {
          background: @bg-darker;
          border: 1px solid @border;
        }
        tooltip label { color: @fg; padding: 6px; }
      '';
    };

    thunderbird = lib.optionalAttrs (!isDarwin && isDesktop) {
      enable = true;
      profiles.default = {
        isDefault = true;
        extensions = [
          (pkgs.fetchFirefoxAddon {
            name = "tbkeys";
            url = "https://github.com/wshanks/tbkeys/releases/download/v2.4.3/tbkeys.xpi";
            hash = "sha256-2e+T5Nr5kc2s8EykFzWKaJZ2jPUDHh9Cqn4hCuDCLaM=";
          })
        ];
      };
      settings = {
        "mail.tabs.drawInTitlebar" = false;
        "ui.key.accelKey" = 91;
        "ui.key.textcontrol.prefer_native_key_bindings_over_builtin_shortcut_key_definitions" = true;
        "extensions.tbkeys.mainkeys" = builtins.toJSON {
          # cycle panes
          "ctrl+x o" = "eval:document.commandDispatcher.advanceFocus()";
          # navigation
          "alt+n" = "cmd:cmd_nextMsg";
          "alt+p" = "cmd:cmd_previousMsg";
          # actions
          "c" = "cmd:cmd_newMessage";
          "r" = "cmd:cmd_reply";
          "a" = "cmd:cmd_replyAll";
          "f" = "cmd:cmd_forward";
          "d" = "cmd:cmd_delete";
          "e" = "cmd:cmd_archive";
          "enter" = "cmd:cmd_openMessage";
          "u" = "tbkeys:closeMessageAndRefresh";
          # unset defaults that conflict
          "j" = "unset";
          "k" = "unset";
          "o" = "unset";
          "x" = "unset";
          "#" = "unset";
        };
      };
    };

    claude-code = {
      enable = true;
      # enableMcpIntegration = true;
      # ~/.claude/settings.json is the unprofiled fallback (used by `command
      # claude` and any non-claude-prof invoker). It uses the work profile's
      # settings so behavior matches `claude-prof run work`. Per-profile
      # settings.json lives under ~/.config/claude-<name>/ (rendered by home.file
      # below).
      settings = claudeProfileSettings.work;
      #
      # Pin a newer CLI than nixpkgs ships (it lags the upstream native-binary
      # releases). Override version + prebuilt src; the checksum is the sha256 hex
      # from https://downloads.claude.ai/claude-code-releases/<version>/manifest.json
      # (same source nixpkgs uses). Bump both when updating.
      package =
        let
          claudeVersion = "2.1.220";
          platformKey = "${pkgs.stdenv.hostPlatform.node.platform}-${pkgs.stdenv.hostPlatform.node.arch}";
          checksums = {
            linux-x64 = "674f61f20ff306f3100cf9200e4c36c4b70278b5bef2884549819b942a89c863";
            darwin-arm64 = "8addc857f3fe64d5a0368af9ee50321b50afb4a6918ba3ef018ab84f5dbbe081";
          };
        in
        pkgs.claude-code.overrideAttrs (_: {
          version = claudeVersion;
          src = pkgs.fetchurl {
            url = "https://downloads.claude.ai/claude-code-releases/${claudeVersion}/${platformKey}/claude";
            sha256 = checksums.${platformKey};
          };
        });
    };

    home-manager.enable = true;

    difftastic = {
      enable = !isDarwin;
      # Deliberately NOT wiring git integration: `git.enable = true` sets
      # `diff.external`, making `git diff` emit difftastic's structural view
      # instead of a unified diff. That breaks the pager (diff-so-fancy can't
      # parse it), `git diff > x.patch`, and every tool/skill that parses diff
      # output. `git dft` (alias below) opts in per-invocation instead.
      git.enable = false;
      options = {
        # display = "inline";
      };
    };
    firefox = {
      enable = isDesktop;
      # Linux: env.nix sets MOZ_LEGACY_PROFILES=1 (and system Arch firefox uses
      # legacy unconditionally), so use .mozilla/firefox. macOS reads from
      # ~/Library/Application Support/Firefox.
      configPath = if isDarwin then "Library/Application Support/Firefox" else ".mozilla/firefox";
      # Drop Version= so Firefox uses non-dedicated profile mode and honors
      # Default=1 — else Firefox 67+ pins profile-per-install via [Install<HASH>]
      # sections in profiles.ini and ignores Default=.
      profileVersion = null;
      policies = {
        Homepage = {
          URL = "https://ko.ag/newtab.html";
          StartPage = "homepage";
        };
      };
      nativeMessagingHosts = lib.optionals (!isDarwin) [
        pkgs.tridactyl-native
      ];
      profiles.default = {
        extensions.packages = lib.optionals (!isDarwin) (with pkgs.nur.repos.rycee.firefox-addons; [
          bitwarden
          tridactyl
        ]);
        settings = {
          "sidebar.verticalTabs" = true;
          "ui.key.accelKey" = 91;
          "ui.key.textcontrol.prefer_native_key_bindings_over_builtin_shortcut_key_definitions" = true;
          "signon.rememberSignons" = false;
          "browser.newtab.extensionControlled" = false;
          "browser.ml.chat.enabled" = false;
          # WebTransport workaround: this profile reports hasThirdPartyRoots=1
          # for every QUIC connection (even public sites chaining to built-in
          # roots), so Firefox's third-party-roots policy kills H3. HTTPS falls
          # back to H2; WebTransport has no fallback and fails with "WebTransport
          # connection rejected". See netwerk/protocol/http/Http3Session.cpp
          # Authenticated() and bugzilla 1929093.
          "network.http.http3.disable_when_third_party_roots_found" = false;
        };
      };
    };
    ghostty = lib.mkIf (!isDarwin) {
      enable = isDesktop;
      package = config.lib.nixGL.wrap pkgs.ghostty;
      enableZshIntegration = true;
      systemd.enable = false;
      # installBatSyntax = true;

      settings = {
        # Default is `auto`, which picks io_uring on Linux. Don't: ghostty's
        # libxev loop parks one thread per ring in io_cqring_wait(), which
        # sleeps via io_schedule() and so sets current->in_iowait. The kernel
        # counts that as blocked-on-IO -- it lands in procs_blocked and is
        # flagged TSK_IOWAIT for PSI -- even though the ring only holds idle
        # IORING_OP_POLL_ADD watches on the pty fds and no disk IO happens.
        #
        # Because every other thread in the cgroup is idle-sleeping, PSI's
        # "full" (all non-idle tasks stalled) reads ~100% for ghostty's scope
        # and sums up the cgroup chain into user.slice and /proc/pressure/io.
        # Measured 2026-08-28: fujiwara's session scope sat at full avg300=99.88
        # for its entire 88-day life with every disk at inflight=0; utsuho had
        # 4 rings, 4 threads in io_cqring_wait and procs_blocked exactly 4,
        # while Slack and Firefox read 0.00 (they use epoll).
        #
        # This is ghostty-org/ghostty#3246 / discussion#3224, whose accepted
        # answer is exactly this setting. Kernel commit 7b72d661f1f2 (6.5) gated
        # iowait on having pending requests, which does NOT help here: the armed
        # pty polls *are* pending requests. The real upstream fix is
        # IORING_ENTER_NO_IOWAIT (kernel 6.15+, probed via IORING_FEAT_NO_IOWAIT),
        # which Zig's IoUring does not expose yet -- ziglang/zig#25566.
        #
        # mitchellh measured no benchmark difference between the backends, and
        # in_iowait also blocks deeper CPU idle states, so epoll is not a
        # trade-off here. Revert to `auto` once libxev passes NO_IOWAIT.
        async-backend = "epoll";

        keybind = [
          "ctrl+enter=text:\\r"
          "performable:super+c=copy_to_clipboard"
          "performable:super+v=paste_from_clipboard"
          "super+t=new_tab"
          "ctrl+comma=unbind"
        ];
      } // lib.optionalAttrs hidpi.enabled {
        font-size = hidpi.ghosttyFontSize;
      };
    };
    gh = {
      enable = true;
      gitCredentialHelper.enable = true;
    };
    ripgrep = {
      enable = true;
      arguments = [
        "--smart-case"
        "--type-add"
        "ql:*.{ql,qll}"
        "--hidden"
      ];
    };
    dircolors = {
      enable = true;
      enableZshIntegration = true;
      extraConfig = builtins.readFile ./home/dircolors;
    };
    direnv = {
      enable = true;
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
      options = [ "--cmd cd" ];
    };

    fzf.enable = true;

    man = {
      enable = true;
      mandoc.enable = true;
      man-db.enable = false;
    };

    git = {
      enable = true;
      package = gitWithLibsecret;
      ignores = [
        ".DS_Store"
        ".vscode"
        "*~"
        "\\#*#"
        "*.orig"
        ".#*"
        ".dir-locals.el"
        "*.zip"
        "*.tar"
        "*.out"
        "*.xz"
        "*.gz"
        "*.7z"
        "shell.nix"
        "flake.nix"
        "flake.lock"
        "*.local.json"
        "*.local.toml"
        ".aider*"
        "**/.claude/worktrees"
        "**/.claude/scheduled_tasks.lock"
        "**/.claude/plans"
        "**/.superpowers"
        ".gemini-review.agent.json" # gemini-review skill's Hunk sidecar artifact
        # Disables the main-checkout edit guard; must stay local, never handed
        # to anyone else. Does not stop a third-party repo shipping its own.
        "**/.claude/allow-main-edit"
      ];

      signing = {
        format = "openpgp";
        signByDefault = false;
        key = "git@sjle.co";
      };

      lfs.enable = true;

      settings = {
        user.name = "Sauyon Lee";
        user.email = "git@sjle.co";
        safe.directory = [
          "/tf/*"
        ];
        init.defaultBranch = "main";
        commit = {
          verbose = true;
        };
        push = {
          default = "current";
        };
        color = {
          ui = "auto";
        };
        core = {
          pager = "${pkgs.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less -RFx4";
          editor = if isDarwin then "/usr/bin/emacsclient -t" else "${withHostNss emacsPkg}/bin/emacsclient -t";
          whitespace = "trailing-space,space-before-tab";
        };
        diff.algorithm = "histogram";
        # Opt into difftastic per-invocation rather than globally via
        # diff.external — see programs.difftastic above.
        #
        # Shell alias rather than plain `-c` because git always pipes an external
        # differ into core.pager, so difft sees a pipe not a tty: it drops color
        # (--color=auto) and falls back to 80 columns. Hence DFT_COLOR/DFT_WIDTH,
        # plus a pager override since diff-so-fancy can't parse difft's output.
        alias = lib.mkIf (!isDarwin) {
          dft =
            let
              difft = "${lib.getExe config.programs.difftastic.package}";
              tput = "${pkgs.ncurses}/bin/tput";
              less = "${pkgs.less}/bin/less";
            in
            "!DFT_COLOR=always DFT_WIDTH=\${DFT_WIDTH:-$(${tput} cols 2>/dev/null || echo 120)} "
            + "git -c diff.external=${difft} -c core.pager='${less} -RFX' diff";
        };
        pull.rebase = true;
        merge.tool = "meld";
        # credential."https://github.com".helper = "!/usr/bin/env gh auth git-credential";
        # credential."https://gist.github.com".helper = "!/usr/bin/env gh auth git-credential";
        # forge.ko.ag (Forgejo over HTTPS via Cloudflare tunnel): reuse the token
        # `fj auth login` stored instead of a second copy in the secret service.
        # See git-credential-fj above.
        credential."https://forge.ko.ag".helper = "${git-credential-fj}/bin/git-credential-fj";
        # huggingface.co: persist the HF token in the secret service so
        # `hf auth login --add-to-git-credential` and direct git HTTPS clones /
        # LFS pulls of Hub repos authenticate without re-prompting.
        credential."https://huggingface.co".helper = "${gitWithLibsecret}/bin/git-credential-libsecret";
      };
    };

    gpg = {
      enable = true;
      settings = {
        keyserver = "hkps://keyserver.ubuntu.com";
      };
    };

    ssh = lib.optionalAttrs (!isDarwin) {
      enable = true;

      enableDefaultConfig = false;

      # Freeform `settings` API (not the deprecated matchBlocks/extraOptions):
      # attribute names are Host patterns, values use OpenSSH directive names.
      settings = {
        "aur" = {
          HostName = "aur.archlinux.org";
          User = "aur";
        };
        "github" = {
          HostName = "github.com";
          User = "git";
        };
        "codeberg.org" = {
          HostName = "codeberg.org";
          User = "git";
        };
        "shizuka" = {
          Port = 59049;
        };
        "akane" = {
          Port = 59049;
        };
        "kanon" = {
          User = "root";
          HostName = "kanon.alai-ionian.ts.net";
          Port = 59048;
        };
        "yui mio meiko ritsu mugi azusa" = {
          ForwardAgent = true;
        };
        "testserver" = {
          HostName = "35.163.118.10";
          User = "ubuntu";
        };
        "testclient" = {
          HostName = "52.38.68.189";
          User = "ubuntu";
        };
        "tf" = {
          HostName = "kanon.ko.ag";
          Port = 59048;
          ForwardAgent = true;
          RemoteForward = [
            {
              bind.address = "/run/user/1000/gnupg/S.gpg-agent";
              host.address = "/run/user/1000/gnupg/S.gpg-agent.extra";
            }
          ];
        };
        "prod-db-subnet-router" = {
          User = "ec2-user";
        };
        "bcctl-subnet-router" = {
          User = "ubuntu";
        };

        # `bin/coder`, not `bin/.coder-wrapped`: the overlay above sets
        # `postInstall = ""`, which drops nixpkgs' terraform PATH wrapper, so
        # `bin/coder` IS the real binary and no `.coder-wrapped` is produced.
        # Reaching past the wrapper — correct before that override — now names a
        # file that does not exist, and home-manager will not clobber the stale
        # working ~/.ssh/config to tell you so.
        "coder.*" = {
          UserKnownHostsFile = "/dev/null";
          ConnectTimeout = "0";
          StrictHostKeyChecking = "no";
          LogLevel = "ERROR";
          ProxyCommand = "${pkgs.coder}/bin/coder --global-config /home/sauyon/.config/coderv2 ssh --stdio --ssh-host-prefix coder. %h";
        };
        # `header` is the escape hatch for a block header carrying Nix string
        # context (the store path), which can't live in an attr name.
        "*.coder-proxy" = {
          header = "Match host *.coder !exec \"${pkgs.coder}/bin/coder connect exists %h\"";
          ProxyCommand = "${pkgs.coder}/bin/coder --global-config /home/sauyon/.config/coderv2 ssh --stdio --hostname-suffix coder %h";
        };
        "*.coder" = {
          UserKnownHostsFile = "/dev/null";
          ConnectTimeout = "0";
          StrictHostKeyChecking = "no";
          LogLevel = "ERROR";
        };
      };
    };

    starship = {
      enable = false;

      settings = {
        add_newline = false;
        scan_timeout = 10;

        git_status = {
          ahead = "⇡\${count}";
          diverged = "⇡\${ahead_count}⇣\${behind_count}";
          behind = "⇣\${count}";
          untracked = "?\${count}";
          modified = "!\${count}";
          staged = "+\${count}";
          renamed = "»\${count}";
          deleted = "×\${count}";
        };

        kubernetes = {
          disabled = false;
        };
      };
    };

    zsh = import ./zsh.nix (
      args
      // {
        xdg = config.xdg;
        home = config.home.homeDirectory;
      }
    );

    zellij = {
      enable = true;
      enableZshIntegration = false;
      settings = {
        keybinds = {
          normal = {
            "bind \"Alt s\"".SwitchToMode = "Locked";
            unbind = "Ctrl g";
          };
          locked = {
            "bind \"Alt s\"".SwitchToMode = "Normal";
            unbind = "Ctrl g";
          };
        };

        default_mode = "locked";
        pane_frames = false;
        show_startup_tips = false;
      };
    };
  };

  xdg = {
    mime.enable = !isDarwin;

    portal = {
      enable = !isDarwin && isDesktop;
      extraPortals = lib.optionals (!isDarwin && isDesktop) [
        (withHostNss pkgs.xdg-desktop-portal-gtk)
      ];
      xdgOpenUsePortal = !isDarwin && isDesktop;
      config = {
        common.default = [ "hyprland;gtk" ];
      };
    };

    mimeApps = {
      enable = !isDarwin && isDesktop;

      defaultApplications = {
        "text/html" = "firefox.desktop";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/mailto" = "thunderbird.desktop";
        "message/rfc822" = "thunderbird.desktop";
      };
    };

    dataHome = "${config.home.homeDirectory}/.local/share";
    configHome = "${config.home.homeDirectory}/.config";
    cacheHome = "${config.home.homeDirectory}/.cache";

    userDirs = {
      enable = true;

      desktop = "${config.home.homeDirectory}/desktop";
      documents = "${config.home.homeDirectory}/documents";
      download = "${config.home.homeDirectory}/downloads";
      music = "${config.home.homeDirectory}/drive/music";
      pictures = "${config.home.homeDirectory}/images";
    };

    configFile."newtab.html".text = newtabHtml;

    # Standalone home-manager doesn't put ~/.nix-profile/share/systemd/user in
    # systemd's search path, so the dbus-activated portal services fail with
    # "unknown unit" and ghostty's OpenURI portal call falls back to spawning
    # xdg-open (and the browser) as a child. Symlink the units in so dbus finds them.
    configFile."systemd/user/xdg-desktop-portal.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.xdg-desktop-portal}/share/systemd/user/xdg-desktop-portal.service";
    };
    configFile."systemd/user/xdg-document-portal.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.xdg-desktop-portal}/share/systemd/user/xdg-document-portal.service";
    };
    configFile."systemd/user/xdg-permission-store.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.xdg-desktop-portal}/share/systemd/user/xdg-permission-store.service";
    };
    configFile."systemd/user/xdg-desktop-portal-rewrite-launchers.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.xdg-desktop-portal}/share/systemd/user/xdg-desktop-portal-rewrite-launchers.service";
    };
    configFile."systemd/user/xdg-desktop-portal-gtk.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.xdg-desktop-portal-gtk}/share/systemd/user/xdg-desktop-portal-gtk.service";
    };
    configFile."systemd/user/xdg-desktop-portal-hyprland.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.xdg-desktop-portal-hyprland}/share/systemd/user/xdg-desktop-portal-hyprland.service";
    };

    configFile."explore-mcp/config.json".text = builtins.toJSON {
      explorers = { cursor = { }; codex = { }; gemini = { }; opencode = { }; };
      summarizer = { backend = "claude"; maxChars = 4000; };
    };



    configFile."tridactyl/tridactylrc".text = ''
      " vim: set filetype=vim

      set smoothscroll true

      unbind d
      bind <A-x> fillcmdline_notrail

      " J/K for tabs, x to close
      bind x tabclose

      " Detach tab to new window
      bind gd tabdetach

      " Reopen current tab in a container via a fuzzy picker. JS lives in
      " ~/.config/tridactyl/reopencontainer.js (deployed below by home-manager).
      bind gC js -r reopencontainer.js

      " Only hint search results on Google/DDG
      bindurl www.google.com f hint -Jc #search a
      bindurl www.google.com F hint -Jbc #search a

      " Move hover URL to right so it doesn't overlap the command line
      guiset_quiet hoverlink right

      " Ignore Tridactyl on sites with their own keybindings
      autocmd DocStart mail.google.com mode ignore

      " Emacs bindings in insert mode
      bind --mode=insert <C-f> !s xdotool key Right
      bind --mode=insert <C-b> !s xdotool key Left
      bind --mode=insert <C-n> !s xdotool key Down
      bind --mode=insert <C-p> !s xdotool key Up
      bind --mode=insert <C-a> !s xdotool key Home
      bind --mode=insert <C-e> !s xdotool key End
      bind --mode=insert <C-d> !s xdotool key Delete
      bind --mode=insert <C-k> !s xdotool key shift+End Delete
      bind --mode=insert <C-w> !s xdotool key ctrl+BackSpace

      " C-g to cancel
      bind --mode=insert <C-g> composite unfocus | mode normal
      bind --mode=ex <C-g> ex.hide_and_clear

      " Emacs bindings in command line
      bind --mode=ex <C-f> ex.next_char
      bind --mode=ex <C-b> ex.prev_char
      bind --mode=ex <C-a> text.beginning_of_line
      bind --mode=ex <C-e> text.end_of_line
      bind --mode=ex <C-d> text.delete_char
      bind --mode=ex <C-k> text.kill_line
      bind --mode=ex <C-w> text.backward_kill_word
      bind --mode=ex <C-n> ex.next_completion
      bind --mode=ex <C-p> ex.prev_completion

      " External editor
      set editorcmd emacsclient -n

      " Wayland clipboard
      set externalclipboardcmd wl-copy
    '';

    configFile."tridactyl/reopencontainer.js".text = ''
      // Fuzzy picker that reopens the current tab in a chosen container.
      // Invoked from tridactylrc via `:js -r reopencontainer.js` on gC.
      (async () => {
        try {
          const containers = await tri.browserBg.contextualIdentities.query({});
          if (!containers.length) return;
          const [tab] = await tri.browserBg.tabs.query({active: true, currentWindow: true});
          const url = tab.url;
          const oldId = tab.id;
          const newIndex = tab.index + 1;

          const existing = document.getElementById("__tri_cpicker");
          if (existing) existing.remove();

          try { tri.excmds.mode("ignore"); } catch (e) {}

          const root = document.createElement("div");
          root.id = "__tri_cpicker";
          root.style.cssText = "position:fixed;top:15%;left:50%;transform:translateX(-50%);z-index:2147483647;background:#1e1e1e;color:#eee;border:1px solid #555;border-radius:6px;padding:8px;min-width:320px;max-width:480px;font-family:monospace;font-size:14px;box-shadow:0 8px 24px rgba(0,0,0,0.6);";

          const input = document.createElement("input");
          input.type = "text";
          input.placeholder = "fuzzy container...";
          input.spellcheck = false;
          input.autocomplete = "off";
          input.style.cssText = "width:100%;background:#111;color:#eee;border:1px solid #444;padding:6px 8px;box-sizing:border-box;font-family:inherit;font-size:inherit;outline:none;border-radius:3px;";

          const listEl = document.createElement("div");
          listEl.style.cssText = "margin-top:6px;max-height:320px;overflow-y:auto;";

          const hint = document.createElement("div");
          hint.textContent = "enter: select   esc: cancel   up/down or ^p/^n: move";
          hint.style.cssText = "margin-top:6px;font-size:11px;color:#888;";

          root.appendChild(input);
          root.appendChild(listEl);
          root.appendChild(hint);
          document.body.appendChild(root);

          let selected = 0;
          let filtered = containers.slice();

          const score = (q, s) => {
            if (!q) return 1;
            q = q.toLowerCase();
            s = s.toLowerCase();
            let qi = 0;
            let sc = 0;
            let lastIdx = -1;
            for (let si = 0; si < s.length && qi < q.length; si++) {
              if (s[si] === q[qi]) {
                sc += (si === lastIdx + 1 ? 2 : 1);
                lastIdx = si;
                qi++;
              }
            }
            return qi === q.length ? sc : 0;
          };

          const render = () => {
            listEl.textContent = "";
            filtered.forEach((c, i) => {
              const item = document.createElement("div");
              item.textContent = c.name;
              item.style.cssText = "padding:4px 8px;cursor:pointer;border-radius:3px;" + (i === selected ? "background:#0066cc;color:#fff;" : "");
              item.addEventListener("mousedown", (e) => {
                e.preventDefault();
                selected = i;
                pick();
              });
              listEl.appendChild(item);
            });
          };

          const refilter = () => {
            const q = input.value.trim();
            if (!q) {
              filtered = containers.slice();
            } else {
              filtered = containers
                .map(c => ({ c, s: score(q, c.name) }))
                .filter(x => x.s > 0)
                .sort((a, b) => b.s - a.s)
                .map(x => x.c);
            }
            selected = 0;
            render();
          };

          let cleanedUp = false;
          const cleanup = () => {
            if (cleanedUp) return;
            cleanedUp = true;
            root.remove();
            document.removeEventListener("keydown", onKey, true);
            try { tri.excmds.mode("normal"); } catch (e) {}
          };

          const pick = async () => {
            const target = filtered[selected];
            cleanup();
            if (!target) return;
            try {
              await tri.browserBg.tabs.create({ url, cookieStoreId: target.cookieStoreId, index: newIndex, active: true });
              await tri.browserBg.tabs.remove(oldId);
            } catch (e) {
              console.error("reopencontainer pick:", e);
            }
          };

          const onKey = (e) => {
            const k = e.key;
            if (k === "Escape") {
              e.preventDefault(); e.stopImmediatePropagation();
              cleanup();
            } else if (k === "Enter") {
              e.preventDefault(); e.stopImmediatePropagation();
              pick();
            } else if (k === "ArrowDown" || (e.ctrlKey && k === "n")) {
              e.preventDefault(); e.stopImmediatePropagation();
              if (filtered.length) { selected = (selected + 1) % filtered.length; render(); }
            } else if (k === "ArrowUp" || (e.ctrlKey && k === "p")) {
              e.preventDefault(); e.stopImmediatePropagation();
              if (filtered.length) { selected = (selected - 1 + filtered.length) % filtered.length; render(); }
            }
          };

          document.addEventListener("keydown", onKey, true);
          input.addEventListener("input", refilter);

          render();
          input.focus();
        } catch (e) {
          console.error("reopencontainer:", e);
        }
      })()
    '';
  };
}
