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
  zen-browser,
  dotfiles-private,
  machine,

  system,
  ...
}:

let
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  hostname = machine.hostname;
  isDesktop = machine.gui or true;
  gpu = machine.gpu or null;

  # Values the public repo deliberately does not carry: see the
  # `dotfiles-private` flake input for why each one is in there rather than
  # here. Imported once, used from `private.*` below.
  private = {
    identity = import "${dotfiles-private}/identity.nix";
    endpoints = import "${dotfiles-private}/endpoints.nix";
    newtabLinks = import "${dotfiles-private}/newtab-links.nix";
    claudeAutoModeEnvByHost =
      import "${dotfiles-private}/claude-auto-mode.nix" { inherit hostname; };
  };
  # Secret Service provider, keyed off one axis so the two halves cannot drift:
  # desktops get gnome-keyring, headless hosts get pass-secret-service (see
  # services.pass-secret-service below). Gating these on different axes — gui vs
  # hostname — made "no provider at all" representable, which fails silently in
  # the libsecret consumers (git credential helper, huggingface).
  gnomeKeyringHost = !isDarwin && isDesktop;

  # Hosts whose OS is ours to manage: the Arch boxes, the same four system/deploy
  # converges pacman on. `!isDarwin` is NOT this predicate -- kyuusaku is a Linux
  # host whose distribution we do not own, which is the reason deploy keeps an
  # explicit allow-list rather than testing `command -v pacman`, and the reason
  # this is a list too. A third list rather than a reuse of the two nearby ones,
  # because each means a different thing: wifHosts is "enrolled in the WIF trust
  # root", home.nix's `hms` case is "has a Linux CI job", and kyuusaku is absent
  # from all three for three unrelated reasons.
  archHosts = [ "utsuho" "setsuna" "shiori" "fujiwara" ];
  isArchHost = !isDarwin && builtins.elem hostname archHosts;

  # ── sops trust root (dotfiles domain) ───────────────────────────────────────
  # Design: ~/devel/reports/Homelab secrets bootstrap trust root.md, Part A.
  #
  # The secrets fleet: hosts that decrypt secrets.yaml at all. kyuusaku is a work
  # box that will never get a device identity and needs none of these secrets,
  # and setsuna is being decommissioned, so enrolling it would only mint a key to
  # revoke later. Both are out entirely -- no sops.secrets, which is what
  # switches sops-nix's module (unit, activation, sops-install-secrets) off for
  # them. An allowlist, not `!= "kyuusaku"`, so a new host starts out of the
  # fleet: it cannot decrypt anything until enrolled anyway.
  secretsHosts = [ "utsuho" "shiori" "fujiwara" "mari" ];
  isSecretsHost = builtins.elem hostname secretsHosts;

  # Hosts listed here decrypt secrets.yaml through a device identity: a local
  # P-256 key under ~/.config/ko (see wifKeyFile) signs a 5-minute JWT, Google STS validates
  # it against the JWKS in the ko-keys-sauyon bucket, and the federated token
  # decrypts with KMS host-key. No decryption key on the host. Hosts NOT listed
  # keep the cluster-domain path (gcp-key.json -> nix-key) until enrolled.
  wifHosts = [ "shiori" "fujiwara" "utsuho" ];
  useWif = builtins.elem hostname wifHosts;
  # Hosts whose device key lives in the TPM (install/wif/tpm-keygen.sh) instead
  # of in a file. A separate list from wifHosts because it is a separate fact:
  # mari is darwin and has no TPM at all, and a host is enrolled (its JWK is
  # published) before and independently of where it keeps the private half.
  # Listing a host here before its TPM key's JWK is in the bucket JWKS gives it
  # an STS 401 at the next activation — publish first, switch second.
  wifTpmHosts = [ "shiori" "fujiwara" "utsuho" ];
  useWifTpm = useWif && !isDarwin && builtins.elem hostname wifTpmHosts;
  wifKeyFile = "${config.home.homeDirectory}/.config/ko/"
    + (if useWifTpm then "wif-tpm.pem" else "wif.pem");
  wifIssuer = "https://storage.googleapis.com/ko-keys-sauyon/hosts";
  wifAudience = private.endpoints.wifAudience;
  # sops-nix runs sops-install-secrets with PATH="" — every path here is absolute.
  koWifToken = pkgs.writeShellScriptBin "ko-wif-token" ''
    export KO_OPENSSL=${pkgs.openssl}/bin/openssl
    ${lib.optionalString (!isDarwin) "export KO_TIMEDATECTL=/usr/bin/timedatectl"}
    ${lib.optionalString useWifTpm ''
      # openssl loads the TPM key only through this provider, and finds providers
      # by OPENSSL_MODULES. Both come from the same `pkgs`, which is the point —
      # but note nothing enforces that at runtime: tpm2.so's RUNPATH holds no
      # openssl at all, so it resolves libcrypto from the loading process and any
      # ABI-compatible OpenSSL 3.x would load it. The pairing is a build-time
      # header dependency, kept honest here by both names coming from one pkgs.
      export OPENSSL_MODULES=${pkgs.tpm2-openssl}/lib/ossl-modules
      # Same reason as gnome-keyring-tpm above: the nixpkgs TSS defaults to
      # tcti-abrmd, a resource-manager daemon this host does not run. /dev/tpmrm0
      # is the kernel's own resource manager and needs only the tss group.
      export TPM2OPENSSL_TCTI=device:/dev/tpmrm0
    ''}
    exec ${pkgs.python3}/bin/python3 ${./home/scripts/ko-wif-token.py} "$@"
  '';
  # Non-secret by construction (GCP documents credential configs as safe to commit).
  wifCredentialConfig = pkgs.writeText "wif-hosts.json" (builtins.toJSON {
    type = "external_account";
    audience = wifAudience;
    subject_token_type = "urn:ietf:params:oauth:token-type:jwt";
    token_url = "https://sts.googleapis.com/v1/token";
    credential_source.executable = {
      command = lib.concatStringsSep " " [
        "${koWifToken}/bin/ko-wif-token"
        "--key" wifKeyFile
        "--iss" wifIssuer
        "--sub" "device:${hostname}"
        "--aud" wifAudience
        "--adc"
      ];
      timeout_millis = 10000;
    };
  });

  # Emacs is NOT part of the desktop stack: it runs headless as a daemon and is
  # reached over tty/SSH with `emacsclient -t` (zsh.nix's non_gui branch already
  # assumes exactly that). Only the graphical *frame* needs a GUI, so pick the
  # build by isDesktop rather than dropping emacs on headless hosts — dropping it
  # takes $EDITOR, git core.editor and the edit/sedit helpers down with it.
  # Unversioned: nixpkgs retired the emacs30-* aliases in August 2026.
  emacsPkg = if isDesktop then pkgs.emacs-pgtk else pkgs.emacs-nox;

  btopPkg =
    if gpu == "amd" then pkgs.btop-rocm
    else if gpu == "nvidia" then pkgs.btop-cuda
    else pkgs.btop;

  # App-level scaling. Multiplies with laptopScale below, which is the
  # compositor's; a host wanting both would get the product.
  hidpi = let
    scale = if hostname == "setsuna" || hostname == "fujiwara" then 1.25 else 1.0;
    enabled = scale != 1.0;
    # Which of the two mechanisms carries `scale` to GTK. text-scaling-factor
    # goes through dconf and is read at runtime via a live dconf D-Bus service,
    # so it only works on a host that has one; GDK_DPI_SCALE is read straight
    # out of the environment and works anywhere. Exactly one per host — setting
    # both multiplies them, which is the double-scaling the laptopScale comment
    # below also guards against.
    #
    # A flag, not a `hostname ==` at each use site: the hosts this picks out are
    # not "setsuna" in any meaningful sense, they are the hosts with a dconf
    # service, and the two use sites (GDK_DPI_SCALE, dconf.settings) have to
    # stay each other's exact complement. Deriving it once here is what makes
    # that checkable. It is emphatically NOT the gate on dconf.enable — see the
    # dconf block for what keying those together cost.
    viaDconf = hostname == "setsuna";
  in {
    inherit scale enabled viaDconf;
    qtFontDpi = builtins.floor (96.0 * scale);
    cursorSize = if enabled then 48 else 24;
    waybarFontSize = if enabled then 20 else 17;
    waybarBarHeight = if enabled then 48 else 42;
    ghosttyFontSize = 14;
  };

  edgeGap = if hostname == "fujiwara" then 20 else 0;
  # Compositor scale for the internal panel, consumed by hyprland.nix's eDP-1
  # rule. Per-host because the panels differ: shiori's is 2880x1920 in 280x190mm
  # (~260dpi), which wants 2x; utsuho's and setsuna's are the ones described in
  # the kanshi output blocks below. 1 means "no eDP-1 rule at all" — the panel
  # falls through to Hyprland's catch-all monitor rule, as it always has.
  # This multiplies with hidpi.scale, so keep at most one of the two off 1 per
  # host: setsuna/fujiwara scale apps, shiori scales the compositor.
  laptopScale = if hostname == "shiori" then 2 else 1;
  noDpmsOutputs = [
    "HDMI-A-1"
  ];

  # `throw`, not `null`: every consumer either interpolates this into a string or
  # puts it in a list behind the same `!isDarwin && isDesktop` guard, and nothing
  # compares it to null -- so the sentinel is only ever reached by a *mistake*.
  # As null that mistake surfaces as "cannot coerce null to a string" with no
  # attribute, file or hint; naming itself costs nothing and is equally lazy. It
  # matters more since hyprpolkitagent's ExecStart started depending on it, because
  # that unit's failure mode is silent.
  # Which GL stack every nix-built GUI app on a desktop host is launched against.
  # ONE decision with TWO consumers, which is the whole reason it is a binding:
  #
  #   * targets.genericLinux.nixGL.defaultWrapper (set below) drives
  #     config.lib.nixGL.wrap -- ghostty, hyprlock, hyprpaper, cumora.
  #   * the `nixGL` shim just below drives hyprpolkitagent's ExecStart, and is the
  #     `nixGL` on PATH.
  #
  # Keeping them in one place is not tidiness. They are reached by different code
  # paths, so a reader who patches only the shim gets a config that evaluates
  # while four apps quietly stay on mesa -- which is the failure this whole area
  # exists to prevent, arrived at by a different route.
  #
  # mesa covers Intel *and* AMD: nixgl's mesa wrapper is named nixGLIntel, but it
  # is "nixGL + mesa" and radeonsi ships in mesa's own lib/dri, so utsuho
  # (gpu = "amd") is correctly served by it. `null` is the unstated case -- shiori
  # and setsuna declare no gpu and are Intel.
  #
  # Anything needing a proprietary driver is a different wrapper, and handing it
  # mesa reproduces hyprpolkitagent's EGL abort: GL initialises against the wrong
  # driver or not at all, and the app dies the first time it renders, long after
  # activation reported success. So refuse at eval rather than guess. A loud build
  # failure on a host that does not exist yet is the cheap end of this trade.
  glWrapper =
    if gpu == null || gpu == "intel" || gpu == "amd" then
      "mesa"
    else
      throw (
        "nixGL: no wrapper mapped for gpu=\"${gpu}\" (hostname=${hostname}). "
        + "Fix BOTH consumers of glWrapper in home.nix or neither: "
        + "targets.genericLinux.nixGL.defaultWrapper takes one of "
        + "mesa/mesaPrime/nvidia/nvidiaPrime and drives config.lib.nixGL.wrap "
        + "(ghostty, hyprlock, hyprpaper, cumora), while nixGLVendor below maps "
        + "the same choice onto a nixgl attr for hyprpolkitagent's ExecStart. "
        + "Patching one alone leaves the other on mesa, silently."
      );

  # The nixgl attribute implementing glWrapper. An attrset lookup rather than an
  # if-chain so that adding a glWrapper value without a shim mapping fails here,
  # loudly, instead of falling through to Intel.
  nixGLVendor = { mesa = "nixGLIntel"; }.${glWrapper};

  nixGL =
    if isDarwin || !isDesktop then
      throw "nixGL is desktop-Linux only (hostname=${hostname}); guard the reference with (!isDarwin && isDesktop)"
    else
      pkgs.writeShellScriptBin "nixGL" ''
        exec ${nixgl.packages.${system}.${nixGLVendor}}/bin/${nixGLVendor} "$@"
      '';
  # hunk builds on all four systems, so no darwin guard needed.
  hunk-pkg = hunk.packages.${system}.default;
  # explore-mcp builds on all four systems (pure JS), so no darwin guard.
  explore-mcp-pkg = explore-mcp.packages.${system}.default;
  # drovr — Rust CLI, buildRustPackage on all unix systems; pairs with herdr.
  # These two RunIdentity tests collide on ext4: same-path recreate reuses the
  # inode, and btime is tick-granular, so (dev, ino, born) repeats. Real drovr
  # bug, not just flake — drop the skip once RunIdentity stops trusting stat.
  drovr-pkg = drovr.packages.${system}.default.overrideAttrs (old: {
    checkFlags = (old.checkFlags or [ ]) ++ [
      "--skip=review::tests::a_recreated_run_is_a_different_identity"
      "--skip=review::tests::an_identity_learned_late_is_adopted_rather_than_left_unknown"
    ];
  });

  # kcs — kube config switch helper; `zsh.nix` runs `kcs init`. Not in nixpkgs.
  kcs = pkgs.buildGoModule rec {
    pname = "kcs";
    version = "0.2.2";
    src = pkgs.fetchFromGitHub {
      owner = "FogDong";
      repo = "kcs";
      rev = "v${version}";
      hash = "sha256-kh57ooLzY9ttkrKVHvbh97qlD0CDZPJ93VaGy0Yj5ZM=";
    };
    vendorHash = "sha256-n2MhWWb7T4zzgmo66PzhmV89S15WcPEK00hgFYTXP8A=";
    ldflags = [ "-X github.com/FogDong/kcs/cmd.version=${version}" ];
  };

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

  # cryptomator-cli, with liburing and libnuma pulled into LD_LIBRARY_PATH so
  # jFuse can dlopen them from /nix/store at runtime, instead of falling
  # through to /usr/lib via /etc/ld.so.cache. Read nix/cryptomator-cli.nix
  # for the full reasoning; in two sentences: the host's transient
  # /run/user/1000/ldshim handled it until its tmpfs went away on a reboot,
  # and /usr/lib on a Nix binary's loader path breaks Nix-built glibc. Same
  # derivation flake.nix exposes as `cryptomator-cli-wrapped` so the test
  # script builds one and asserts against the same binary the live profile
  # ships.
  cryptomator-cli = import ./nix/cryptomator-cli.nix { inherit pkgs; };

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
          ${pkgs.hyprland}/bin/hyprctl dispatch "hl.dsp.dpms({ action = \"$1\", monitor = \"$mon\" })"
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

  # Tell the lock screen when pam_faillock has the account locked out. hyprlock
  # forwards a PAM message only when it contains "left to unlock"
  # (src/auth/Pam.cpp), so PAM's own "The account is locked due to N failed
  # logins." is dropped, and the countdown that does survive shows up only after a
  # password has been submitted -- typing the right one and being rejected anyway
  # is how you find out. This reads the tally instead, so a label can say it up
  # front. No root needed: /run/faillock/$USER is mode 0660 owned by the user.
  hyprlock-faillock = pkgs.writeShellScriptBin "hyprlock-faillock" ''
    set -u

    # The lock screen's lockout is enforced by the pam hyprlock links, not the host's:
    # nix libpam loads pam_faillock.so from its own lib/security, and that module has
    # its own faillock.conf path compiled in. So read THAT file -- reading
    # /etc/security/faillock.conf would report host policy while the lock screen went
    # on enforcing whatever nix's copy says (both are upstream's all-commented
    # default today, so the numbers agree, which is exactly why the divergence would
    # go unnoticed). Module arguments in /etc/pam.d/system-auth would override the
    # file for either pam and are invisible from here; the host's stack passes none.
    CONF="''${FAILLOCK_CONF:-${pkgs.pam-host-chkpwd}/etc/security/faillock.conf}"
    # Unreadable (a nixpkgs that stops installing it): read nothing, which lands on
    # pam built-in defaults -- the same thing read_config_file() leaves pam with.
    # Falling back to the host file would reintroduce exactly the divergence above.
    [ -r "$CONF" ] || CONF=/dev/null
    # The reader from that same pam, for the same reason: its record layout is
    # version-locked to the module that writes the tally, and it resolves the `dir`
    # option from the conf above -- so if either is ever moved off /var/run/faillock,
    # reader and writer move together. The host's reader would resolve `dir` from the
    # host's conf and could end up looking in the wrong place entirely. Wrapped in
    # withHostNss because it calls getpwnam(): on a systemd-homed host only the host's
    # NSS resolves the name, exactly as for hyprlock itself.
    BIN="''${FAILLOCK_BIN:-${withHostNss pkgs.pam-host-chkpwd}/bin/faillock}"
    # FAILLOCK_FALLBACK, unset, is the host reader; empty is how a test reaches the
    # no-reader-at-all path, since /usr/bin/faillock exists on this host.
    [ -x "$BIN" ] || BIN="''${FAILLOCK_FALLBACK-/usr/bin/faillock}"
    [ -n "$BIN" ] && [ -x "$BIN" ] || exit 0

    # Whose tally to report. SUDO_USER, but only when this process really is root:
    # under `sudo hyprlock-faillock` the interesting tally is the invoking user's and
    # root's own is empty, which would read as "not locked out" at the moment someone
    # is checking whether they are -- while under `sudo -u alice` (or a shell holding
    # a stale SUDO_USER) it names someone who is not running this. Otherwise the
    # host's id, for the same reason hyprlock itself needs withHostNss: on a
    # systemd-homed host the user has no /etc/passwd entry and only the host's NSS
    # resolves the name. Nix's id is the fallback, and works wherever passwd does; -u
    # is numeric, so it needs no NSS at all.
    WHO="''${FAILLOCK_USER:-}"
    if [ -z "$WHO" ] && [ "$(${pkgs.coreutils}/bin/id -u 2>/dev/null || echo 1)" = 0 ]; then
      WHO="''${SUDO_USER:-}"
    fi
    [ -n "$WHO" ] || WHO=$(/usr/bin/id -un 2>/dev/null || ${pkgs.coreutils}/bin/id -un 2>/dev/null || true)
    [ -n "$WHO" ] || exit 0

    # Arch ships faillock.conf with every option commented out, so an option that is
    # not set means pam's built-in default, not zero.
    # $3 marks a duration: "time" gets pam's MAX_TIME_INTERVAL clamp, "never-ok" that
    # plus the `never` spelling, which only the unlock times accept
    # (pam_faillock(8) documents it as equivalent to 0). deny passes neither: pam
    # rejects a non-numeric deny and keeps its default, so `deny = never` must not
    # read as 0 here -- that would silence the label on a screen still locking at 3.
    optval() {
      v=$(${pkgs.gnused}/bin/sed -nE \
        "s/^[[:space:]]*$1([[:space:]]*=[[:space:]]*|[[:space:]]+)([^[:space:]#]+).*/\2/p" \
        "$CONF" 2>/dev/null \
        | ${pkgs.coreutils}/bin/tail -1)
      [ "''${3:-}" = never-ok ] && [ "$v" = never ] && v=0
      case "$v" in
        "" | *[!0-9]*) printf '%s' "$2"; return ;;
      esac
      # Strip leading zeros before any arithmetic: `[ 09 -gt 1 ]` is an octal error in
      # the shell (and 0900 would compare as 576), while pam parses decimally.
      while [ "''${v#0}" != "$v" ] && [ -n "''${v#0}" ]; do v="''${v#0}"; done
      # faillock_config.c rejects a duration over MAX_TIME_INTERVAL (7 days) and keeps
      # its default. The digit-count test first, so the arithmetic cannot overflow.
      if [ -n "''${3:-}" ] && { [ "''${#v}" -gt 7 ] || [ "$v" -gt 604800 ]; }; then
        printf '%s' "$2"
        return
      fi
      printf '%s' "$v"
    }
    DENY=$(optval deny 3)
    UNLOCK=$(optval unlock_time 600 never-ok)
    INTERVAL=$(optval fail_interval 900 time)
    [ "$DENY" -gt 0 ] || exit 0

    if [ "$WHO" = root ]; then
      # pam never denies root unless the conf opts in: check_tally() returns
      # PAM_SUCCESS early for an admin with no FAILLOCK_FLAG_DENY_ROOT, while the
      # authfail path still records the failure -- so root can hold a full tally and
      # never be locked, and a countdown here would be invented. Only even_deny_root
      # sets that flag from a conf file: faillock_config.c stores root_unlock_time
      # without touching the flags, whatever the man page says about implying it.
      ${pkgs.gnugrep}/bin/grep -qE "^[[:space:]]*even_deny_root([[:space:]=]|$)" "$CONF" || exit 0
      # Unset, it defaults to unlock_time (pam_faillock.c initialises it to
      # MAX_TIME_INTERVAL+1 and substitutes unlock_time when it is still that).
      # Not modelled: admin_group, which makes a plain user is_admin too and would
      # need group resolution; the conf this reads is a store file that cannot set it.
      UNLOCK=$(optval root_unlock_time "$UNLOCK" never-ok)
    fi

    OUT=$("$BIN" --user "$WHO" 2>/dev/null) || exit 0

    printf '%s\n' "$OUT" | ${pkgs.gawk}/bin/awk \
      -v deny="$DENY" -v unlock="$UNLOCK" -v interval="$INTERVAL" \
      -v now="$(${pkgs.coreutils}/bin/date +%s)" '
      # Skip the "user:" line and the column header.
      NR <= 2 { next }
      # Fixed-width columns: "<date> <time> <type> <source> V|I".
      $NF == "V" {
        ts = $1 " " $2
        gsub(/[-:]/, " ", ts)
        t = mktime(ts)
        if (t < 0) next
        times[++n] = t
        if (t > latest) latest = t
      }
      END {
        if (!n) exit 0
        # Two counts, because pam uses two windows. check_tally() decides the lockout
        # over the records within fail_interval of the NEWEST failure; write_tally()
        # then voids every record older than fail_interval measured from NOW, so it is
        # that second count which says what the next failure will add up to.
        for (i = 1; i <= n; i++) {
          if (latest - times[i] < interval) fails++
          if (now - times[i] < interval) live++
        }
        # "Password locked out", not "Locked out": the fingerprint path in hyprlock
        # talks to fprintd over D-Bus and never enters PAM, so pam_faillock does not
        # gate it -- the reader still opens the screen while this label is up.
        # (No apostrophes in here: this whole program is single-quoted in the shell.)
        if (fails >= deny) {
          if (unlock == 0) {
            print "Password locked out — no automatic unlock"
            exit 0
          }
          left = latest + unlock - now
          if (left > 0) {
            printf "Password locked out — %d min left\n", int((left + 59) / 60)
            exit 0
          }
          # pam denies while latest + unlock_time >= now, and only prints its
          # countdown when there is a minute left to print.
          if (left == 0) {
            print "Password locked out"
            exit 0
          }
          # unlock_time has elapsed. check_tally() takes its expiry branch, which sets
          # FAILLOCK_FLAG_UNLOCKED, and the next write_tally() voids every record on
          # that flag -- the whole deny budget is back, so there is nothing to warn
          # about and a count here would be a lie.
          exit 0
        }
        if (!live) exit 0
        printf "%d/%d failed attempts\n", live, deny
      }
    '
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

  # Steam is the pacman package (system/packages.shiori), but it inherits a PATH
  # whose first entry is ~/.nix-profile/bin, and that breaks it in one measured
  # way. Steam shells out to `xdg-user-dir <key>` (the string in steamclient.so is
  # the format `xdg-user-dir %s`) to locate a user directory;
  # xdg.userDirs.setSessionVariables below already exports XDG_DOWNLOAD_DIR, but
  # Steam asks the binary anyway, and xdg.userDirs.enable puts nix's xdg-user-dirs
  # in the profile, so the call lands on the nix copy. Measured on shiori
  # 2026-09-28, once per launch:
  #
  #   xdg-user-dir: symbol lookup error: /usr/lib/libc.so.6: undefined symbol:
  #   __pointer_chk_guard, version GLIBC_PRIVATE
  #
  # The mechanism is a mismatched loader/libc pair, and it runs the opposite way to
  # what the message suggests at a glance. Measured with `nm -D` on this box:
  # Arch's ld.so *defines* __pointer_chk_guard@@GLIBC_PRIVATE and Arch's
  # libc.so.6 leaves it *undefined*, expecting its own loader to supply it. nix's
  # glibc 2.42 defines it in neither. The nix binary keeps nix's hardcoded ELF
  # interpreter, Steam's runtime puts Arch's /usr/lib/libc.so.6 in its library
  # path, and nix's older loader cannot satisfy that newer libc's reference --
  # which is why the error names /usr/lib/libc.so.6 as the referrer, not the binary.
  #
  # The fix is to put the host's directories FIRST, and deliberately not to
  # sanitise nix out of PATH: `xdg-open` exists ONLY in the profile here (Arch's
  # xdg-utils is not installed) and is how Steam opens a link in a browser.
  #
  # One caveat that belongs next to that argument rather than left out of it: only
  # libcef.so invokes a bare `xdg-open` resolved through PATH. linux64/steamclient.so
  # and ubuntu12_64/steamwebhelper hardcode the absolute /usr/bin/xdg-open, which does
  # not exist on this box -- so those link-opening paths are already broken, whatever
  # PATH says, and the profile copy is reachable only via CEF. The prepend is still
  # the right call, but it preserves one codepath, not all of them.
  #
  # Whether the handler also gets a repaired *library* path is a separate question,
  # and the honest answer is that it is inferred. steam.sh saves this wrapper's
  # environment as SYSTEM_PATH/SYSTEM_LD_LIBRARY_PATH (:135-136), and steamclient.so
  # references both names in `LD_LIBRARY_PATH=... PATH=... <cmd>` prefixes -- but the
  # commands identifiable in `strings` are SteamOS power/branch helpers, not the URL
  # handler. It matters because nix's xdg-open is a bash script whose *interpreter* is
  # a nix-glibc binary, so it dies the same way xdg-user-dir did if a Steam-runtime
  # library path is in force.
  #
  # Not steam.sh:1015: that restore is on the restart path ("Restore paths before
  # restarting if we need to", feeding the MAGIC_RESTART_EXITCODE re-exec), which an
  # earlier draft of this comment miscited as the handler path. And that re-exec is
  # `exec "$0"` where $0 is ~/.local/share/Steam/steam.sh -- bin_steam.sh:313 exec's
  # the bootstrap directly -- so the tray's restart re-runs steam.sh and NOT this
  # wrapper. The fix survives anyway, and the restore is precisely why: PATH comes
  # back from SYSTEM_PATH, which is this wrapper's already-prepended PATH, and the
  # GIO_EXTRA_MODULES unset is inherited rather than re-applied.
  #
  # The cost, stated plainly because it is larger than the fix: this reorders name
  # resolution for Steam's whole descendant tree, not just Steam. Games, Proton
  # helpers and anything a launch option invokes now get the host's copy of any name
  # the host also provides -- and via SYSTEM_PATH, so do external handlers. Nothing
  # is removed (nix-only names like xdg-open stay reachable, and container launches
  # get pressure-vessel's own PATH), and the reordering runs toward root-owned
  # directories, since /usr/local/bin and /usr/bin are root:root here while
  # ~/.nix-profile/bin is under $HOME. A shim directory holding only xdg-user-dir
  # would scope this tighter; prepending is preferred because it needs no list of
  # which helpers to redirect, and that list is the thing that would silently rot.
  #
  # GIO_EXTRA_MODULES (env.nix) goes for a related reason -- the journal carries this
  # in pairs, eight records over four launches, tagged steamwebhelper for the first
  # pair and steam for the rest:
  #
  #   libgvfscommon.so: undefined symbol: g_task_set_static_name
  #
  # Only the glib *inside Steam's own runtime* is too old for it, and the host is not
  # part of this: Arch's glib2 is 2.88.3, the same version nix's gvfs is built
  # against, and it exports the symbol. g_task_set_static_name landed in glib 2.76,
  # while the runtime that logged this ships 2.66.8 and the scout runtime the 32-bit
  # client runs under ships 2.58.3. The journal names that runtime by path --
  # steamrt64/pv-runtime -- and `steamrt3c` is what steam.sh and the platform
  # directory call it; `journalctl -g steamrt3c` finds nothing, so an earlier draft
  # attributing the name to the journal was wrong. (An earlier draft also said
  # "sniper"; this install has none, steam.sh rm -fr's it as "now replaced by
  # steamrt3c".)
  # Steam's client UI is CEF, not GTK, so the
  # removable-drive volume monitor that variable exists for buys Steam nothing. It
  # is unset by name rather than by a GIO_* sweep, so GIO_MODULE_DIR survives --
  # though note the unset is inherited too, so a browser first started from a Steam
  # link comes up without gvfs's GIO module. Small blast radius here because the
  # GTK file chooser goes through xdg-desktop-portal-gtk, a separate user unit with
  # its own environment.
  #
  # `set -u` with a $PATH reference is safe: bash supplies a compiled-in default when
  # the variable is absent, so this cannot abort on a launcher that passes no PATH.
  # Worth naming the default rather than leaving it to be assumed -- for nixpkgs bash
  # it is `/no-such-path`, not the system one, so the inherited tail is a directory
  # that does not exist. Harmless here, since everything this wrapper needs is in the
  # three directories it prepends, and verified against the built wrapper under
  # `env -i`.
  #
  # STEAM_BIN was a test seam and now has no consumer -- tests/steam-env.sh was
  # removed. Left in place because it is inert (nothing sets it, so the default
  # is what runs) and because removing it would rewrite the shipped wrapper for
  # no functional gain. It is not a privilege boundary either way: anything that
  # can write this process's environment could exec what it liked regardless --
  # and could more durably just rewrite the Exec= line of the 0644 desktop file
  # this change installs.
  steam = pkgs.writeShellScriptBin "steam" ''
    set -u
    export PATH="/usr/local/bin:/usr/bin:/bin:$PATH"
    unset GIO_EXTRA_MODULES
    exec "''${STEAM_BIN:-/usr/bin/steam}" "$@"
  '';

  # The Exec= rewrite that points the packaged steam.desktop at the wrapper above.
  # A repo script rather than an inline writeShellScript, the way
  # system/thermald-setup is one: it can then be read and run directly instead of
  # only existing inside a built generation. Its own header carries the reasoning.
  steam-desktop-override = ./home/steam-desktop-override;

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
  # That claim only holds because the passphrase is generated on the host and
  # escrowed nowhere. It used to live in secrets.yaml as `gnomeKeyringPassphrase`,
  # which made it false twice over: one value opened every host's keyring file, and
  # it was decryptable by a cloud KMS from anywhere, so "off this machine" bought
  # nothing. The escrow's only benefit was surviving a TPM clear without re-signing
  # in -- and what lives in here is a Bitwarden refresh token, a huggingface token
  # and fj's store, all of which a sign-in replaces. There is deliberately no
  # recovery path now: a cleared TPM means a new keyring.
  #
  # None of that is retroactive. A host enrolled before this still has the old
  # shared passphrase in its sealed blob, and the old value remains in git history
  # where its recipients open it -- so treat it as burned, not gone. Such a host
  # becomes per-host only by re-enrolling (gnome-keyring-tpm-seal --force), which
  # costs it the keyring.
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

  # One-time enrolment per keyring. Takes no input and no secret -- it generates
  # the passphrase itself, seals it, and verifies the round-trip:
  #   gnome-keyring-tpm-seal
  #   systemctl --user restart gnome-keyring
  # It refuses if a login.keyring already exists, because a new passphrase cannot
  # open an old keyring; `--force` is the "yes, I am giving up those secrets" flag.
  # Cases: tests/gnome-keyring-seal.sh.
  gnome-keyring-tpm-seal = pkgs.writeShellScriptBin "gnome-keyring-tpm-seal" ''
    set -euo pipefail

    export TPM2TOOLS_TCTI="device:/dev/tpmrm0"
    SEAL="''${XDG_DATA_HOME:-$HOME/.local/share}/gnome-keyring-tpm"
    RUN="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"
    # Override seams for tests/gnome-keyring-seal.sh, gated behind SEAL_TEST so a
    # real run cannot be redirected by a stray exported variable. That gate is not
    # tidiness: SEAL_KEYRING_DIR is the guard standing between a user and an
    # unopenable keyring, and SEAL_TPM2_BIN decides which binaries get to see the
    # passphrase.
    if [ "''${SEAL_TEST:-0}" = 1 ]; then
      TPM2_BIN="''${SEAL_TPM2_BIN:-${pkgs.tpm2-tools}/bin}"
      KEYRINGS="''${SEAL_KEYRING_DIR:-''${XDG_DATA_HOME:-$HOME/.local/share}/keyrings}"
    else
      TPM2_BIN="${pkgs.tpm2-tools}/bin"
      KEYRINGS="''${XDG_DATA_HOME:-$HOME/.local/share}/keyrings"
    fi

    FORCE=0
    for a in "$@"; do
      case "$a" in
        --force) FORCE=1 ;;
        *) echo "gnome-keyring-tpm-seal: unknown argument: $a (only --force)" >&2; exit 2 ;;
      esac
    done

    # A new passphrase cannot open an existing login.keyring -- that file is
    # encrypted with the old one, and there is no re-key path here. Sealing over it
    # would leave the daemon holding a passphrase the keyring has never seen, which
    # reads exactly like a corrupt keyring and loses every secret in it. Refuse
    # before touching anything.
    # Every store the daemon opens, not just login.keyring: it runs with
    # --components="pkcs11,secrets", and user.keystore (the PKCS#11 half) keeps its
    # own unlock secret *inside* the login keyring. A fresh login passphrase
    # therefore orphans the keystore too, and checking only login.keyring made the
    # refusal's own advice ("delete it, run again") break things -- that leaves
    # user.keystore behind and then passes the guard.
    stores() {
      ${pkgs.findutils}/bin/find "$KEYRINGS" -maxdepth 1 \
        \( -name '*.keyring' -o -name 'user.keystore' \) -printf '%f\n' 2>/dev/null || true
    }

    EXISTING="$(stores)"
    if [ "$FORCE" -eq 0 ] && [ -n "$EXISTING" ]; then
      echo "gnome-keyring-tpm-seal: $KEYRINGS already holds:" >&2
      printf '    %s\n' $EXISTING >&2
      echo "  A freshly generated passphrase cannot open any of them, so sealing now" >&2
      echo "  would lose every secret they hold. Enrolment is once per keyring, not" >&2
      echo "  once per boot." >&2
      echo "  If the TPM was cleared they are already unreadable, so nothing is lost by" >&2
      echo "  starting over: run with --force, which seals and then moves all of them" >&2
      echo "  aside, and sign back in to whatever stored secrets there (Bitwarden, the" >&2
      echo "  git credential helper). Then: systemctl --user restart gnome-keyring" >&2
      exit 1
    fi

    WORK="$(${pkgs.coreutils}/bin/mktemp -d "$RUN/gnome-keyring-seal.XXXXXX")"
    trap '${pkgs.coreutils}/bin/rm -rf "$WORK"' EXIT
    ${pkgs.coreutils}/bin/mkdir -p -m700 "$SEAL"

    # Generated here, per host, and never written anywhere but the TPM-sealed blob.
    # It used to be escrowed in secrets.yaml and piped in on stdin, which made the
    # "useless off this machine" claim above false -- one KMS-decryptable value
    # opened every host's keyring file -- and made a new host wait on a decrypt it
    # could not do. Nothing needs to know this value: the daemon unseals it, and a
    # TPM clear means starting the keyring over, which costs a few sign-ins.
    # base64, not raw bytes. The consumer is gnome-keyring-tpm above, which reads
    # the unsealed value with PW="$(unseal)" and pipes it with printf: command
    # substitution drops NUL bytes and strips trailing newlines, and --unlock reads
    # a newline-terminated password. Raw /dev/urandom would therefore hand the
    # keyring a *shorter* passphrase than was sealed for about a fifth of
    # enrolments, and an empty one when the first byte is 0x0a -- silently, since
    # the round-trip check below compares files rather than what the daemon gets.
    # Encoding keeps all 256 bits and makes the value survive both hops. (The old
    # escrowed value could not hit this: it was base64 in secrets.yaml already.)
    ${pkgs.coreutils}/bin/head -c 32 /dev/urandom \
      | ${pkgs.coreutils}/bin/base64 -w0 \
      | ${pkgs.coreutils}/bin/tr -d '\n' > "$WORK/pw"
    [ "$(${pkgs.coreutils}/bin/wc -c < "$WORK/pw")" -eq 44 ] \
      || { echo "gnome-keyring-tpm-seal: could not generate a 32-byte passphrase" >&2; exit 1; }

    # Seal into $WORK and only install after verifying. Writing straight into $SEAL
    # means any failure past this point -- a mismatch, a tpm2 error under set -e, a
    # signal -- leaves the blob the script just called untrusted as the one the
    # daemon reads at next start, having already destroyed the enrolment it
    # replaced. With nothing escrowed, that is unrecoverable.
    "$TPM2_BIN/tpm2_createprimary" -C o -g sha256 -G ecc -c "$WORK/primary.ctx" >/dev/null
    "$TPM2_BIN/tpm2_create" -C "$WORK/primary.ctx" -g sha256 -i "$WORK/pw" \
      -u "$WORK/seal.pub" -r "$WORK/seal.priv" >/dev/null

    # Prove the blob round-trips before trusting it, from a freshly re-derived
    # parent -- that is the path the daemon will actually take at next start.
    "$TPM2_BIN/tpm2_createprimary" -C o -g sha256 -G ecc -c "$WORK/verify.ctx" >/dev/null
    "$TPM2_BIN/tpm2_load" -C "$WORK/verify.ctx" \
      -u "$WORK/seal.pub" -r "$WORK/seal.priv" -c "$WORK/vseal.ctx" >/dev/null
    "$TPM2_BIN/tpm2_unseal" -c "$WORK/vseal.ctx" -o "$WORK/verify"
    ${pkgs.diffutils}/bin/cmp -s "$WORK/pw" "$WORK/verify" \
      || { echo "gnome-keyring-tpm-seal: seal/unseal round-trip MISMATCH, not trusting this blob" >&2; exit 1; }

    ${pkgs.coreutils}/bin/chmod 600 "$WORK/seal.pub" "$WORK/seal.priv"

    # Install via $SEAL itself, not straight from $WORK. $WORK is under
    # XDG_RUNTIME_DIR (tmpfs) and $SEAL is on disk, so `mv` between them is
    # copy-then-unlink rather than rename: ENOSPC or a signal partway through would
    # leave a NEW seal.pub beside the OLD seal.priv, an unloadable pair with the old
    # pub already gone -- the unrecoverable state the staging exists to avoid. Copy
    # both onto the same filesystem first, keep the outgoing pair as .prev so a
    # half-finished swap is still recoverable by hand, then rename, which is atomic
    # per file within one directory.
    ${pkgs.coreutils}/bin/cp -f "$WORK/seal.pub"  "$SEAL/seal.pub.new"
    ${pkgs.coreutils}/bin/cp -f "$WORK/seal.priv" "$SEAL/seal.priv.new"
    if [ -e "$SEAL/seal.pub" ] && [ -e "$SEAL/seal.priv" ]; then
      ${pkgs.coreutils}/bin/cp -f "$SEAL/seal.pub"  "$SEAL/seal.pub.prev"
      ${pkgs.coreutils}/bin/cp -f "$SEAL/seal.priv" "$SEAL/seal.priv.prev"
    fi
    ${pkgs.coreutils}/bin/mv -f "$SEAL/seal.priv.new" "$SEAL/seal.priv"
    ${pkgs.coreutils}/bin/mv -f "$SEAL/seal.pub.new"  "$SEAL/seal.pub"

    # Only now, with a working seal installed: the old keyring cannot be opened by
    # the passphrase just sealed, so leaving it in place would hand the daemon a
    # collection it cannot unlock and bring the prompts back -- right after this
    # script said "verified". Move it aside rather than delete it, so a user who
    # changes their mind still has the file even though nothing can read it.
    if [ "$FORCE" -eq 1 ] && [ -n "$EXISTING" ]; then
      stamp="$(${pkgs.coreutils}/bin/date +%Y%m%d%H%M%S)"
      for f in $EXISTING; do
        ${pkgs.coreutils}/bin/mv "$KEYRINGS/$f" "$KEYRINGS/$f.superseded-$stamp"
        echo "gnome-keyring-tpm-seal: moved $f aside as $f.superseded-$stamp (nothing can read it now)"
      done
    fi

    echo "gnome-keyring-tpm-seal: sealed to $SEAL and verified"
    # Named here, not only in the docs: until the daemon restarts it still holds the
    # old passphrase and will happily recreate login.keyring under it if anything
    # stores a secret in the meantime -- which then cannot be opened by what was
    # just sealed, and the guard above will refuse to re-enrol over it.
    echo "gnome-keyring-tpm-seal: now run: systemctl --user restart gnome-keyring"
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
  # forge.ko.ag is a `fj auth login` OAuth grant (LoginInfo::OAuth), not a
  # `fj auth add-token` application token: the access token in keys.json carries
  # an expires_at roughly an hour out, and only fj can mint a new one. Handing
  # git an expired one fails the push outright — the shim exits 0 with a
  # credential, so git never falls through to a prompt or to another helper.
  # So when the stored expiry has passed, poke fj first: any authenticated call
  # runs LoginInfo::refresh + KeyInfo::save (upstream src/keys.rs) and rewrites
  # keys.json in place, and the read below then picks up the new token.
  #
  # Gate the poke on the expiry rather than running it every time. fj's save()
  # is a truncate-in-place write with no locking, and a rotation whose new
  # refresh token never reaches disk wedges the login permanently with "token
  # was already used"; recovering needs `fj auth login`, which shells out to
  # xdg-open and is useless on this box over SSH. Same reason for the flock:
  # it keeps two concurrent git operations from racing a rotation against each
  # other. (macOS has no flock(1) in $PATH, so mari pokes unserialized — one
  # laptop, one user, and the window is a single expiry instant.)
  #
  # Application logins have no expires_at, so they skip the poke entirely and
  # this stays a pure keys.json read for them.
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

    # fj writes keys.json to its ProjectDirs data dir: on Linux that is
    # $XDG_DATA_HOME (~/.local/share); on macOS it is ~/Library/Application
    # Support/forgejo-cli.forgejo-cli, NOT ~/.local/share — so on darwin the XDG
    # path never exists and the shim must fall back to app-support, or git drops
    # to a prompt. (fj still reads its *config*, client_ids, from ~/.config on
    # both, so only this data path is platform-split.)
    keys="''${XDG_DATA_HOME:-$HOME/.local/share}/forgejo-cli/keys.json"
    [ -r "$keys" ] || keys="$HOME/Library/Application Support/forgejo-cli.forgejo-cli/keys.json"
    [ -r "$keys" ] || exit 0

    # expires_at is time::OffsetDateTime's serde tuple, in order:
    # [year, day-of-year, hour, minute, second, nanos, offset-h, offset-m,
    # offset-s]. jq's mktime wants [year, month0, mday, h, m, s, wday, yday]
    # and is UTC-based, so build January 1st and add the ordinal by hand.
    # `-e` makes "still valid" exit 0; anything else — expired, absent
    # (Application login or unknown host), or unparseable — exits nonzero.
    if ! ${lib.getExe pkgs.jq} -e --arg h "$host" '
      .hosts[$h].expires_at
      | if type == "array" and length >= 9 then
          ([.[0], 0, 1, .[2], .[3], .[4], 0, 0] | mktime)
            + (.[1] - 1) * 86400
            - (.[6] * 3600 + .[7] * 60 + .[8])
          > now
        else true end
    ' "$keys" >/dev/null 2>&1; then
      # Best-effort: a failed refresh still falls through to whatever is on
      # disk rather than dropping git to a terminal prompt.
      ${lib.optionalString (!isDarwin) ''${pkgs.util-linux}/bin/flock -w 60 "''${XDG_RUNTIME_DIR:-''${TMPDIR:-/tmp}}/git-credential-fj.lock" \''}
        ${lib.getExe pkgs.forgejo-cli} -H "$host" whoami >/dev/null 2>&1 || true
    fi

    # A nonzero exit aborts git's whole operation rather than falling through
    # to a prompt, so a half-written keys.json degrades to "no credential".
    token=$(${lib.getExe pkgs.jq} -r --arg h "$host" '.hosts[$h].token // empty' "$keys" 2>/dev/null) || exit 0
    [ -n "$token" ] || exit 0

    # Forgejo ignores the basic-auth username when the password is a token.
    printf 'username=oauth2\npassword=%s\n' "$token"
  '';

  # `hms` — switch this host, but let the forge do the building.
  #
  # A local switch compiles whatever is not in attic; the in-cluster runner has
  # far more of everything and pushes its result to attic, so waiting for CI and
  # then switching turns the switch into a pure download. The wait is only worth
  # it because the closure CI pushes is bit-identical to the one we would build.
  #
  # Refuses on a dirty tree: CI builds a pushed commit, so an uncommitted switch
  # is one CI can never reproduce, and silently building it locally would hide
  # that. `--local` is the escape hatch for exactly that case.
  # waypipe, wrapped so its DMABUF path can actually find a GPU.
  #
  # waypipe 0.10 rewrote DMABUF handling onto Vulkan (src/dmabuf.rs), so the
  # `dmabuf: true` the binary advertises is a build-time fact, not a runtime one:
  # it also needs a Vulkan driver, and a store-built loader on an Arch box has no
  # way to find one. The host's own manifest is not a fallback -- Arch's
  # /usr/share/vulkan/icd.d/radeon_icd.json names the bare soname
  # `libvulkan_radeon.so`, which a nix binary cannot resolve, so the loader reads
  # a manifest and still reports none. Measured on utsuho 2026-10-02:
  #
  #   ERR waypipe-server src/dmabuf.rs:970:
  #       Failed to create Vulkan instance: Unable to find a Vulkan driver
  #
  # waypipe then tells the client to drop the dmabuf protocols and the far-end
  # application dies in GTK init -- "Failed to initialize GTK", or from ghostty
  # the even blanker "Gtk: Failed to open display". No window and nothing naming
  # Vulkan, which is why this is worth a comment rather than a one-liner.
  #
  # Why not nixGL, the wrapper this repo already reaches for: nixGLIntel sets
  # GBM_BACKENDS_PATH, LIBGL_DRIVERS_PATH, LIBVA_DRIVERS_PATH,
  # __EGL_VENDOR_LIBRARY_FILENAMES and LD_LIBRARY_PATH -- and no Vulkan variable
  # at all. It would have changed nothing here. nixgl's separate nixVulkanIntel
  # does set VK_ICD_FILENAMES, but it also overwrites LD_LIBRARY_PATH (warning on
  # stderr as it goes) and drags in validation layers, for one variable we can set
  # ourselves.
  #
  # Both manifests, from one derivation, named rather than globbed:
  #   - Both, because shiori is Intel (anv) and utsuho AMD (radv), and the loader
  #     skips an ICD whose device is absent. Keying this on `gpu` would give the
  #     two ends different store paths, and waypipe refuses a version mismatch,
  #     so both ends must resolve to byte-identical manifests.
  #   - Named, because `builtins.readDir "${pkgs.mesa}/share/..."` is
  #     import-from-derivation, and CI evaluates every host without building.
  #     A filename that moves upstream therefore fails a test, not an eval.
  # --prefix, not --set: the host's own manifests stay behind ours, so a box that
  # grows a working system ICD is not cut off from it.
  waypipe = pkgs.waypipe.overrideAttrs (old: {
    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];
    postFixup = (old.postFixup or "") + ''
      wrapProgram $out/bin/waypipe \
        --prefix VK_ICD_FILENAMES : "${
          lib.concatStringsSep ":" [
            "${pkgs.mesa}/share/vulkan/icd.d/intel_icd.x86_64.json"
            "${pkgs.mesa}/share/vulkan/icd.d/radeon_icd.x86_64.json"
          ]
        }"
    '';
  });

  hms = pkgs.writeShellScriptBin "hms" ''
    set -euo pipefail

    # Seams, all defaulted to the real thing. tests/hms-ci-poll.sh drives the
    # built script through them: a throwaway repo, a stub forge, a stub
    # credential helper and a marker-file "switch", with the status poll bounded
    # to seconds rather than the hour it ships with.
    repo="''${HMS_REPO:-$HOME/devel/dotfiles}"
    host="$(uname -n)"
    forge="https://forge.ko.ag"
    slug="sauyon/dotfiles"
    curl="''${HMS_CURL:-${lib.getExe pkgs.curl}}"
    jq=${lib.getExe pkgs.jq}
    token_cmd="''${HMS_TOKEN_CMD:-${git-credential-fj}/bin/git-credential-fj}"
    wait_seconds="''${HMS_WAIT_SECONDS:-3600}"

    local_only=0
    hm_args=()
    for a in "$@"; do
      case "$a" in
        -l|--local) local_only=1 ;;
        -h|--help)
          echo "usage: hms [--local] [home-manager switch args...]"
          echo "  default: push HEAD, wait for its CI run, then switch (a download)"
          echo "  --local: skip CI and build here"
          exit 0 ;;
        *) hm_args+=("$a") ;;
      esac
    done

    switch_now() {
      exec ''${HMS_SWITCH_CMD:-home-manager switch} --flake "$repo#$host" ''${hm_args[@]+"''${hm_args[@]}"}
    }

    # mari (darwin) has no Linux CI job; nix-home.yml builds only these.
    case "$host" in
      utsuho|setsuna|fujiwara|shiori) ;;
      *) echo "hms: $host has no CI job — switching locally" >&2; switch_now ;;
    esac

    [ "$local_only" -eq 0 ] || switch_now

    if [ -n "$(git -C "$repo" status --porcelain)" ]; then
      echo "hms: working tree dirty. Commit before switching:" >&2
      git -C "$repo" status --short >&2
      echo "hms: or run 'hms --local' to build it here." >&2
      exit 1
    fi

    # Fetch before deciding: these boxes push to each other constantly, so an
    # unconditional `push HEAD:master` loses the race regularly, and `set -e`
    # would turn that into a wall of git hints. Worse, waiting on a run for a SHA
    # that is not what origin/master holds would be waiting on the wrong build.
    # Rebasing is a history decision, so say what is wrong and stop.
    git -C "$repo" fetch --quiet origin master
    sha=$(git -C "$repo" rev-parse HEAD)
    remote=$(git -C "$repo" rev-parse origin/master)
    if [ "$sha" = "$remote" ]; then
      echo "hms: ''${sha:0:7} is already origin/master"
    elif git -C "$repo" merge-base --is-ancestor "$remote" "$sha"; then
      # Fetching narrows the race but cannot close it — another box can advance
      # origin/master between the fetch and the push. Catch that rather than let
      # `set -e` surface the same hint wall this block exists to avoid. git keeps
      # its stderr: a rejected push and a failed auth are not the same problem,
      # and only a ref that actually moved earns the race message.
      if git -C "$repo" push --quiet origin HEAD:master; then
        echo "hms: pushed ''${sha:0:7}"
      else
        # Only a ref we watched move earns the race message. An ls-remote that
        # did not run tells us nothing, and guessing "someone else pushed" from
        # its empty output would point at the wrong culprit.
        moved=""
        if now=$(git -C "$repo" ls-remote --heads origin master 2>/dev/null | cut -f1) \
           && [ -n "$now" ] && [ "$now" != "$remote" ]; then
          moved=1
        fi
        if [ -n "$moved" ]; then
          echo "hms: push lost the race — origin/master moved just now. Re-run." >&2
        else
          echo "hms: push failed — see git's error above." >&2
        fi
        exit 1
      fi
    else
      behind=$(git -C "$repo" rev-list --count "$sha..$remote")
      ahead=$(git -C "$repo" rev-list --count "$remote..$sha")
      echo "hms: origin/master (''${remote:0:7}) has $behind commit(s) you do not have." >&2
      if [ "$ahead" -gt 0 ]; then
        echo "hms: and you have $ahead it does not — rebase, then re-run." >&2
      else
        echo "hms: you are strictly behind — pull, then re-run." >&2
      fi
      # -n 5 rather than `| head -5`: under pipefail, head closing the pipe makes
      # git exit 141 and `set -e` aborts on that instead of the exit 1 below.
      git -C "$repo" --no-pager log --oneline -n 5 "$sha..$remote" >&2
      exit 1
    fi

    # The token lands in a 0600 curl config, never in argv or the environment —
    # /proc/<pid>/cmdline and environ are world-readable.
    tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
    # Timeouts belong here, not on the call sites: the poll's hour-long bound
    # counts iterations, so a connection that stalls forever would never reach it.
    write_curlrc() {
      local tok
      # The first password line is the token, and only the first: the curlrc is
      # line-oriented, so a second one would not lengthen the header but end it
      # and leave a stray directive behind. Read to EOF rather than exiting on
      # the match — under `set -o pipefail` an early exit SIGPIPEs the helper and
      # fails the pipeline, the same trap the `-n 5` note further down guards.
      tok=$(printf 'host=forge.ko.ag\n\n' | "$token_cmd" get \
        | ${lib.getExe pkgs.gawk} '/^password=/ && !seen { sub(/^password=/, ""); print; seen = 1 }')
      [ -n "$tok" ] || return 1
      ( umask 077
        printf 'header = "Authorization: token %s"\nsilent\nconnect-timeout = 10\nmax-time = 120\n' \
          "$tok" > "$tmp/curlrc" )
    }
    if ! write_curlrc; then
      echo "hms: no forge.ko.ag token from $token_cmd — try 'fj auth login'." >&2
      exit 1
    fi

    # An HTTP error must not read as success — otherwise a dead token gives jq an
    # error body to find no runs in, and hms concludes "nothing CI-relevant
    # changed" and quietly builds locally. But --fail alone only yields curl's
    # exit 22 for everything >=400, which cannot tell an expired token from a
    # forge having a bad day, so the status code is carried out explicitly.
    # The code goes to a file, not a variable: every call site is `x=$(api ...)`,
    # which runs api in a subshell, so an assignment inside it would never reach
    # api_why and every failure would read as "could not reach". 0 means the
    # request never got an HTTP response at all.
    echo 0 > "$tmp/code"
    # Re-mint the header before every request rather than once per run.
    # forge.ko.ag is an OAuth grant whose access token expires roughly an hour
    # out — the same hour the status poll below is bounded by — so a snapshot
    # taken before the wait is routinely dead by the end of it, and the whole
    # wait then burns against a token nothing will refresh. git-credential-fj
    # owns the expiry check and the `fj whoami` poke that mints a new one;
    # asking it per request is what picks that up.
    #
    # Best-effort, exactly like the helper itself: a refresh that fails leaves
    # the last good curlrc in place, so one hiccup cannot throw away a wait that
    # is already minutes deep. The request then fails on its own merits and the
    # 401 branch of api_why says so.
    api() {
      local p="$1"; shift
      local out code
      write_curlrc || true
      if ! out=$($curl -K "$tmp/curlrc" -w '\n%{http_code}' "$forge$p" "$@"); then
        echo 0 > "$tmp/code"; return 1
      fi
      code=''${out##*$'\n'}
      echo "$code" > "$tmp/code"
      printf '%s' "''${out%$'\n'*}"
      [ "$code" -lt 400 ]
    }

    # Why the request failed, in the terms that decide what you do about it.
    api_why() {
      local code; code=$(cat "$tmp/code" 2>/dev/null || echo 0)
      case "$code" in
        0)       echo "could not reach $forge" ;;
        # Every request now re-mints the header through git-credential-fj, which
        # refreshes an expired grant on its own. So a 401 that survives that is
        # not staleness — it is a login that can no longer be refreshed, and the
        # fix is to log in again. Emphatically *not* `fj auth add-token`: an
        # application token has no expires_at, so it skips the refresh poke
        # entirely, and "fixes" this by abandoning the OAuth login instead.
        401|403) echo "$forge rejected our token (HTTP $code) — try 'fj auth login'" ;;
        5??)     echo "$forge returned HTTP $code — server-side, retry later" ;;
        *)       echo "$forge returned HTTP $code" ;;
      esac
    }

    # The job-id lookup is the exception: it *wants* the 404 body, which is the
    # only place Forgejo 13 names the job id. Failing on it would throw that away.
    api_raw() { local p="$1"; shift; write_curlrc || true; $curl -K "$tmp/curlrc" "$forge$p" "$@"; }

    # The workflow's `paths:` filter means a commit touching nothing nix-shaped
    # never starts a run. Give it 90s to appear, then stop waiting for a run that
    # is not coming.
    # Both poll loops tolerate a failed request rather than assign from a failing
    # pipeline: under `set -o pipefail` that would abort a wait minutes deep over
    # one blip. `last_ok` tracks the most recent attempt, not whether one ever
    # worked — the question at the deadline is "did the forge just tell us there
    # is no run", and a success ten tries ago does not answer it.
    echo -n "hms: waiting for a run on ''${sha:0:7}"
    run=""; last_ok=""; why=""; give_up=$((SECONDS + 90))
    while [ -z "$run" ]; do
      if tasks=$(api "/api/v1/repos/$slug/actions/tasks?limit=20"); then
        last_ok=1
        run=$(printf '%s' "$tasks" | $jq -r --arg s "$sha" 'first(.workflow_runs[]
            | select(.head_sha == $s and .workflow_id == "nix-home.yml")
            | .run_number) // empty' 2>/dev/null || true)
      else
        last_ok=""; why=$(api_why)
      fi
      [ -z "$run" ] || break
      if [ "$SECONDS" -ge "$give_up" ]; then
        echo
        if [ -n "$last_ok" ]; then
          echo "hms: no run for ''${sha:0:7} (nothing CI-relevant changed) — switching locally" >&2
        else
          echo "hms: ''${why:-request failed} — switching locally" >&2
        fi
        switch_now
      fi
      echo -n "."; sleep 10
    done
    echo; echo "hms: run $run — $forge/$slug/actions/runs/$run"

    # A blip here leaves $status alone and the loop simply asks again. Bounded at
    # an hour so a run that never reaches a terminal state cannot hang the shell —
    # by SECONDS, not an iteration count, since a request can burn max-time before
    # returning and a counted hour would then be several.
    status=""; degraded=""; deadline=$((SECONDS + wait_seconds))
    while :; do
      if tasks=$(api "/api/v1/repos/$slug/actions/tasks?limit=20"); then
        [ -z "$degraded" ] || { echo "hms: forge back" >&2; degraded=""; }
        status=$(printf '%s' "$tasks" | $jq -r --arg n "$run" 'first(.workflow_runs[]
            | select(.run_number == ($n | tonumber)) | .status) // empty' 2>/dev/null || true)
      else
        # Say it once per outage rather than every 15s, and once on recovery: an
        # hour of silence is indistinguishable from an hour of patient waiting.
        [ -n "$degraded" ] || { echo "hms: $(api_why); still waiting" >&2; degraded=1; }
      fi
      case "$status" in
        success|failure|cancelled|skipped) break ;;
      esac
      if [ "$SECONDS" -ge "$deadline" ]; then
        echo "hms: run $run still ''${status:-unknown} after ''${wait_seconds}s — giving up on the wait." >&2
        echo "hms: check $forge/$slug/actions/runs/$run, or 'hms --local'." >&2
        exit 1
      fi
      sleep 15
    done

    if [ "$status" = "success" ]; then
      echo "hms: run $run green — switching (should be a download)"
      switch_now
    fi

    echo "hms: run $run $status" >&2
    # Forgejo 13 exposes no run->job API, but the web job endpoint names the job
    # id in its error body, and /actions/jobs/<id>/logs then serves the log.
    job=$(api_raw "/$slug/actions/runs/$run/jobs/0" -X POST \
            -H 'Content-Type: application/json' -d '{"logCursors":[]}' \
          | ${lib.getExe pkgs.gnugrep} -o 'job_id [0-9]*' | tr -dc '0-9' || true)
    if [ -n "$job" ]; then
      log="$tmp/job.log"
      if ! api "/api/v1/repos/$slug/actions/jobs/$job/logs" > "$log"; then
        # Printing "--- CI error ---" over an empty file would read as "the run
        # failed silently", which is a different and much more alarming bug.
        echo "hms: could not fetch the job log: $(api_why)" >&2
        echo "hms: read it at $forge/$slug/actions/runs/$run" >&2
        exit 1
      fi
      echo "--- CI error ---" >&2
      # Timestamps are a fixed 29-char prefix; drop them so the errors read.
      # A pipeline's status is its last command, so grep's miss has to be
      # captured rather than tested through `cut | tail`.
      errs=$(${lib.getExe pkgs.gnugrep} -aE "^.{29}(error:|.*Job failed)" "$log" || true)
      if [ -n "$errs" ]; then
        printf '%s\n' "$errs" | cut -c30- | tail -30 >&2
      else
        tail -30 "$log" | cut -c30- >&2
      fi
    fi
    echo "hms: not switching. Fix CI, or 'hms --local' to build here." >&2
    exit 1
  '';

  # `hmeval` — answer "does this still evaluate?" without turning the laptop
  # into a build farm, and run the tests/ suite somewhere other than here.
  #
  # The default is LOCAL, because the question is usually cheap. Nix's evaluator
  # is single-threaded, so evaluation alone cannot saturate 24 cores — what does
  # is a build that evaluation starts: an import-from-derivation, or an input
  # that has to be realised before the expression referencing it can be read.
  # `--max-jobs 0` forbids exactly that. Nix then substitutes from attic or
  # stops with "cannot build ... max-jobs = 0", which is a far better outcome
  # than twenty minutes of fans: it means the answer needs the runner, and
  # `--ci` is one flag away.
  #
  # The systemd scope on top is belt-and-braces for the part --max-jobs cannot
  # bound — parallel substituter downloads and nix's own GC/daemon traffic.
  #
  # `--ci` pushes the WORKING TREE, dirty or not, to a throwaway `eval/<host>`
  # branch and reads the answer out of nix-eval.yml's job log. That is the whole
  # point: the case worth offloading is the one where you have just edited
  # home.nix and have not committed anything, which `hms` deliberately refuses.
  # It builds the commit with `commit-tree` against a private index, so your
  # real index, HEAD and working tree are never touched.
  hmeval = pkgs.writeShellScriptBin "hmeval" ''
    set -euo pipefail

    # Seams, all defaulted to the real thing — tests/hmeval.sh drives the built
    # script through them, the way tests/hms-ci-poll.sh drives hms.
    repo="''${HMEVAL_REPO:-$HOME/devel/dotfiles}"
    forge="https://forge.ko.ag"
    slug="sauyon/dotfiles"
    curl="''${HMEVAL_CURL:-${lib.getExe pkgs.curl}}"
    jq=${lib.getExe pkgs.jq}
    grep=${lib.getExe pkgs.gnugrep}
    awk=${lib.getExe pkgs.gawk}
    token_cmd="''${HMEVAL_TOKEN_CMD:-${git-credential-fj}/bin/git-credential-fj}"
    wait_seconds="''${HMEVAL_WAIT_SECONDS:-1800}"
    nix_cmd="''${HMEVAL_NIX:-nix}"
    quota="''${HMEVAL_CPUQUOTA:-200%}"
    memmax="''${HMEVAL_MEMORYMAX:-8G}"

    # Hardcoded for the same reason hms hardcodes its CI-host list: asking the
    # flake which configurations exist costs a full evaluation, which is the
    # thing this script exists to avoid paying for. Out of step with
    # flake.nix => a host silently never gets checked, so keep them together.
    all_hosts="utsuho setsuna fujiwara shiori kyuusaku mari"

    mode=eval
    ci=0
    hosts=""
    tests=""
    attr=""
    while [ $# -gt 0 ]; do
      case "$1" in
        -c|--ci)    ci=1 ;;
        -t|--tests)
          mode=tests; ci=1
          # Bare --tests means every script in tests/; names after it narrow it.
          while [ $# -gt 1 ] && case "$2" in -*) false ;; *) true ;; esac; do
            tests="$tests $2"; shift
          done ;;
        -a|--attr|-e|--expr)
          [ $# -ge 2 ] || { echo "hmeval: $1 needs an attribute path" >&2; exit 2; }
          mode=attr; attr="$2"; shift ;;
        -h|--help)
          echo "usage: hmeval [--ci] [host...]           evaluate host activation packages"
          echo "       hmeval [--ci] --attr <attrpath>   evaluate one attribute under the flake"
          echo "       hmeval --tests [script...]        run tests/ on the runner (implies --ci)"
          echo
          echo "  default is local: --max-jobs 0 --cores 1 inside a CPUQuota=$quota scope,"
          echo "  so it can never become a build. --ci pushes the working tree (dirty or"
          echo "  not) to eval/<host> and reads nix-eval.yml's answer back."
          exit 0 ;;
        -*) echo "hmeval: unknown flag $1 (try --help)" >&2; exit 2 ;;
        *)  hosts="$hosts $1" ;;
      esac
      shift
    done
    hosts="''${hosts# }"; tests="''${tests# }"
    [ -n "$hosts" ] || hosts="$all_hosts"

    ########################################################################
    # Local: bounded evaluation.
    ########################################################################
    if [ "$ci" -eq 0 ]; then
      # A --max-jobs 0 failure is the interesting one, so say what it means
      # rather than leaving nix's "cannot build" to look like a broken config.
      bounded() {
        # systemd-run is best-effort: a user manager that is not there (or is
        # the broken HOME=/ one a passwordless login leaves behind) must not
        # stop an evaluation that nice+max-jobs already bounds.
        if systemd-run --user --scope --quiet --collect \
             -p CPUQuota="$quota" -p MemoryMax="$memmax" -p MemorySwapMax=0 \
             -- true >/dev/null 2>&1; then
          systemd-run --user --scope --quiet --collect \
            -p CPUQuota="$quota" -p MemoryMax="$memmax" -p MemorySwapMax=0 \
            -- nice -n 19 "$@"
        else
          nice -n 19 "$@"
        fi
      }
      # One nix invocation per target, so a host that fails to evaluate names
      # itself instead of aborting the whole list. nix's own stderr is left
      # alone — the `error:` lines it prints are the entire point of running
      # this, and swallowing them to print a tidy FAILED would be worse.
      eval_one() {
        local label="$1" target="$2" out
        if out=$(bounded "$nix_cmd" eval --raw --max-jobs 0 --cores 1 "$target"); then
          printf '  %-10s %s\n' "$label" "$out"
        else
          printf '  %-10s FAILED\n' "$label" >&2
          return 1
        fi
      }

      rc=0
      if [ "$mode" = attr ]; then
        eval_one "$attr" ".#$attr" || rc=1
      else
        for h in $hosts; do
          eval_one "$h" ".#homeConfigurations.$h.activationPackage.drvPath" || rc=1
        done
      fi
      if [ "$rc" -ne 0 ]; then
        echo >&2
        echo "hmeval: if that said \"cannot build ... max-jobs = 0\", the config needs" >&2
        echo "hmeval: something built (IFD, or an input attic does not have). That is" >&2
        echo "hmeval: the runner's job: re-run as 'hmeval --ci'." >&2
      fi
      exit $rc
    fi

    ########################################################################
    # CI: push the working tree to a scratch branch and read the job log.
    ########################################################################
    [ -d "$repo/.git" ] || { echo "hmeval: $repo is not a git repo" >&2; exit 1; }

    # One branch per host, force-pushed over, and deliberately NOT deleted
    # afterwards.
    #
    # Deleting it looks tidier and is actively harmful: a deletion is itself a
    # push event on refs/heads/eval/<host>, so it matches nix-eval.yml's
    # `branches: ["eval/**"]` and starts a run of its own. That run lands in the
    # same concurrency group, and `cancel-in-progress: true` then has it kill
    # whichever real run is in flight. Because the forge processes the deletion
    # a little behind the push, what it kills is the *next* invocation's run,
    # not its own — runs 140, 141 and 142 were all cancelled this way, each by
    # the cleanup of the invocation before it, which reads as flaky CI rather
    # than as anything to do with cleanup.
    #
    # So the stale branch stays. It is one ref per host on a repo that is
    # already public, it is overwritten on the next run, and nothing reads it
    # between runs.
    branch="eval/$(uname -n)"
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT

    source ${./home/scripts/forge-api.sh}

    # The request file the workflow parses. Single-line values only: it is read
    # with `while IFS='=' read -r k v` in pure bash (the nix image has no awk or
    # sed), so a newline would silently become a second key.
    case "$mode$hosts$tests$attr" in
      *$'\n'*) echo "hmeval: newline in an argument" >&2; exit 2 ;;
    esac
    request="mode=$mode
hosts=$hosts
tests=$tests
attr=$attr
"

    # Build the commit against a private index: `git add -A` against the real
    # one would stage the user's whole working tree behind their back, and this
    # script has no business touching their index, HEAD or checkout.
    (
      export GIT_INDEX_FILE="$tmp/index"
      git -C "$repo" read-tree HEAD
      git -C "$repo" add -A
      blob=$(printf '%s' "$request" | git -C "$repo" hash-object -w --stdin)
      git -C "$repo" update-index --add --cacheinfo "100644,$blob,.hmeval-request"
      git -C "$repo" write-tree > "$tmp/tree"
    )
    tree=$(cat "$tmp/tree")
    # The nonce is load-bearing, and only since the branch stopped being
    # deleted. commit-tree is deterministic: same tree, same parent, same
    # message, same one-second timestamp gives the same sha. Re-running hmeval
    # with nothing changed — the most ordinary thing to do — then force-pushes
    # the sha the branch already points at, git sends nothing, no push event
    # fires, and hmeval waits out its 90 seconds before reporting "no run
    # appeared for <sha>", which reads like a broken workflow rather than like
    # a no-op push. Deleting the branch used to hide this by making every push
    # a branch creation. A nonce is cheaper than either.
    sha=$(git -C "$repo" commit-tree "$tree" -p HEAD \
            -m "hmeval: $mode on $(git -C "$repo" rev-parse --short HEAD)" \
            -m "nonce: $(date -u +%s%N)-$$")

    if ! git -C "$repo" push --force --quiet origin "$sha:refs/heads/$branch"; then
      echo "hmeval: could not push $branch — see git's error above." >&2
      exit 1
    fi
    # Progress goes to stderr, results to stdout, so `hmeval --ci | grep ...`
    # sees the answer and nothing else. The local path above holds the same
    # contract for the same reason.
    echo "hmeval: $mode @ ''${sha:0:7} -> $branch" >&2

    if ! write_curlrc; then
      echo "hmeval: no forge.ko.ag token from $token_cmd — try 'fj auth login'." >&2
      exit 1
    fi
    echo 0 > "$tmp/code"

    printf 'hmeval: waiting for a run' >&2
    if ! run=$(await_run "$sha" nix-eval.yml 90); then
      echo >&2
      if [ -s "$tmp/poll_ok" ]; then
        echo "hmeval: no run appeared for ''${sha:0:7} — is nix-eval.yml on master?" >&2
      else
        echo "hmeval: $(api_why)" >&2
      fi
      exit 1
    fi
    echo >&2; echo "hmeval: run $run — $forge/$slug/actions/runs/$run" >&2

    if ! status=$(await_status "$run" "$wait_seconds"); then
      echo >&2
      echo "hmeval: run $run never finished within ''${wait_seconds}s." >&2
      echo "hmeval: check $forge/$slug/actions/runs/$run." >&2
      exit 1
    fi
    echo >&2

    log="$tmp/job.log"
    if ! fetch_job_log "$run" "$log"; then
      echo "hmeval: run $run $status, but the log would not come: $(api_why)" >&2
      echo "hmeval: read it at $forge/$slug/actions/runs/$run" >&2
      exit 1
    fi

    # Every log line carries a timestamp prefix. hms hardcodes it at 29 chars
    # and cuts there, which is fine for finding `error:` lines but not here: an
    # off-by-one in that width leaves a stray character glued to the marker, the
    # anchored match fails, and a perfectly good run reports no output at all.
    # So measure the prefix off the opening marker instead of assuming it, and
    # strip exactly that much from the lines between. Unanchored on purpose.
    #
    # If the markers ever drift out of step with nix-eval.yml this prints
    # nothing, so say so rather than exiting silently green.
    body=$("$awk" '
      !inside && match($0, /---8<--- hmeval$/) { inside = 1; off = RSTART; next }
      inside && match($0, /---8<--- end$/)     { inside = 0 }
      inside { print substr($0, off) }' "$log" || true)
    if [ -n "$body" ]; then
      printf '%s\n' "$body"
    else
      echo "hmeval: no result block in the log — markers out of step with nix-eval.yml?" >&2
      cut -c30- "$log" | tail -30 >&2
    fi

    [ "$status" = success ] || { echo "hmeval: run $run $status" >&2; exit 1; }
  '';

  args = { inherit config lib pkgs; };

  # The local auto-mode classifier's PreToolUse entry. Currently unregistered —
  # add it back to `claudeBaseSettings.hooks.PreToolUse` to re-enable (the
  # plugin's files are still installed by home.file below).
  localAutoModeHook = {
    matcher = ".*";
    hooks = [ {
      type = "command";
      command = "python3 ${config.home.homeDirectory}/.claude/plugins/local-auto-mode/classifier.py";
      timeout = 15;
    } ];
  };

  # The whole of Claude Code's settings, rendered into ~/.claude/settings.json by
  # `programs.claude-code` below. This used to be a base that three profile
  # overlays sat on top of; the profiles are gone and this is the only config.
  # Facts about this host and these repos, handed to the auto-mode classifier as
  # `autoMode.environment` below. Written by hand rather than captured from
  # `/auto-mode-setup`: that flow ends by saving into <config dir>/settings.json,
  # which is a read-only store symlink on every host here, so it can only fail
  # with `Could not write .../settings.json`. Launching it against a scratch
  # CLAUDE_CONFIG_DIR does not dodge that -- it resolves the save path from the
  # live config dir, not from the one it was started with.
  #
  # Entries must be single-line plain text with no double quotes; Claude Code
  # validates the block and rejects the whole of autoMode if one is malformed.
  claudeAutoModeEnvShared = [
    "This user environment is managed declaratively by Nix home-manager from the dotfiles repo at ${config.home.homeDirectory}/devel/dotfiles, which builds six hosts: utsuho, kyuusaku, setsuna, shiori, fujiwara and mari. Everything under /nix/store is read-only on purpose."
    "A tool that cannot write ~/.claude/settings.json is hitting that read-only store symlink, not a permissions or disk fault. The fix is an edit to home.nix in the dotfiles repo followed by hms, never a chmod."
    "Config is applied with the hms wrapper, which pushes, waits for the commit to build in CI on forge.ko.ag, then switches. Its refusals on a dirty tree or on a checkout behind origin/master are intended; a bare home-manager switch is the wrong way around them."
    "The dotfiles repo is public and single-maintainer, worked directly on master. Committing and pushing there is routine and needs no PR or review gate."
    "${config.home.homeDirectory}/devel/kube is a personal single-maintainer GitOps tree where direct pushes to main are the intended workflow."
    "Repos under github.com/modular, github.com/modularml and github.com/bentoml are shared work repos: changes there go through a branch and a PR, never a direct push to the default branch."
    "Secrets are sops-encrypted in the dotfiles repo and decrypted at activation. Passing one to a command by reading its file inline is the normal pattern here; printing one into the terminal or into a file is not."
    "Per-project toolchains come from mise, direnv and nix develop, so a missing-command failure usually means the command belongs under mise run or nix develop rather than a global install."
    "A SessionStart hook gives each Claude session a copy of ~/.kube/config with every context matching prod deleted and current-context unset, so kubectl in this session has no production cluster to reach."
    "That hook does not revoke the underlying SSO credential, so re-running an SSO login or writing a fresh kubeconfig could restore production reach. Those are worth a prompt rather than an auto-approval."
  ];


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
    # (rendered from these settings via programs.claude-code below), which is
    # the only path its SETTINGS_PATHS looks at.
    autoMode = {
      allow = [
        "$defaults"
        # PR creation auto-approves inside quite-app only. Nothing denies it
        # elsewhere any more — it just isn't auto-mode work, so it surfaces a
        # prompt, which is what CLAUDE.md's confirm-before-a-PR rule wants.
        "Creating a pull request (`gh pr create`, a `gh api` POST to a repo's pulls endpoint, or mcp__github__create_pull_request) is ALLOWED when the working directory is under ${config.home.homeDirectory}/devel/quite-app."
        "Git Push to Default Branch is allowed when the current working directory is under ${config.home.homeDirectory}/devel/kube. That repo is a personal single-maintainer GitOps tree where direct pushes to main are the intended workflow; no PR review applies."
        # drovr worktrees live at <repo>/.drovr/wt/<run>, i.e. inside the repo the
        # session is already working in, so a `cd` there is navigation within the
        # project rather than an escape from it. Expressing this as a permissions
        # .allow rule isn't possible: Bash rules are exact-or-`:*`-prefix, so the
        # repo name can't be wildcarded mid-path and the only rule that would
        # match is `Bash(cd:*)`, which allows every cd anywhere.
        "Changing directory into a drovr worktree is ALLOWED: a `cd` whose target path contains `/.drovr/wt/` (for example `cd ${config.home.homeDirectory}/devel/dotfiles/.drovr/wt/some-run`). Judge any command chained after the `cd` on its own merits — this rule covers the directory change only."
      ];
      environment = claudeAutoModeEnvShared ++ private.claudeAutoModeEnvByHost;
    };
    # Declare marketplaces here instead of shelling out to `claude plugin
    # marketplace add` at activation: Claude Code registers every entry into
    # <config dir>/plugins/known_marketplaces.json on startup, overwriting a
    # stale same-name entry from this source. This makes the drovr pin
    # self-correcting on the first launch after a flake.lock bump, rather than
    # only for the ambient CLAUDE_CONFIG_DIR during the switch. See
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
    # unifi `cat`s its key at launch rather than degrading without it, so it is
    # registered only where sops writes that key (see isSecretsHost).
    mcpServers = lib.optionalAttrs isSecretsHost {
      unifi = {
        type = "stdio";
        command = "sh";
        args = [
          "-c"
          "UNIFI_API_KEY=$(cat ${config.home.homeDirectory}/.config/unifi/api-key) exec ${config.home.homeDirectory}/.local/share/mise/shims/uvx unifi-mcp-server"
        ];
        env = {
          UNIFI_API_TYPE = "local";
          UNIFI_LOCAL_HOST = private.endpoints.unifiHost;
          UNIFI_LOCAL_VERIFY_SSL = "false";
        };
      };
    } // {
      explore-mcp = {
        type = "stdio";
        command = "${explore-mcp-pkg}/bin/explore-mcp";
        env = {
          EXPLORE_MCP_CONFIG = "${config.home.homeDirectory}/.config/explore-mcp/config.json";
        };
      };
    };
    model = "claude-opus-5";
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

  newtabLinks = private.newtabLinks;

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
  imports = [ sops-nix.homeManagerModules.sops walker.homeManagerModules.default zen-browser.homeModules.default ./antigravity.nix ./opencode.nix ./pi.nix ./cursor-agent.nix ./kimi-code.nix ./even-terminal.nix ./mcode.nix ];

  home.stateVersion = "26.05";

  # ── sops-nix ────────────────────────────────────────────────────────────────
  sops.defaultSopsFile = ./secrets.yaml;
  # Decryption is GCP KMS (see .sops.yaml + GOOGLE_APPLICATION_CREDENTIALS below).
  # sops-nix still asserts *some* age/gpg key source, and sops-install-secrets
  # opens the configured keyFile at runtime — so declare an empty managed file
  # to satisfy both.
  home.file.".config/sops/age-unused.txt" = lib.mkIf isSecretsHost { text = ""; };
  sops.age.keyFile = "${config.home.homeDirectory}/.config/sops/age-unused.txt";
  sops.age.sshKeyPaths = [];
  sops.gnupg.sshKeyPaths = [];
  sops.environment = lib.mkIf isSecretsHost (
    if useWif then {
      GOOGLE_APPLICATION_CREDENTIALS = "${wifCredentialConfig}";
      # Required by Google's auth library for executable-sourced credentials. The
      # config above is a read-only store path, so this is a constraint, not a risk.
      GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES = "1";
    } else {
      GOOGLE_APPLICATION_CREDENTIALS = "${config.home.homeDirectory}/.config/sops/gcp-key.json";
    });

  # Every secret is behind isSecretsHost, as one set: a single one declared for
  # a host outside the fleet switches the whole sops-nix module back on there.
  sops.secrets = lib.mkIf isSecretsHost {
    # ── Modular API (local auto-mode classifier) ────────────────────────────────
    modularApiKey = {
      path = "${config.home.homeDirectory}/.config/local-auto-mode/api-key";
      mode = "0600";
    };

    # ── ko.ag API (opencode provider + local-auto-mode classifier) ─────────────
    # This is now litellm's MASTER KEY, not the old CF AI Gateway token: ai.ko.ag
    # was deleted and both consumers dial the router's LAN address directly. The
    # same value must exist in the cluster as the `litellm-master-key` Secret in
    # the litellm / hakobiya / opencode namespaces — rotating here without
    # rotating there 401s everything. See the kube repo's docs/litellm-access.md.
    koAgApiKey = {
      path = "${config.home.homeDirectory}/.config/opencode/ko-ag-key";
      mode = "0600";
    };

    # ── Z.AI API (opencode zai / zai-coding-plan providers) ───────────────────
    zaiApiKey = {
      path = "${config.home.homeDirectory}/.config/opencode/zai-key";
      mode = "0600";
    };

    # ── Modular private endpoint base URL (opencode mcloud provider) ────────────
    # Kept in sops so the internal hostname never lands in the committed config.
    modularApiUrl = {
      path = "${config.home.homeDirectory}/.config/opencode/mcloud-base-url";
      mode = "0600";
    };

    # ── UniFi API key (unifi-mcp-server) ───────────────────────────────────────
    unifiApiKey = {
      path = "${config.home.homeDirectory}/.config/unifi/api-key";
      mode = "0600";
    };
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
  # outside a herdr pane.
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

  # ── herdr integration (mcode) ─────────────────────────────────────────────
  # Hand-rolled — herdr 0.9.1 has no built-in mcode integration, and `mcode`
  # (MiniMax Code, npm `@minimax-ai/code`) is the only MiniMax-shaped TUI in
  # this repo that ships an extension surface herdr does not already cover.
  #
  # Layout: mcode's `local` marketplace is `~/.minimax/plugins/`. Dropping a
  # directory with `.claude-plugin/plugin.json` plus a matching
  # `hooks/hooks.json` there auto-installs and enables it on the next
  # `mcode plugin list` — no `mcode plugin add` step. mcode reads the
  # generated plugin via its `CLAUDE_CODE` loader path (the `defaultPath`
  # for that source format) and emits the hook wanting Claude-Code-shaped
  # JSON on stdin, with `$CLAUDE_PLUGIN_ROOT` substituted into the
  # `command` field. Verified by running `mcode exec` against this exact
  # payload — SessionStart / UserPromptSubmit / PreToolUse / PostToolUse /
  # Stop / Notification all fire as expected.
  #
  # The script is the herdr-protocol-emitting half (mirrors cursor's pattern,
  # which is shorter than claude's and the closest non-claude analog here);
  # like all four other vendored integrations, it no-ops unless HERDR_ENV=1.
  #
  # Why this is an activation step and not three `home.file` entries:
  # `home.file` produces symlinks under $HOME that point into /nix/store,
  # and mcode 0.6.2's local-marketplace scanner enumerates entries with
  # lstat semantics — readdir() of `~/.minimax/plugins/`, then a check
  # that rejects anything not seen as a regular file. The symlink target
  # is a real file in the store, but the entry at the marketplace root
  # is LNK, not REG, so the plugin is invisible: `mcode plugin list`
  # shows zero local plugins and `mcode exec` fires no hooks. Verified
  # empirically by mirroring this layout into a fresh $MINIMAX_DATA_DIR
  # where the symlinked entry does not appear and the real-file entry
  # does (`herdr-agent-state@local enabled`).
  #
  # Materialising each file via `install(1)` at activation time gives
  # mcode the file types its scanner asks for, and is idempotent: a
  # subsequent `hms` re-installs the same content. Three files only,
  # sized 494B / 1530B / 3749B — cheap in time and disk, no use in
  # extending `home.file` because Nix offers no "link-as-regular"
  # option for it.
  home.activation.herdrAgentStateMcode = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir -p \
      "$HOME/.minimax/plugins/herdr-agent-state/.claude-plugin" \
      "$HOME/.minimax/plugins/herdr-agent-state/hooks"
    $DRY_RUN_CMD ${pkgs.coreutils}/bin/install -m 0644 \
      ${./home/.minimax/plugins/herdr-agent-state/.claude-plugin/plugin.json} \
      "$HOME/.minimax/plugins/herdr-agent-state/.claude-plugin/plugin.json"
    $DRY_RUN_CMD ${pkgs.coreutils}/bin/install -m 0644 \
      ${./home/.minimax/plugins/herdr-agent-state/hooks/hooks.json} \
      "$HOME/.minimax/plugins/herdr-agent-state/hooks/hooks.json"
    $DRY_RUN_CMD ${pkgs.coreutils}/bin/install -m 0755 \
      ${./home/.minimax/plugins/herdr-agent-state/hooks/herdr-agent-state.sh} \
      "$HOME/.minimax/plugins/herdr-agent-state/hooks/herdr-agent-state.sh"
  '';

  # ── Claude plugins ─────────────────────────────────────────────────────────
  home.file.".claude/plugins/local-auto-mode/hooks.json".source =
    ./home/.claude/plugins/local-auto-mode/hooks.json;
  home.file.".claude/plugins/local-auto-mode/classifier.py".source =
    ./home/.claude/plugins/local-auto-mode/classifier.py;
  home.file.".claude/plugins/local-auto-mode/prompt.py".source =
    ./home/.claude/plugins/local-auto-mode/prompt.py;
  # Substituted rather than copied: the router's LAN address comes from the
  # private input, so the checked-in file carries a placeholder.
  home.file.".claude/plugins/local-auto-mode/config.py".text =
    builtins.replaceStrings [ "@LOCAL_CLASSIFIER_URL@" ] [ private.endpoints.localClassifierUrl ]
      (builtins.readFile ./home/.claude/plugins/local-auto-mode/config.py);

  # settings.local.json is deliberately NOT nix-managed. Leaving it unmanaged
  # keeps it writable for Claude Code's own user-scope "don't ask again" saves,
  # which a read-only store symlink silently broke. The autoMode rules it once
  # carried live in `claudeBaseSettings` above instead.

  # Gecko 67+ keys profile-per-install via [Install<HASH>] sections in
  # profiles.ini (gated by `Version=2`), overriding `Default=1`. Every nix
  # zen bump makes a new install hash, so Zen creates a fresh
  # *.default-release profile and pins it, ignoring the home-manager one.
  # Dropping Version= makes Zen honor Default=1 (like Darwin); rm the legacy
  # installs.ini backup so it can't re-seed the Install section on next launch.
  # Unprefixed path: zen-browser's configPath is absolute (firefox's was
  # home-relative), so do not put $HOME in front of it.
  home.activation.zenInstallsIni = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD rm -f "${config.programs.zen-browser.configPath}/installs.ini"
  '';

  xdg.userDirs.setSessionVariables = true;

  # getEnv is "" under pure eval, so these always take the fallback; hosts whose
  # $USER/$HOME differ declare it in their `machine` attrset.
  home.username =
    machine.username or (let v = builtins.getEnv "USER"; in if v != "" then v else "sauyon");
  home.homeDirectory =
    machine.homeDir or (
      let v = builtins.getEnv "HOME";
      in if v != "" then v else (if isDarwin then "/Users/sauyon" else "/home/sauyon")
    );

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
    # The non-dconf half of hidpi.viaDconf: a scaled host without a dconf D-Bus
    # service cannot use text-scaling-factor, so it gets GDK_DPI_SCALE instead.
    // (lib.optionalAttrs (hidpi.enabled && !hidpi.viaDconf) {
      GDK_DPI_SCALE = toString hidpi.scale;
    })
    # Steam's desktop UI is CEF, and X11. force_zero_scaling hands it the panel's
    # real pixels with no DPI hint, and it reads neither Xft.dpi nor GDK_DPI_SCALE,
    # so on a laptopScale host it draws at 1/laptopScale of physical size. This is
    # the one factor it does read, and CEF re-lays-out at it instead of upscaling a
    # bitmap -- so the client comes back to size and stays crisp, rather than
    # trading the force_zero_scaling win away for the whole of XWayland.
    #
    # An env var and not -forcedesktopscaling on the .desktop Exec: the tray's
    # "restart Steam" re-execs, and steam:// handler launches never see that argv.
    # Gated on laptopScale for the same reason as the rest of this block -- a host
    # that scales apps instead would multiply the two.
    // (lib.optionalAttrs (laptopScale != 1) {
      STEAM_FORCE_DESKTOPUI_SCALING = toString laptopScale;
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

  # Rewrite the packaged steam.desktop's Exec= to the `steam` wrapper, so a launch
  # from elephant/walker -- and a steam:// link handed over by xdg-open -- goes
  # through it rather than straight to /usr/bin/steam. Same destination directory,
  # and for the same reason, as Zoom.desktop just below: ~/.local/share beats
  # /usr/share, where hm's xdg.desktopEntries (~/.nix-profile/share) loses to it.
  #
  # An activation entry rather than a declarative file because the content is
  # derived from the pacman file at switch time -- see home/steam-desktop-override
  # for why this repo does not keep its own copy of those 282 lines. Re-runs on
  # every `hms`, which is also what picks up a steam.desktop changed by a package
  # update. Nothing re-runs update-desktop-database: the override keeps the filename
  # steam.desktop, so an existing mimeinfo.cache entry for x-scheme-handler/steam
  # still names a file that exists, and XDG_DATA_HOME's precedence picks ours.
  #
  # `|| warnEcho` and not a bare call, which is the whole failure policy. The
  # generated `activate` runs under `set -eu` (verified: line 2 of a built
  # generation), and this entry sorts after linkGeneration and reloadSystemd but
  # BEFORE zenInstallsIni. So a non-zero exit here would not merely skip the
  # launcher fix -- it would abort the rest of activation, leaving the profile
  # already pointing at the new generation while gcroots/current-home still names
  # the old one, and leaving installs.ini in place for Zen to re-pin. The degraded
  # outcome this buys instead is "Steam launches unwrapped", which is recoverable
  # and strictly smaller. Same call system/deploy already makes for the same reason:
  # `./thermald-setup || echo ... continuing`.
  #
  # entryAfter [ "linkGeneration" ] rather than the bare write boundary, matching
  # antigravity.nix: this writes into a directory hm also links into, and relying on
  # attribute order to land after linkGeneration is an accident rather than an edge.
  #
  # Guarded like the Zoom.desktop sibling below rather than left unconditional: the
  # script's own gate would make it a no-op on Darwin anyway, but the guard keeps a
  # Linux-shaped wrapper out of the Darwin host's closure.
  home.activation.steamDesktopOverride = lib.mkIf (!isDarwin && isDesktop) (
    lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      $DRY_RUN_CMD ${pkgs.bash}/bin/bash ${steam-desktop-override} \
        /usr/share/applications/steam.desktop \
        "${config.xdg.dataHome}/applications/steam.desktop" \
        "${steam}/bin/steam" \
        || warnEcho "steamDesktopOverride: failed; Steam will launch unwrapped"
    ''
  );

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
  # serve_host: the review server has NO auth, so whatever can reach it reads
  # every run and can answer its asks. Bind it where something else already does
  # the authentication -- the tailnet -- so nothing here has to.
  #
  # A LAN bind was tried and reverted, and the reason is worth keeping.
  # `serve_host` is a single string, so "tailnet and LAN at once" means 0.0.0.0,
  # and on a Kubernetes node that also publishes the server to every pod on the
  # box. The nftables allowlist written to hold that back did work -- and was
  # still the wrong answer, because it made network location stand in for
  # authentication. It also had to enumerate the OTHER cluster nodes by IP:
  # cilium masquerades pod traffic to the node address, so a pod on meiko arrived
  # as that node's own address, inside any sane "my LAN" range. That list would have had to
  # stay complete forever, and adding a node would have reopened the hole in
  # silence. Deleted, along with the reason to need it.
  #
  # If a device that cannot run Tailscale ever needs this, the answer is drovr's
  # `serve_public_host` behind a proxy that actually authenticates -- not a wider
  # bind and another allowlist.
  # worktree: every run gets .drovr/wt/<run> on its own branch, so a run in
  # flight leaves the invoking checkout free. The default was off, and the cost
  # showed up as an agent editing main while the tree moved under it: reads went
  # stale, HEAD advanced past the commit under review, and a test count was
  # reported from a tree that no longer existed. `--no-worktree` per run.
  # force: drovr rewrites this path itself — it replaces the symlink with a real
  # file and reserializes only the keys it knows, which is how `worktree = true`
  # went missing on 2026-09-01 and how switch started aborting at
  # checkLinkTargets. Nothing in here is a secret or runtime state, so let the
  # generation win every time rather than hand-deleting the file each switch.
  # Note the guarantee is per-switch, not continuous: force only fixes the abort
  # and restores this content at activation. Between switches drovr still owns
  # the file, so a key-dropping rewrite can silently disable worktree isolation
  # again — with exactly the stale-read failure mode described just above — until
  # the next switch puts it back.
  xdg.configFile."drovr/config.toml" = {
    force = true;
    text = ''
      # pi, not opencode. pi's findings channel is a generated TypeScript
      # extension rather than an MCP server (pi ships no MCP client), and drovr
      # writes it per review pass — see cli/src/pi_extension.rs upstream.
      #
      review_agent = "pi"
      worktree = true
    '' + lib.optionalString (hostname == "fujiwara") ''
      # The TAILNET address, not 0.0.0.0. `0cce01d` deleted the nftables allowlist
      # that made an all-interfaces bind survivable, so the two changes have to
      # move together: binding every interface with the allowlist gone would put
      # an unauthenticated server on the LAN.
      serve_host = "100.94.172.21"
    '' + lib.optionalString (hostname == "utsuho") ''
      serve_host = "100.71.58.39"
    '' + ''
      # Reviews run on gemma, pinned explicitly rather than left to pi's own
      # default so this file says which model reviewed. LAST in the file because
      # a TOML table swallows every key after it — the top-level settings above
      # have to be emitted first.
      #
      # gemma returns a panel in seconds rather than minutes. That is the model
      # being fast, not a reviewer skipping the diff — read the findings, not
      # the wall clock. The delivery floor that used to reject exactly this is
      # gone (drovr f016dcc); it could not tell "did not read" from "reads
      # quickly" and so refused every review on a fast backend.
      #
      # glm-5.3 was tried instead and does not DELIVER: measured 2026-09-02 on
      # a 278-line diff, four fresh iterations, sixteen reviewers, every one
      # provably started (pi extension loaded, seed delivered), worked 8-25
      # minutes, then exited without ever calling `submit_findings`. 0 of 4
      # angles, four times running.
      #
      # Only the REVIEWER is pinned here; interactive pi takes the same gemma
      # from pi.nix as its default.
      [agents.pi]
      command = "pi"
      review_model = "google/gemma-4-31b-it"
    '';
  };

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

      # Parsed rather than matched positionally: the old `[ "$1" != --no-exit ]`
      # sent every typo (--noexit) down the exit path, which now has teeth.
      noexit=no; force=no
      for a in "$@"; do
        case "$a" in
          --no-exit) noexit=yes ;;
          --force)   force=yes ;;
          *) echo "hyprland-graceful-exit: unknown argument: $a" >&2; exit 2 ;;
        esac
      done

      # The config backend is Lua, so `hyprctl dispatch` evaluates its argument
      # as `hl.dispatch(<expr>)` -- the legacy word syntax ("closewindow
      # address:0x...") is a Lua parse error, exits 7, and a bare `|| true`
      # swallowed it. Same migration as the focus bind in hyprland.nix.
      #
      # Every hyprctl call that gathers state is `|| <default>`: under
      # `pipefail` a dead compositor or a stale socket would otherwise abort
      # the script here, before the refusal branch and before the
      # notification -- and from a keybind that notification is the only thing
      # the user ever sees. The final exit dispatch is deliberately bare: if
      # that one fails, the nonzero status is the right outcome.
      addrs=$(hyprctl clients -j | ${pkgs.jq}/bin/jq -r '.[].address') || addrs=
      # Split out of the pipeline so `|| addrs=` can catch a dead compositor
      # under `pipefail` -- that split is what removes the abort, not the
      # subshell it also happens to drop. Fed by here-string rather than
      # re-piping so `read -r` still handles the lines: no IFS splitting, no
      # globbing. An empty $addrs still yields one empty read, hence the guard.
      while read -r addr; do
        [ -n "$addr" ] || continue
        hyprctl dispatch "hl.dsp.window.close({ window = [[address:$addr]] })" \
          || echo "hyprland-graceful-exit: close dispatch failed for $addr" >&2
      done <<< "$addrs"

      # Seeded before the loop, and re-clamped inside it: `for i in $(seq ...)`
      # runs zero times if seq is missing, and an empty `hyprctl clients`
      # leaves jq printing nothing -- either way `set -u` would abort on $count
      # below, which is exactly where the notification is the only output.
      # Every degenerate value therefore fails toward refusing, never toward
      # exiting, and count_known keeps the message honest about which it was.
      count=1; count_known=no
      for i in $(seq 1 10); do
        count=$(hyprctl clients -j | ${pkgs.jq}/bin/jq 'length') || count=
        case "$count" in
          ""|*[!0-9]*) count=1; count_known=no ;;
          *)           count_known=yes ;;
        esac
        [ "$count_known" = yes ] && [ "$count" -eq 0 ] && break
        sleep 0.5
      done

      if [ "$count_known" = yes ]; then
        headline="$count window(s) refused to close"
      else
        headline="could not tell whether any windows are still open"
      fi

      if [ "$noexit" = yes ]; then
        # ExecStop path: say so, but never fail the unit during shutdown. The
        # session is going down regardless, so refusing achieves nothing.
        if [ "$count" -ne 0 ]; then
          echo "hyprland-graceful-exit: $headline" >&2
        fi
        exit 0
      fi

      # A dispatch that selects nothing still exits 0 -- Hyprland validates the
      # dispatcher path, not the selector -- so the window count is the only
      # honest evidence the closes landed. Without this the script would exit
      # the session with windows still open, discarding unsaved work, which is
      # the one thing "graceful" is supposed to prevent. Ghostty triggers it on
      # its own: a surface with a live process puts up a close confirmation,
      # which makes refusing the common path and --force the way out.
      if [ "$count" -ne 0 ] && [ "$force" = no ]; then
        stuck=
        if [ "$count_known" = yes ]; then
          stuck=$(hyprctl clients -j \
            | ${pkgs.jq}/bin/jq -r '.[:5][] | "  \(.class): \(.title)"') || stuck=
        fi
        echo "hyprland-graceful-exit: $headline; not exiting" >&2
        [ -n "$stuck" ] && echo "$stuck" >&2
        # stderr from a keybind lands in the Hyprland log, where nobody looks.
        #
        # The next three lines are flush left ON PURPOSE: they sit at this
        # block's minimum indentation, which is what Nix strips, so they reach
        # notify-send at column 0. Indenting them to match their neighbours
        # would put literal spaces in the message the user reads.
        if command -v notify-send >/dev/null 2>&1; then
          notify-send -u critical "Hyprland exit cancelled" \
            "$headline. To exit anyway, run
      hyprland-graceful-exit --force

      $stuck" || true
        fi
        exit 1
      fi

      hyprctl dispatch "hl.dsp.exit()"
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

  # The polkit authentication agent. polkit has no prompt of its own: with no
  # agent registered it refuses every auth_self/auth_admin action outright --
  # no dialog, nothing in polkitd's journal, just a bare not-authorized. So this
  # unit is what makes two things in this repo reachable at all:
  #   - `fprintd-enroll` on a new box, since net.reactivated.fprint.device.enroll
  #     defaults to auth_self_keep (docs/new-host.md's enrollment step).
  #   - Bitwarden's "unlock with system authentication", which authorizes
  #     com.bitwarden.Bitwarden.unlock as auth_self. That action is the whole
  #     reason system/etc/pam.d/polkit-1 puts pam_fprintd ahead of system-auth;
  #     without an agent that stack is never reached, so the fingerprint wiring
  #     reads as broken when it is only unreachable.
  # `pkexec` is the exception that hid this for so long -- it carries its own
  # text agent, so it kept prompting from a TTY while everything going through
  # the bus did not.
  #
  # Fields mirror upstream's own share/systemd/user/hyprpolkitagent.service
  # rather than being invented here; we define the unit instead of installing
  # that file because standalone home-manager's units come from this attrset,
  # and a copied unit gets no graphical-session.target.wants symlink.
  # withHostNss is load-bearing, not boilerplate: the agent calls getpwuid on
  # the user it is authenticating, and sauyon is homed-only with no /etc/passwd
  # entry, so an unwrapped binary cannot name whose password it is asking for.
  # The upstream binary sits in libexec/, which withHostNss wraps alongside bin/.
  systemd.user.services.hyprpolkitagent = lib.mkIf (!isDarwin && isDesktop) {
    Unit = {
      Description = "Hyprland Polkit Authentication Agent";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
      # Keeps activation outside a Wayland session (a plain ssh login) a no-op
      # rather than leaving a failed unit behind. The hyprland start hook imports
      # WAYLAND_DISPLAY into the user manager's environment before it starts
      # hyprland-session.target, so the ordering holds on this host -- but when it
      # does not (a session brought up by some other path), the unit is *skipped*,
      # not failed: `systemctl --user status hyprpolkitagent` reads
      # `inactive (dead)`, Restart= never applies, and nothing retries later. That
      # reads identically to having no agent at all, so inactive-not-failed is the
      # tell when auth_self prompts go missing again.
      ConditionEnvironment = "WAYLAND_DISPLAY";
      # RestartSec below fights the start limiter, and without this it wins.
      # systemd's defaults here are burst 5 over a 10s window; a 5s delay fits
      # only three attempts into 10s, so the limiter never trips and a
      # *permanently* broken agent restarts every five seconds forever while
      # `systemctl --user status` reads `active (running)` for most of any sample.
      # That is the same false-healthy reading that let the EGL crash below ship
      # in the first place, so widen the window until five failures (~25s at
      # RestartSec=5) do stick and the unit lands in `failed` where it is visible.
      # The three values are one decision: change one and redo the arithmetic.
      StartLimitIntervalSec = "60s";
      StartLimitBurst = 5;
    };
    Service = {
      # nixGL is as load-bearing as withHostNss, and fails later and more
      # confusingly. The agent builds its Qt Quick dialog only once a challenge
      # arrives, and nix-built Qt resolves libEGL/GBM/DRI out of the store, which
      # carries no driver for this GPU. Unwrapped, the unit starts clean and stays
      # up until the first prompt, then logs "EGL not available" / "Failed to
      # initialize graphics backend for OpenGL" and SIGABRTs; polkitd records the
      # operator as having FAILED to authenticate, so the caller gets the same
      # bare PermissionDenied as with no agent at all, and RestartSec brings it
      # back looking healthy. That is how this was shipped once already, so when
      # prompts go missing the check is `coredumpctl list hyprpolkitagent`, not
      # `systemctl --user status`, which reads `active (running)` either way.
      # Note that `active` here only ever means bash fork+exec'd -- a polkit agent
      # owns no bus name, so there is no Type=dbus or sd_notify to make it mean
      # "registered with polkitd".
      #
      # config.lib.nixGL.wrap (hyprlock, ghostty, hyprpaper) only rewrites bin/,
      # and this package ships its binary in libexec/, so the chain is spelled out
      # here instead. Nesting order does *not* matter: nixGL assigns
      # LD_LIBRARY_PATH while preserving what it finds, the inner wrapper --prefixes
      # onto it, and neither set of dirs ships the other's libraries -- so both
      # reach the real binary either way. nixGL is outermost only because something
      # has to be the ExecStart binary. (The hyprlock comment below states the
      # opposite order as though it were required; it is not, and neither is this.)
      #
      # One string, not a list, deliberately: systemd splits an ExecStart string on
      # whitespace into argv, whereas `[ a b ]` would emit two ExecStart= lines and
      # run them in sequence. The test pins this by rejecting a second entry.
      ExecStart =
        "${nixGL}/bin/nixGL ${withHostNss pkgs.hyprpolkitagent}/libexec/hyprpolkitagent";
      Slice = "session.slice";
      TimeoutStopSec = "5sec";
      Restart = "on-failure";
      # Upstream omits this and so inherits systemd's 100ms default, which is a
      # trap here rather than a preference: with DefaultStartLimitBurst=5 over a
      # 10s interval, five attempts fit inside half a second and the unit is then
      # marked failed for the rest of the session -- nothing retries, nothing
      # notifies, and the symptom is the exact silence this unit exists to
      # remove. Five seconds spreads the same budget over ~25s, which is enough
      # to outlast the things that actually fail transiently here: the Qt wayland
      # plugin while the compositor is still settling, and a
      # RegisterAuthenticationAgent collision during the start hook's
      # stop-then-start of hyprland-session.target. Matches psi-notify above.
      RestartSec = 5;
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

  # `clp-rc` == `claude remote-control`: a persistent server
  # letting claude.ai/code and the Claude mobile app drive local sessions in a
  # project. Template unit keyed on the project path so any number can run
  # concurrently and start on the fly (see clp-rc/clp-rc-stop in zsh.nix):
  #   systemctl --user start claude-remote-control@$(systemd-escape -p /path/to/proj)
  # %I unescapes back to the absolute project path for WorkingDirectory. Verified
  # headless: claude bundles its own node, connects with stdin=null and no TTY,
  # and shuts down gracefully on SIGTERM. RC refuses to start in an untrusted
  # workspace, so clp-rc pre-accepts the trust dialog in ~/.claude.json.
  systemd.user.services."claude-remote-control@" = lib.mkIf (!isDarwin) {
    Unit = {
      Description = "Claude Code Remote Control — %I";
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
      ExecStart = "${config.home.profileDirectory}/bin/claude remote-control --spawn worktree";
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
    # Unpinned: the fork's focus-steal fixes are not in v0.9.1, but upstream
    # #1621 closed COMPLETED and the pin no longer builds under zig 0.16.
    pkgs.herdr
    hms
    hmeval
    kcs
  ]
  # Enrolment/recovery tool for the TPM-sealed keyring passphrase; the daemon
  # wrapper itself is referenced straight from its unit, so it stays off PATH.
  ++ lib.optional gnomeKeyringHost gnome-keyring-tpm-seal
  # paru, the AUR helper, on the Arch hosts only -- a pacman frontend in a
  # profile with no pacman under it (kyuusaku, mari) is a tool that evaluates and
  # builds fine and fails the moment anyone runs it.
  #
  # Why it is HERE and not in system/packages, given that what it installs is
  # host state: that list ends in `pacman -S`, and paru is itself an AUR package,
  # in no pacman repo. A `paru` line there would be reported missing on every
  # host on every deploy and answered with "target not found" -- which reads as a
  # typo in the list rather than as "this is not that kind of package". Nothing
  # else about it wants root-side ownership: it reaches host state only by
  # invoking the host's own pacman under sudo, the way a human does.
  #
  # Why nixpkgs' paru and not the AUR's paru-bin, which is the usual way in: this
  # way the helper arrives from the attic cache like everything else here, with no
  # makepkg run and no PKGBUILD to trust at bootstrap. The AUR trust surface then
  # covers only the packages actually wanted from the AUR, rather than including
  # the tool that fetches them.
  #
  # The real cost, stated because it is a genuine one and it is not zero: paru
  # links libalpm, and this paru carries nixpkgs' copy while the `pacman` it
  # shells out to for repo work is the host's. They agree today -- `paru
  # --version` prints the libalpm it loaded (16.0.1) and `pacman -Qi pacman` the
  # one the host provides (libalpm.so=16, 16.0.1) -- and nixpkgs' half is doing
  # queries and dependency resolution, not writing the db. If nixpkgs and Arch
  # ever straddle a libalpm soname bump, re-check that pair before assuming this
  # still holds.
  ++ lib.optional isArchHost pkgs.paru
  ++ (with pkgs; [
    bfs
    btopPkg

    # The full google-fonts is ~2,000 families and a 2.44 GiB output — the single
    # largest path in every closure, and the one whose NAR used to blow the CI
    # substituter's ten-minute ceiling. Nothing here names a Google font (the UI
    # font is NotoSans Nerd Font, from nerd-fonts.noto); this set exists so web
    # pages and documents find common families rather than falling back.
    #
    # Names are matched against TTF filenames, and a name that matches nothing is
    # silently dropped rather than an error — so verify against
    # `ls $out/share/fonts/truetype` after changing this, do not trust the build
    # going green. (The *build* still fetches the whole google/fonts repo; only
    # the output, which is what gets substituted, shrinks.)
    (google-fonts.override {
      fonts = [
        # sans
        "Roboto" "OpenSans" "Lato" "Montserrat" "Inter" "Poppins" "Nunito"
        "Raleway" "WorkSans" "SourceSans3" "IBMPlexSans" "NotoSans" "DMSans"
        "Rubik" "Figtree" "FiraSans" "Oswald" "Ubuntu"
        # serif
        "Merriweather" "PlayfairDisplay" "SourceSerif4" "NotoSerif" "RobotoSlab"
        # mono
        "RobotoMono" "SourceCodePro" "IBMPlexMono" "NotoSansMono" "JetBrainsMono"
        "FiraCode"
      ];
    })

    # The emoji font, and it has to be its own package. Everything Noto above is
    # text -- NotoSans Nerd Font for the UI, NotoSans/NotoSerif/NotoSansMono in
    # the google-fonts set -- and not one of them carries an emoji glyph, so
    # "we have Noto" is true and emoji still render as tofu. Missing, it fails
    # silently: `fc-match emoji` returns whatever Noto sorts first rather than
    # erroring. Ungated, like google-fonts beside it: ~10 MiB, and a headless
    # host that renders a document wants the glyphs too. fontconfig's own
    # 60-generic.conf binds the `emoji` generic to the family name this ships,
    # so installing it is the whole fix.
    noto-fonts-color-emoji

    claude-agent-acp
    coder
    comma
    cosign
    entire  # git-hook layer checkpointing AI agent sessions alongside commits
    # Even Realities' G2/R1 bridge for a terminal coding agent; see
    # even-terminal.nix. Listed for every host rather than gated on isDesktop:
    # it is a headless HTTP server that the phone connects to, and the glasses
    # are the display -- a host with no graphical session can still serve it.
    even-terminal
    jq
    jujutsu
    kimi-code  # Moonshot's Kimi Code CLI (binary: kimi); see kimi-code.nix
    lnav
    mcode  # MiniMax Code CLI (binary: mcode); see mcode.nix
    mise
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
    cryptomator-cli
    # Beside cryptomator-cli because the two are only useful together here: the
    # vault holding the dotfiles paper recovery identity lives in the personal
    # Drive, and cryptomator-cli's `unlock` mounts a LOCAL directory only. So the
    # vault has to be reachable as a filesystem before it can be unlocked, which
    # is what rclone provides.
    #
    # Deliberate and on record: `rclone config` leaves a Drive refresh token in
    # ~/.config/rclone/rclone.conf. That is the credential class report A6.3 warns
    # about -- the same kind Shai-Hulud wave 1 harvested from ~/.config/gcloud --
    # and shiori is otherwise deliberately bare. Accepted by the human after the
    # trade was stated. Revoke the token at
    # https://myaccount.google.com/permissions when the vault work is done, or
    # keep it scoped to the one Drive path it needs.
    #
    # NEVER write the paper key into the vault's Drive folder directly: the vault
    # is a tree of encrypted blobs, and a file dropped in unencrypted is an
    # unrevocable master backdoor sitting in cleartext in cloud storage. Mount,
    # unlock, then write through the mount.
    pkgs.rclone
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
    # Also on PATH so a lockout can be checked (and waited out) from a TTY.
    hyprlock-faillock
    nixGL

    pkgs.bitwarden-cli
    # Desktop app is the biometric backend the browser extension talks to over
    # native messaging (the extension can't unlock with biometrics on its own).
    # Unaffected by the move off firefox: gecko asks for native manifests under
    # XREUserNativeManifests, which on Linux is a forced-legacy ~/.mozilla for
    # every gecko app no matter where its profile lives — so whatever
    # bitwarden-desktop writes there, once browser integration is switched on in
    # the app, is visible to Zen too, and the wrapper drops tridactyl's manifest
    # in beside it. Unverified in the direction that matters: that directory
    # holds only home-manager's .keep and tridactyl.json today, so no
    # bitwarden-written manifest has been observed there under either browser.
    # Pairs with the polkit action + pam_fprintd wiring in system/.
    pkgs.bitwarden-desktop
    # gvfs must be *installed*, not just referenced by store path the way most
    # things here are: GIO reaches its udisks2 volume monitor over D-Bus, and
    # dbus-broker only activates names whose .service files sit in a directory
    # it already indexes (~/.nix-profile/share/dbus-1/services). Pointing
    # XDG_DATA_DIRS at the store path instead fails at runtime with "The name
    # is not activatable" -- the broker built its index at session start and a
    # later env var cannot retroactively add to it. Pairs with
    # GIO_EXTRA_MODULES in env.nix; the module alone finds the four monitors
    # but cannot start them.
    pkgs.gvfs
    pkgs.hyprpicker
    pkgs.psi-notify
    pkgs.pwvucontrol
    pkgs.slack
    # Discord client. Was dropped while its build pulled pnpm-10.29.2, which
    # nixpkgs marks insecure (CVE-2026-48995, CVE-2026-50014). Under the current
    # flake.lock it is vesktop 1.6.7 built with pnpm-11.27.0, which nixpkgs does
    # not flag, so it needs no permittedInsecurePackages entry --
    # tests/insecure-packages.sh is what keeps that true across a lock bump.
    pkgs.vesktop
    # Keymap editor for the Svalboard (keyboards/svalboard/). Needs the hidraw
    # udev rule in system/etc/udev/rules.d/92-vial.rules to see the keyboard at
    # all -- deployed separately, this package alone is not enough.
    pkgs.vial
    zoom # wayland wrapper bypassing Zoom's xcb-forcing launcher; see above
    pkgs.xauth
    pkgs.xdg-utils
  ]
  # waypipe, the Wayland equivalent of `ssh -X`: `waypipe ssh utsuho <app>` runs
  # the application on the far host and the surface on this one, over the ssh
  # channel.
  #
  # Why a two-host list and not isDesktop. waypipe is never installed for a host,
  # it is installed for a *pair* -- the invocation starts one waypipe next to the
  # compositor that will show the window and a second next to the application,
  # and neither half is any use without the other. shiori (display, no GPU worth
  # the name) and utsuho (the amd desktop) are the pair that has a reason to
  # forward; setsuna and mari would run it fine and have nothing to point it at,
  # and it is not free -- this build links ffmpeg, vulkan-loader and mesa's gbm
  # for DMABUF and `--video`, so the closure grows on every host it lands on.
  # Add a host here when it becomes an end, not before.
  #
  # Both ends must be the same waypipe: the 0.10 rewrite (C -> Rust) changed the
  # wire format and waypipe refuses a mismatch rather than negotiating down.
  # Coming from one flake.lock is what makes that hold -- the two ends need
  # identical store paths, not merely a waypipe present on each.
  #
  # The part this package alone does not buy, stated because it is the failure
  # that looks like a missing package: `waypipe ssh` resolves `waypipe` on the far
  # end through the non-interactive `$SHELL -c` sshd hands it, which reads .zshenv
  # and never .zshrc -- the same constraint the MOSH_SERVER_NETWORK_TMOUT note in
  # zsh.nix describes. It resolves today because home-manager emits its
  # hm-session-vars.sh source line into .zshenv under `if [[ ! -o login ]]`, and
  # that file puts ~/.nix-profile/bin first on PATH. So a far end that says
  # "command not found: waypipe" with the package plainly installed is a .zshenv
  # problem, not this gate.
  ++ lib.optionals (builtins.elem hostname [ "shiori" "utsuho" ]) [
    # The nixGL-less Vulkan wrap, defined up top; pkgs.waypipe bare cannot do GPU
    # transfers on these hosts.
    waypipe
  ]
  ++ lib.optionals (hostname == "shiori") [
    # work-slack: pull utsuho's Slack onto this display over the WG overlay.
    # Two things the bare `waypipe ssh utsuho slack` gets wrong: Slack is
    # single-instance per session, so a copy running on utsuho's own desktop
    # swallows the launch (the far end exits 0 and nothing forwards) -- quit
    # it first; and Electron's default X11 backend has no server at the far
    # end of a waypipe connection ("Missing X server or $DISPLAY"), while the
    # nixpkgs wrapper's NIXOS_OZONE_WL only adds --ozone-platform-hint=auto,
    # which this Electron still resolves to X11 -- name Wayland explicitly.
    # waypipe splits the remote argv itself, so an env-assignment prefix would
    # be exec'd as the program name.
    (pkgs.writeShellScriptBin "work-slack" ''
      set -euo pipefail
      ssh utsuho 'pkill -x slack || true'
      exec waypipe ssh utsuho slack --ozone-platform=wayland
    '')
  ]
  ++ lib.optionals (hostname == "fujiwara") [
    clawpatrol
  ] ++ lib.optionals (hostname == "shiori") [
    # `framework_tool`, Framework's own utility for talking to the embedded
    # controller. It is the only way to read EC state (fan duty, per-cell battery
    # health, EC/PD firmware versions) and the only way to set a charge limit:
    # Framework exposes no charge_control_end_threshold under /sys, so the
    # kernel-level battery knobs every other laptop has simply are not there.
    #   framework_tool --charge-limit 80       # spare the cells when desk-bound
    #   framework_tool --versions              # BIOS/EC/PD, without rebooting
    # Wants sudo (it drives the EC over the LPC port), which is fine from a nix
    # profile -- so by system/packages' own rule this is a user package, not a
    # host one. shiori-only because it is the only Framework we own; on anything
    # else it would just fail to find an EC.
    pkgs.framework-tool
    # The `steam` wrapper (see its comment up top). In the profile so a bare
    # `steam` from a shell gets it too -- ~/.nix-profile/bin precedes /usr/bin, so
    # this shadows the pacman binary it then execs by absolute path. shiori-only
    # because that is the only host whose package list asks for Steam; elsewhere
    # it would be a `steam` command that only ever fails to find /usr/bin/steam.
    steam
  ];

  # No permittedInsecurePackages entry: nothing here needs one, and
  # tests/insecure-packages.sh holds that rather than a comment a lock bump can
  # quietly falsify -- which is exactly how the entry this replaces went stale.
  # `sandbox` used to be set here too, doing nothing: it is a nix.conf setting,
  # not a nixpkgs config attr, and nixpkgs' freeform config type swallows
  # undeclared attrs without even a warning unless warnUndeclaredOptions is on.
  # Nothing in this repo sets it for these boxes at all; they get nix's
  # compiled-in default, which is on. The one place it is set on purpose is CI,
  # via NIX_CONFIG in both workflows, forcing it back on because the runner image
  # ships sandbox = false and unsandboxed builds fail there.
  nixpkgs.config.allowUnfree = true;

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
      # Named rather than inlined into the override so hyprlock-faillock can point
      # at the faillock.conf this pam reads: it is the pam that enforces the lock
      # screen's lockout, so it is the one whose thresholds the label must report.
      pam-host-chkpwd = prev.pam.overrideAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          substituteInPlace modules/module-meson.build \
            --replace-fail "'/run/wrappers/bin/unix_chkpwd'" "'/usr/bin/unix_chkpwd'"
        '';
      });
      hyprlock = (prev.hyprlock.override {
        pam = final.pam-host-chkpwd;
      }).overrideAttrs (old: {
        patches = (old.patches or []) ++ [
          ./patches/hyprlock-skip-dtors-on-early-fail.patch
          # Both are upstream bugs we carry a workaround for, so both have a
          # test that says when to stop carrying it; this one is guarded by
          # tests/hyprlock-pending-race.sh, which fails once nixpkgs ships a
          # hyprlock that already registers the listener first.
          ./patches/hyprlock-fix-lost-finished-event.patch
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
      # Pin coder to match the RDE server (rde.modular.com runs v2.35.7); the
      # CLI warns on every invocation about a client/server mismatch, in either
      # direction. nixpkgs drifts to both sides of the server — 2.28.6 when this
      # pin was first written, 2.36.6 as of this bump — so the pin stays even
      # when nixpkgs looks newer. Read the server's version off its
      # `/api/v2/buildinfo` endpoint before bumping. The nixpkgs derivation just
      # fetches a prebuilt release tarball, so bumping is a version + per-system
      # hash swap (no Go/frontend rebuild).
      coder = prev.coder.overrideAttrs (old: rec {
        version = "2.35.7";
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
            x86_64-linux = "sha256-w3MnVWTWuMk9FomSPs++e1oXkaKu7eEMEpy4f+hTLJo=";
            aarch64-linux = "sha256-ZN7mEDVmd+QYXU2Y6e1HN2Prg5MG89jpOhtzRdkPYgs=";
            x86_64-darwin = "sha256-aoTJXBSGqJU+YAxiuosMFKfwZGjwndNG4lsqtHNIBwE=";
            aarch64-darwin = "sha256-GeaLUwUd4xIsZ2Ry6FRud3MzvNwovumT7tkTehgy9+c=";
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
      name = "adw-gtk3-dark";
      package = pkgs.adw-gtk3;
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
    # avahi-discover, bssh and bvnc, from Arch's avahi: nothing here uses avahi
    # (transitive dep of passim and pipewire-pulse, daemon and socket disabled),
    # and "Avahi Zeroconf Browser" fuzzy-matches `zen`. Patterns run against the
    # basename with .desktop stripped, so spelling out `\.desktop$` matches
    # nothing. Only the startup walk is filtered; the inotify re-index path
    # ignores the blacklist until elephant restarts.
    elephant.provider.desktopapplications.settings.blacklist = [
      "^avahi-discover$"
      "^bssh$"
      "^bvnc$"
    ];
  };

  # Make a switch that changes the package set restart elephant, so walker sees
  # apps that were added since the session started. Covered by
  # tests/elephant-reindex.sh.
  #
  # elephant walks $XDG_DATA_DIRS/applications once at startup and then relies on
  # inotify. Its watch on ~/.nix-profile/share/applications resolves into the store,
  # and inotify watches inodes: a switch does not rewrite that immutable directory,
  # it builds a new one and repoints the symlink, so no event ever reaches the
  # watch. The index stays frozen at whichever generation was current when the unit
  # last started -- silently, and in both directions. On 2026-09-30 shiori's
  # elephant had been up since 2026-09-27 against a generation ten switches old:
  # `vesktop` returned nothing and a `dev.warp.Warp.desktop` that no longer existed
  # was still offered.
  #
  # config.home.path and not some other store path: it is the collection every
  # desktop entry in the profile comes from, so an entry-set change always changes
  # it. Not the converse -- it is a buildEnv over home.packages, so any version or
  # hash bump of anything in it changes the path too, which means most flake-input
  # bumps fire this trigger without a single .desktop file differing. That is a real
  # cost, not a rounding error: by the restart note below, every such switch bounces
  # walker. Accepted deliberately, because the alternative is hashing the entry set
  # itself, and a launcher blinking on a switch is cheaper than a launcher that
  # silently lies about what is installed.
  #
  # Which store path the profile actually hands elephant is not something to rely
  # on -- under the nix-env layout share/ symlinks into home-manager-path, under
  # nix's own profile format it is a merged `-profile` directory with
  # home-manager-path nowhere in the resolved path -- and the trigger is correct
  # either way. What it does not cover: a `nix profile install` straight into that
  # profile changes the indexed directory with no home.path change and no switch, so
  # the index goes stale again with nothing to fire.
  #
  # The upstream module sets its own X-Restart-Triggers, hashing elephant's settings,
  # which is why a package add never restarted it. Note it lands in [Service] while
  # this one lands in [Unit] -- two separate keys in two sections, not one merged
  # list. [Unit] is where the convention puts it, and sd-switch only diffs unit text
  # in any case.
  #
  # X-SwitchMethod=restart, and it is not decoration. sd-switch's default for a
  # changed unit is a stop followed by a start -- two jobs -- and walker.service
  # carries `Requires=elephant.service`, which systemd propagates. So the first
  # switch to ship this trigger stopped walker along with elephant and then started
  # only elephant: "Stopping units: elephant.service" at 04:55:42, walker "Stopped"
  # the same second, walker left inactive until started by hand.
  #
  # What a restart buys is NOT that walker is left alone. systemd.unit(5) is explicit
  # that Requires= "already stops (or restarts) the configuring unit when a listed
  # unit is explicitly stopped (or restarted)" -- so walker is restarted too, and the
  # journal shows exactly that. The win is that the propagated action is a restart
  # rather than a stop, so walker comes back up on its own instead of staying down.
  #
  # Recording how the first cut got this wrong, because the check looked sound: it
  # was verified by hand with `systemctl --user restart elephant.service`, and walker
  # was `active` afterwards. It was also stopped and started inside that same
  # transaction -- visible in the journal at 03:55:02, invisible to a status check
  # after the fact. Hence the sd-switch --dry-run case in
  # tests/elephant-reindex.sh, which pins the job type rather than a state sampled
  # once the dust settled.
  #
  # Not keep-old, which sd-switch consults before anything else and which would
  # leave the unit untouched however much its text changed -- the trigger inert.
  # That is the right setting for hyprland-cleanup above, whose ExecStop closes
  # every window; it is exactly wrong here, and the two are easy to confuse.
  #
  # walker needs no trigger of its own: the propagation above already restarts it.
  systemd.user.services.elephant = lib.mkIf (!isDarwin && isDesktop) {
    Unit.X-Restart-Triggers = [ config.home.path ];
    Unit.X-SwitchMethod = "restart";
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
  # The other half of glWrapper (see its comment above the nixGL shim). Left at
  # home-manager's "mesa" default this silently ignored machine.gpu, so a
  # proprietary-driver host would have kept the mesa wrapper for every app going
  # through config.lib.nixGL.wrap even after the shim learned to refuse it.
  targets.genericLinux.nixGL.defaultWrapper = lib.mkIf (!isDarwin && isDesktop) glWrapper;

  wayland.windowManager.hyprland = lib.optionalAttrs (!isDarwin && isDesktop) (import ./hyprland.nix { inherit pkgs config edgeGap laptopScale hyprDpmsPhysical; });

  # Replaces the hyprland module's reload hook. With no compositor running,
  # hyprctl 0.56 prints "\n]\n" (no opening bracket) for `instances -j`, and the
  # module pipes that straight into jq — a parse error on every switch that
  # follows a crashed or killed session, since its $XDG_RUNTIME_DIR/hypr stays.
  # mkIf wraps the whole entry, not just onChange: guarding the leaf still creates
  # the "hypr/hyprland.lua" key on hosts without a compositor, where nothing
  # defines its source, and home-manager's file module then fails to evaluate.
  xdg.configFile."hypr/hyprland.lua" = lib.mkIf (!isDarwin && isDesktop) {
    onChange = lib.mkForce (
      let hyprctl = "${config.wayland.windowManager.hyprland.finalPackage}/bin/hyprctl"; in ''
        XDG_RUNTIME_DIR=''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}
        if [[ -d /tmp/hypr || -d "$XDG_RUNTIME_DIR/hypr" ]]; then
          for i in $(${hyprctl} instances -j 2>/dev/null | ${pkgs.jq}/bin/jq -r '.[].instance' 2>/dev/null); do
            ${hyprctl} -i "$i" reload config-only
          done
        fi
      '');
  };

  dconf = {
    # NOT gated on the scaling decision below, which is what 3d6d403 (2026-05-26,
    # "gate dconf.enable on the setsuna hostname") made it and what left every
    # GTK app rendering light on a config that asks for dark. dconf.settings is
    # not only the text-scaling-factor this file declares: home-manager's own
    # gtk3 module writes color-scheme, gtk-theme, icon-theme, cursor-theme,
    # cursor-size and font-name into the same attrset, computed from the gtk.*
    # options set above, on every desktop host.
    # This option gates whether any of it is applied (hm's modules/misc/dconf.nix
    # `config = mkIf (cfg.enable && databases != [])`), so keying it to the one
    # host that scales via dconf discarded six keys to protect one.
    #
    # The symptom was invisible from the usual place to look: gtk-3.0/settings.ini
    # is written by a different code path and stayed correct, reading
    # gtk-application-prefer-dark-theme=true, while the XDG portal — which answers
    # org.freedesktop.appearance color-scheme out of dconf — reported 0, "no
    # preference". Gecko and every other portal-aware toolkit then picks light.
    #
    # Hosts with no dconf D-Bus service are not a reason to gate: hm's activation
    # falls back to `dbus-run-session` when DBUS_SESSION_BUS_ADDRESS is unset, and
    # on a host that never reads the database the keys are inert, not harmful.
    enable = !isDarwin && isDesktop;
    # The scaling half, and the exact complement of the GDK_DPI_SCALE block
    # above — a host must carry `scale` by one mechanism or the other, never both.
    settings = lib.optionalAttrs (hidpi.enabled && hidpi.viaDconf) {
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
          {
            monitor = "";
            # Prints nothing unless pam_faillock holds failures recent enough
            # to still count, so an ordinary lock screen looks unchanged. A
            # faster poll would buy nothing: it renders whole minutes.
            text = "cmd[update:5000] ${hyprlock-faillock}/bin/hyprlock-faillock";
            font_size = 16;
            font_family = "NotoSans Nerd Font";
            position = "0, -180";
            halign = "center";
            valign = "center";
            color = "rgba(235, 100, 100, 0.95)";
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
      # ~/.claude/settings.json is the only Claude config there is: the
      # ~/.config/claude-<name>/ profile dirs and the claude-prof wrapper that
      # drove them are gone, so every invocation reads these settings.
      settings = claudeBaseSettings;
      # No version/src override: nixpkgs now leads upstream's native-binary
      # releases, and its installPhase unzstds a `claude.zst` src that the old
      # uncompressed-binary pin could not satisfy.
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
    # Zen, not firefox: same gecko underneath (1.22.3b is gecko 156), so the
    # policies/prefs/extensions below are the ones the firefox block carried,
    # moved over unchanged except where noted.
    #
    # Two things the input adds that programs.firefox did not, neither of them
    # asked for here. policies.DisableAppUpdate and DisableTelemetry, both
    # mkDefault true in hm-module/package.nix, which is why the built
    # policies.json next to the real binary carries policies at all while this
    # config declares none (the wrapper keeps a second copy that gecko never
    # reads — see hm-module/package.nix on why only the unwrapped one counts).
    # And, from the flake's own top-level package.nix rather than the
    # home-manager module, a SecurityDevices entry
    # pointing NSS at p11-kit-trust.so, because Zen ships no libnssckbi.so and
    # would otherwise see only the roots compiled into libxul: that makes this
    # host's system trust store a browser trust anchor, which nixpkgs' firefox
    # never did (its wrapper sets SecurityDevices only under withPCSC), and it
    # plausibly pins the hasThirdPartyRoots=1 condition behind the H3 workaround
    # pref below to permanently true.
    #
    # DisableAppUpdate is right for a store-managed browser, but it pairs badly
    # with .forgejo/workflows/vulnix-scan.yml: that scan keys on derivation
    # names, and the closure no longer holds a `firefox-<ver>` for it to match
    # — nothing there is named for this browser at all. Whether NVD carries a
    # `zen-beta` product was not checked; a name-keyed scan cannot match
    # `zen-beta-1.22.3b` either way. thunderbird-156.0 is still in the closure
    # and is the same gecko line, so a gecko-CVE week will surface something,
    # just never attributed to the browser. Fixes arrive on `hmu`.
    #
    # Nothing migrates the old profile either: ~/.mozilla/firefox stays on disk
    # unmanaged (history, logins, and the containers the tridactyl `gC` picker
    # further down would have listed — it degrades to an empty list, it does not
    # break), and Zen starts empty. Import from inside Zen if it turns out to be
    # wanted.
    zen-browser = {
      enable = isDesktop;
      # Without this Zen has no GL at all — not degraded acceleration, no EGL
      # vendor whatsoever, so WebGL reports itself unsupported and
      # /proc/<pid>/maps across every Zen process shows zero GL libraries.
      #
      # The flake's wrapper puts nix's libglvnd on LD_LIBRARY_PATH but ships no
      # DRI drivers (the closure carries mesa-libgbm and nothing else). glvnd
      # therefore falls through its vendor-dir list to the host's
      # /usr/share/glvnd/egl_vendor.d/50_mesa.json and dlopens Arch's
      # /usr/lib/libEGL_mesa.so.0 into a process running nix's glibc. Measured
      # 2026-10-02 with LD_DEBUG=libs under the wrapper's exact environment:
      #
      #   libm.so.6: version lookup error: version `GLIBC_2.43' not found
      #     (required by /usr/lib/libgallium-26.2.2-arch1.1.so) (fatal)
      #
      # Arch is on glibc 2.44, nixpkgs here is pinned (see the flake.lock
      # revert for mosh) at 2.42, and the vendor load is fatal: eglGetDisplay
      # returns NULL with EGL_BAD_DISPLAY. The same probe under `nixGL` gets
      # eglInitialize -> 1.5, Mesa Project — because nixGL points
      # LIBGL_DRIVERS_PATH and the glvnd vendor dirs at nixpkgs' own mesa, so
      # the host's is never opened and the glibc skew stops mattering.
      #
      # This is the module's own option (hm-module/package.nix), which applies
      # config.lib.nixGL.wrap to the selected package — the same wrap ghostty,
      # hyprlock, hyprpaper and cumora already get. Gated like
      # targets.genericLinux.nixGL.* below, since that wrap is only configured
      # on desktop Linux.
      nixGL.enable = !isDarwin && isDesktop;
      # No configPath here, unlike the firefox block — and NOT because of
      # env.nix's MOZ_LEGACY_PROFILES=1. That var only opts gecko out of
      # dedicated (profile-per-install) mode; it never picks the directory. The
      # directory is XDG (~/.config/zen) unless gecko takes its legacy-home
      # branch, which for Zen means MOZ_LEGACY_HOME starts with 1, or ~/.zen
      # already exists, or an undotted ~/mozilla does (that last one is
      # LegacyHomeExists' $HOME/MOZ_USER_DIR probe, and MOZ_USER_DIR is
      # `mozilla`, not `.mozilla`). None holds on these hosts, so the module
      # default is the path Zen actually opens. macOS gets ~/Library/Application
      # Support/Zen, also the module default.
      #
      # Measured 2026-09-26 against zen-beta 1.22.3b, a throwaway $HOME each
      # time, MOZ_LEGACY_PROFILES=1 in scope throughout — the wrapper sets it
      # itself, so it cannot be taken out of the experiment:
      #
      #   bare $HOME                       -> ~/.config/zen
      #   $HOME/.mozilla/firefox present   -> ~/.config/zen  (full launch, not
      #                                       just -CreateProfile)
      #   $HOME/.zen present               -> ~/.zen
      #   MOZ_LEGACY_HOME=1                -> ~/.zen
      #
      # The second line is the one that earns its keep. Two reviewers read gecko
      # 156's LegacyHomeExists() as "any existing ~/.mozilla flips this to
      # ~/.zen" — which, since every host here has ~/.mozilla (thunderbird's
      # native-messaging dir alone guarantees it), would mean every file this
      # block writes is never read. It does not, for two independent reasons:
      # the dotted probe is ~/.zen, because Zen patches AppendFromAppData to
      # append `.` + `Profile=` and drops the vendor component, and the other
      # probe is $HOME/MOZ_USER_DIR = ~/mozilla, undotted. ~/.mozilla matches
      # neither. Zen's same patch deletes upstream's MOZ_APP_PROFILE carve-out,
      # which is why an app with `Profile=` still lands in XDG here.
      #
      # What the experiment does NOT cover: install a distro-packaged Zen, or
      # let anything export MOZ_LEGACY_HOME, and the profile moves to ~/.zen
      # while home-manager keeps writing ~/.config/zen. The symptom is a virgin
      # profile with none of the prefs below and no extensions; check
      # about:profiles, and if it reads ~/.zen then set
      # `configPath = "${config.home.homeDirectory}/.zen"` here. Absolute, not
      # the home-relative `.zen` that mkFirefoxModule also accepts: the
      # zenInstallsIni activation above and the module's own profilesPath both
      # interpolate this value unprefixed, so a relative one turns them
      # CWD-relative. That override also flips mkFirefoxModule's
      # configureAppDataDir, which passes MOZ_APP_DATA into the wrapper — gecko
      # does take it as the profile root ahead of all this logic, but it is
      # untested from here.
      #
      # Drop Version= so Zen uses non-dedicated profile mode and honors
      # Default=1 — else gecko 67+ pins profile-per-install via [Install<HASH>]
      # sections in profiles.ini and ignores Default=. Belt-and-braces rather
      # than load-bearing: MOZ_LEGACY_PROFILES already forces non-dedicated
      # mode, from env.nix and again from the wrapper. Paired with the
      # zenInstallsIni activation above.
      profileVersion = null;
      # No `policies` here any more: the Homepage policy pointed at
      # https://ko.ag/newtab.html, which is broken, so it is gone and what the
      # input contributes by itself is all this config wants. Startup, new
      # windows and the Home button now fall back to Zen's own shipped
      # browser.startup.homepage = about:home (browser.startup.page = 1); new
      # tabs never went through this policy at all — that is NewTabPage — and
      # the deleted StartPage = "homepage" was already Zen's default. So the
      # whole removal amounts to "the homepage reverts to about:home".
      #
      # ~/.config/newtab.html is still written by xdg.configFile below. It has
      # been unreferenced since tridactyl's `set newtab` went away in 265a018,
      # months before this browser migration, so nothing here orphaned it.
      # Linux ownership change worth knowing: programs.firefox fed
      # home-manager's mozilla.firefoxNativeMessagingHosts, so
      # ~/.mozilla/native-messaging-hosts/tridactyl.json was a managed symlink.
      # This module only does that on darwin, so on Linux the manifest comes
      # from the wrapper's launch-time `ln -sfLt` instead — it appears once Zen
      # has run, and is not cleaned up if this option later goes away.
      nativeMessagingHosts = lib.optionals (!isDarwin) [
        pkgs.tridactyl-native
      ];
      # Read hm-module/activation.nix before reaching for profiles.<n>.presets.*,
      # extensionButtons, mods or sine.*. The module's second activation entry
      # (zen-browser-default) is NOT gated on them — it is in every generation
      # already, running the preset-prefs-cleanup and extension-buttons scripts
      # under one lsof guard, and they exit early only because every declared set
      # is empty. What those options arm is the mutation: the scripts rewrite
      # prefs.js and browser.uiCustomization.state, and carry no $DRY_RUN_CMD
      # anywhere, so the first non-empty set makes `switch -n` rewrite the live
      # profile for real. mods/sine go further and curl unpinned `main`-branch
      # code from GitHub at activation, to be run with chrome privileges.
      profiles.default = {
        # rycee's firefox-addons install as-is — same extension IDs, same
        # gecko. Still skipped on macOS, as under firefox; that path has never
        # been exercised and mari keeps darwin.packageMode = "signed" (the
        # module default: upstream .app untouched, so its Team ID integrations
        # — 1Password, Touch ID — keep working).
        extensions.packages = lib.optionals (!isDarwin) (with pkgs.nur.repos.rycee.firefox-addons; [
          bitwarden
          tridactyl
        ]);
        settings = {
          # sidebar.verticalTabs is gone from this list on purpose: it toggles
          # Firefox's own vertical tab strip, which Zen replaces outright with
          # its sidebar. Zen's knobs are the zen.* prefs.
          "ui.key.accelKey" = 91;
          "ui.key.textcontrol.prefer_native_key_bindings_over_builtin_shortcut_key_definitions" = true;
          "signon.rememberSignons" = false;
          "browser.newtab.extensionControlled" = false;
          "browser.ml.chat.enabled" = false;
          # WebTransport workaround: this profile reports hasThirdPartyRoots=1
          # for every QUIC connection (even public sites chaining to built-in
          # roots), so gecko's third-party-roots policy kills H3. HTTPS falls
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
        # Anything but `epoll`, which this was from 2026-08-28 to 2026-09-26
        # and which segfaults ghostty outright. Pinned rather than left at the
        # default `auto` to state the intent: never epoll by choice. Note that
        # neither spelling is a guarantee -- ghostty falls back to another
        # backend when the requested one is unavailable, and on Linux the only
        # other backend IS epoll, so a host with io_uring disabled (hardened
        # kernel, `kernel.io_uring_disabled=1|2`, a restricted container) lands
        # back on the crashing path with no signal. shiori reads 0 today; the
        # other hosts sharing this file were not checked.
        #
        # Untested in the direction that matters: the crash below has not been
        # observed even once under io_uring, because it was never reproduced on
        # demand at all. The mechanism says io_uring cannot reach this path --
        # it is epoll-backend-only -- but the diagnosis rests on the crash
        # signature, not on a reproduction, so this is a move away from a
        # known-bad backend rather than a verified fix.
        #
        # libxev's epoll backend runs a completion's callback for every epoll
        # event it receives without checking the completion is still
        # outstanding. When stream.WriteQueue has drained q_inner on an earlier
        # event in the same batch, a later callback finds q_inner.head == null
        # and the `.?` unwrap in ReleaseFast yields NULL, which is then
        # dereferenced at field offset 0x108. That kills ghostty's `io` thread,
        # so the whole process dies and every window goes with it.
        #
        # Any burst of reply-generating queries can hit it. `herdr` attaching
        # sends 258 of them in its first frame -- OSC 10/11 plus OSC 4;0..255 --
        # and took ghostty 1.3.1 down 3/3 times on 2026-09-26, each crash 30ms,
        # 5ms and 4ms after the client handshake in herdr-server.log -- the
        # preamble's own replies, nothing later. (`journalctl -k -o
        # short-precise` against the connect lines; the herdr-side detach is
        # 0.5-0.8s behind, but that is the server noticing the socket close.)
        # Each a null
        # deref at 0x108 in a thread named `io`. Same signature, same code
        # offset, all three. The race is batch-timing dependent, so it does not
        # reproduce on demand (0 hits in 24 local runs of the upstream flood).
        #
        # mitchellh/libxev#239 is the fix and is unmerged, so no ghostty release
        # carries it yet. Re-test `epoll` once it lands; see also
        # omacom/omarchy#12917, which names herdr as the trigger.
        #
        # Why epoll was tempting, and what io_uring costs: ghostty's libxev loop
        # parks one thread per ring in io_cqring_wait(), which sleeps via
        # io_schedule() and so sets current->in_iowait. The kernel counts that
        # as blocked-on-IO -- it lands in procs_blocked and is flagged
        # TSK_IOWAIT for PSI -- even though the ring only holds idle
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
        # answer was exactly the epoll setting. Kernel commit 7b72d661f1f2 (6.5)
        # gated iowait on having pending requests, which does NOT help here: the
        # armed pty polls *are* pending requests. The real upstream fix is
        # IORING_ENTER_NO_IOWAIT (kernel 6.15+, probed via IORING_FEAT_NO_IOWAIT),
        # which Zig's IoUring does not expose yet -- ziglang/zig#25566.
        #
        # So the accounting noise stays until libxev passes NO_IOWAIT. It is
        # cosmetic; the epoll crash was not.
        async-backend = "io_uring";

        keybind = [
          "ctrl+enter=text:\\r"
          "performable:super+c=copy_to_clipboard"
          "performable:super+v=paste_from_clipboard"
          "super+t=new_tab"
          "ctrl+comma=unbind"

          # Ghostty ships `ctrl++` for increase_font_size and it is unreachable
          # on a US layout. Triggers match the *unmodified* codepoint against the
          # full modifier set, so `+` -- which is shift+`=` -- arrives as
          # ctrl+shift+`=`, and ctrl+shift+= != ctrl++ because the modifiers do
          # not match. Ghostty's own docs name the identical trap for `ctrl+_`.
          # Nothing matches, so the key is forwarded to the app and the terminal
          # types a bare `=`.
          #
          # ctrl+`-` and ctrl+`0` need no shift and were never affected; only
          # increase is. The shipped `ctrl+=` also still works, and this adds the
          # shifted spelling beside it rather than replacing it.
          "ctrl+shift+equal=increase_font_size:1"
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
        key = private.identity.signingKey;
      };

      lfs.enable = true;

      settings = {
        user.name = private.identity.name;
        user.email = private.identity.email;
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
        # kanon over the UCG's "Kon WireGuard" overlay, not Tailscale: shiori's
        # tailscaled is logged out, and kanon is reachable at its LAN address
        # from any WG client once 10.0.10.0/24 is in the tunnel's routes (kube
        # repo docs/network-ip-map.md, "A plain WG client needs the home DHCP
        # pool, not a /32"). Literal IP rather than the bare name so the alias
        # does not also depend on the overlay's resolver winning in resolv.conf.
        "kanon" = {
          User = "root";
          HostName = "10.0.10.47";
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

        # shiori -> utsuho rides the UCG's "Kon WireGuard" overlay, not
        # Tailscale: utsuho's tailscaled is on the work tailnet (tail1beac),
        # and the personal tailnet (alai-ionian) never meets it. utsuho is a
        # plain WG client pinned at 10.9.0.6 (kube repo docs/network-ip-map.md),
        # reachable from the home LAN via the UCG or from anywhere once this
        # host is a WG client itself. The waypipe pair in home.packages is the
        # consumer: `waypipe ssh utsuho <app>` runs the app on utsuho and shows
        # the window here.
        "utsuho" = {
          HostName = "10.9.0.6";
        };

        # `bin/coder`, not `bin/.coder-wrapped`: the overlay above sets
        # `postInstall = ""`, which drops nixpkgs' terraform PATH wrapper, so
        # `bin/coder` IS the real binary and no `.coder-wrapped` is produced.
        # Reaching past the wrapper — correct before that override — now names a
        # file that does not exist, and home-manager will not clobber the stale
        # working ~/.ssh/config to tell you so.
        #
        # No `--global-config`, though `coder config-ssh` emits it and these two
        # blocks were first transcribed from its output. It only names the
        # directory that is already the default — but coder also reads it as
        # "file-based tokens, ignore the keyring" (see --use-keyring in `coder
        # --help`, default true). With the flag, `coder login` puts the session
        # in the keyring while these ProxyCommands keep reading
        # ~/.config/coderv2/session: ssh fails on a stale token while `coder`
        # itself looks signed in. Leave it off so both read the one store.
        "coder.*" = {
          UserKnownHostsFile = "/dev/null";
          ConnectTimeout = "0";
          StrictHostKeyChecking = "no";
          LogLevel = "ERROR";
          ProxyCommand = "${pkgs.coder}/bin/coder ssh --stdio --ssh-host-prefix coder. %h";
        };
        # `header` is the escape hatch for a block header carrying Nix string
        # context (the store path), which can't live in an attr name.
        "*.coder-proxy" = {
          header = "Match host *.coder !exec \"${pkgs.coder}/bin/coder connect exists %h\"";
          ProxyCommand = "${pkgs.coder}/bin/coder ssh --stdio --hostname-suffix coder %h";
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
        # zen-beta.desktop: the flake names package, binary and desktop entry
        # after its variant — `homeModules.default` is the beta one, which
        # tracks upstream's `<ver>b` release tags — so this is the id that
        # exists in the profile.
        "text/html" = "zen-beta.desktop";
        "x-scheme-handler/http" = "zen-beta.desktop";
        "x-scheme-handler/https" = "zen-beta.desktop";
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

    # gvfs falls into the exact same "unknown unit" trap as the portals above.
    # Its D-Bus service files activate via SystemdService=, not Exec=, so
    # putting gvfs on PATH and pointing GIO_EXTRA_MODULES at it (both done, see
    # home.packages and env.nix) gets you only as far as NameHasNoOwner: the
    # broker resolves the name, then hands activation to systemd, which has
    # never heard of the unit. GIO then falls back to GUnixVolumeMonitor, which
    # reads just fstab and /proc/mounts, so a plugged-but-unmounted USB stick
    # is invisible in every GTK file picker and there is nothing to click.
    #
    # All six units, not only udisks2: GIO probes every monitor that has a
    # .monitor file in the package, so linking one and omitting the rest trades
    # a missing drive for four "IsSupported() failed" lines on every
    # enumeration. They are D-Bus activated and idle until something asks.
    configFile."systemd/user/gvfs-udisks2-volume-monitor.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.gvfs}/share/systemd/user/gvfs-udisks2-volume-monitor.service";
    };
    configFile."systemd/user/gvfs-daemon.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.gvfs}/share/systemd/user/gvfs-daemon.service";
    };
    configFile."systemd/user/gvfs-metadata.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.gvfs}/share/systemd/user/gvfs-metadata.service";
    };
    configFile."systemd/user/gvfs-mtp-volume-monitor.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.gvfs}/share/systemd/user/gvfs-mtp-volume-monitor.service";
    };
    configFile."systemd/user/gvfs-gphoto2-volume-monitor.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.gvfs}/share/systemd/user/gvfs-gphoto2-volume-monitor.service";
    };
    configFile."systemd/user/gvfs-afc-volume-monitor.service" = lib.mkIf (!isDarwin && isDesktop) {
      source = "${withHostNss pkgs.gvfs}/share/systemd/user/gvfs-afc-volume-monitor.service";
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
