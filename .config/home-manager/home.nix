{
  config,
  lib,
  pkgs,
  ...
}:

let
  isDarwin = pkgs.stdenv.isDarwin;
  machine = import ./machine.nix;
  hostname = machine.hostname;

  walker-flake = builtins.getFlake "github:abenz1267/walker";

  agent-orchestrator = (builtins.getFlake "github:sauyon/agent-orchestrator").packages.${pkgs.system}.default;
  ao-mcp = (builtins.getFlake "github:sauyon/ao-mcp").packages.${pkgs.system}.default;
  rampart = import ./rampart-patched.nix { inherit pkgs; };

  ao-run = pkgs.writeShellScriptBin "ao-run" ''
    docker network inspect agent-net >/dev/null 2>&1 || docker network create agent-net

    # Build/rebuild the ao image (dereference nix symlinks for Docker build context)
    AO_BUILD=$(mktemp -d)
    cp -rL ~/.config/agent-orchestrator/* "$AO_BUILD/"
    docker build -t ao "$AO_BUILD"
    rm -rf "$AO_BUILD"

    # Build the hermes plugin (source is on host, node_modules resolved in container)
    PLUGIN_DIR=~/devel/agent-orchestrator/packages/plugins/agent-hermes
    CORE_DIR=~/devel/agent-orchestrator/packages/core
    if [ -d "$PLUGIN_DIR/src" ] && [ ! -f "$PLUGIN_DIR/dist/index.js" ]; then
      docker run --rm -v ~/devel:/repos -w /repos/agent-orchestrator \
        node:24-bookworm sh -c 'npm install -g pnpm && pnpm install --no-frozen-lockfile && pnpm --filter @aoagents/ao-core build && pnpm --filter @aoagents/ao-plugin-agent-hermes build'
    fi

    AO_CONFIG="/tmp/ao-config.yaml"
    cp -f ~/.config/agent-orchestrator/config.yaml "$AO_CONFIG"
    chmod 644 "$AO_CONFIG"
    TTY_FLAG=""
    [ -t 0 ] && TTY_FLAG="-t"
    exec docker run --rm -i $TTY_FLAG \
      --name ao \
      --network agent-net \
      -v "$AO_CONFIG":/work/agent-orchestrator.yaml \
      -e AO_CONFIG_PATH=/work/agent-orchestrator.yaml \
      -e RAMPART_URL=http://rampart:9090 \
      -v ~/devel:/repos \
      -v ~/.config/gh:/root/.config/gh:ro \
      -v ~/.aws:/root/.aws:ro \
      -v ~/.hermes-orchestrator:/root/.hermes \
      -e HERMES_GATEWAY_TOKEN="$(cat ~/.hermes-gateway-token 2>/dev/null)" \
      -e ANTHROPIC_BASE_URL=http://litellm:4000 \
      -e AO_ANTHROPIC_KEY=sk-ao \
      -e OPENAI_API_BASE=https://deepseek.api.modular.com/v1 \
      -e OPENAI_API_KEY="$(cat ~/.config/hermes/secrets/api-key 2>/dev/null || echo REPLACE_ME)" \
      -e AIDER_MODEL=openai/moonshotai/kimi-k2.5 \
      -p 3000:3000 \
      ao "$@"
  '';

  args = { inherit config lib pkgs; };

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
  imports = [ walker-flake.homeManagerModules.default ./hermes.nix ];

  home.stateVersion = "21.11";

  xdg.userDirs.setSessionVariables = true;

  home.username = let v = builtins.getEnv "USER"; in if v != "" then v else "sauyon";
  home.homeDirectory = let v = builtins.getEnv "HOME"; in if v != "" then v else "/home/sauyon";

  home.sessionVariables =
    import ./env.nix (
      args
      // {
        xdg = config.xdg;
        home = config.home.homeDirectory;
      }
    )
    // (lib.optionalAttrs (hostname == "setsuna") {
      QT_FONT_DPI = "120";
    });

  systemd.user.sessionVariables = config.home.sessionVariables;

  home.file.".local/bin/rampart-remote-hook" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      # Proxy Claude Code hook events to a remote Rampart serve instance.
      set -euo pipefail

      RAMPART_URL="''${RAMPART_SERVE_URL:-https://rampart.ko.ag}"
      RAMPART_TOKEN="''${RAMPART_TOKEN:-}"

      input=$(cat)

      tool=$(echo "$input" | jq -r '.tool_name // empty')
      hook_event=$(echo "$input" | jq -r '.hook_event_name // "PreToolUse"')

      if [ -z "$tool" ]; then
        echo '{"hookSpecificOutput":{"hookEventName":"PreToolUse","permissionDecision":"allow"}}'
        exit 0
      fi

      params=$(echo "$input" | jq -c '.tool_input // {}')
      session=$(echo "$input" | jq -r '.session_id // "default"')

      auth_header=""
      if [ -n "$RAMPART_TOKEN" ]; then
        auth_header="Authorization: Bearer ''${RAMPART_TOKEN}"
      fi

      response=$(curl -s --max-time 5 \
        -X POST "''${RAMPART_URL}/v1/tool/''${tool}" \
        -H "Content-Type: application/json" \
        ''${auth_header:+-H "$auth_header"} \
        -d "{\"agent\":\"claude-code\",\"session\":\"''${session}\",\"params\":''${params}}" \
        2>/dev/null) || true

      if [ -z "$response" ]; then
        echo "Rampart: server unreachable, asking by default" >&2
        echo '{"hookSpecificOutput":{"hookEventName":"'"''${hook_event}"'","permissionDecision":"ask","permissionDecisionReason":"Rampart: server unreachable, manual approval required"}}'
        exit 0
      fi

      decision=$(echo "$response" | jq -r '.decision // "ask"' 2>/dev/null) || decision="ask"
      message=$(echo "$response" | jq -r '.message // empty' 2>/dev/null) || message=""

      case "$decision" in
        deny)
          if [ -n "$message" ]; then
            echo "Rampart: ''${message}" >&2
          else
            echo "Rampart: Command blocked by remote policy" >&2
          fi
          exit 2
          ;;
        ask)
          reason="''${message:-Rampart: Manual approval required}"
          echo '{"hookSpecificOutput":{"hookEventName":"'"''${hook_event}"'","permissionDecision":"ask","permissionDecisionReason":"'"''${reason}"'"}}'
          ;;
        allow)
          echo '{"hookSpecificOutput":{"hookEventName":"'"''${hook_event}"'","permissionDecision":"allow"}}'
          ;;
        *)
          echo "Rampart: unexpected decision [''${decision}], asking by default" >&2
          echo '{"hookSpecificOutput":{"hookEventName":"'"''${hook_event}"'","permissionDecision":"ask","permissionDecisionReason":"Rampart: unexpected decision ['"''${decision}"'], manual approval required"}}'
          ;;
      esac
    '';
  };

  systemd.user.services.xremap = lib.optionalAttrs (!isDarwin) {
    Unit = {
      Description = "xremap key remapper";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.xremap}/bin/xremap ${config.xdg.configHome}/xremap/config.yml";
      Restart = "on-failure";
      RestartSec = 3;
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  home.packages = [
    agent-orchestrator
    ao-mcp
    ao-run
    rampart
  ] ++ (with pkgs; [
    bfs
    btop
    coder
    comma
    cosign
    ripgrep
    lnav
    bat
    mise
    mosh
    rustup
    nixfmt
    kubectl
    kube-capacity
    kubectx
    unzip
    zip
    slack
    vesktop
    (emacsPackages.treesit-grammars.with-grammars (grammars: with grammars; [
      tree-sitter-tsx
      tree-sitter-typescript
    ]))
  ]) ++ lib.optionals (!isDarwin) [
    pkgs.hyprpicker
  ];

  nixpkgs.config = {
    allowUnfree = true;
    sandbox = true;
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
    (self: super: {
      any-nix-shell-s = super.any-nix-shell.overrideAttrs (old: {
        src = super.fetchFromGitHub {
          owner = "sauyon";
          repo = "any-nix-shell";
          rev = "3a99be0b3d76a691c940608c477955d122f37e75";
          sha256 = "1g735n0xr50vgcw30igldhmjvb40jgk65x5qjnnxidvm1i3vykw9";
        };
      });
      mosh = super.mosh.overrideAttrs (old: {
        version = "git-decd9b7";
        src = super.fetchFromGitHub {
          owner = "mobile-shell";
          repo = "mosh";
          rev = "decd9b705eb81626f694335b8d5940538beb06da";
          hash = "sha256-SsIj2JCDw7qkJ0NiX0FEQVYM0ATFpC9O/m7jycri0nU=";
        };
        # eee1a8cf is already included in HEAD; drop it to avoid reversed-patch error
        patches = builtins.filter
          (p: !(builtins.isAttrs p && lib.hasInfix "eee1a8cf" (p.url or "")))
          old.patches;
      });
    })
    (final: prev: {
      astal = prev.astal // {
        network = prev.astal.network.overrideAttrs (old: {
          patches = (old.patches or []) ++ [
            ./patches/astal-network-null-bssid.patch
          ];
        });
      };
    })
  ];

  gtk = lib.optionalAttrs (!isDarwin) {
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

  qt = lib.optionalAttrs (!isDarwin) {
    enable = true;
    platformTheme.name = "gtk2";
  };

  programs.walker = lib.optionalAttrs (!isDarwin) {
    enable = true;
    runAsService = true;
  };

  services = {
    hyprpaper = {
      enable = !isDarwin;
      settings = {
        path = "${config.home.homeDirectory}/images/wallpapers/${hostname}.png";
      };
    };

    gpg-agent = lib.optionalAttrs (!isDarwin) {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 600;
      maxCacheTtl = 1200;
      pinentry.package = pkgs.pinentry-gnome3;
    };

    gnome-keyring = lib.optionalAttrs (!isDarwin) {
      enable = true;
      components = [
        "pkcs11"
        "secrets"
      ];
    };

    # emacs.enable = !isDarwin;
  };

  targets.genericLinux.nixGL.packages = import <nixgl> { inherit pkgs; };

  wayland.windowManager.hyprland = lib.optionalAttrs (!isDarwin) (import ./hyprland.nix (pkgs));

  dconf.settings = lib.optionalAttrs (hostname == "setsuna") {
    "org/gnome/desktop/interface" = {
      text-scaling-factor = 1.25;
    };
  };

  fonts.fontconfig.enable = true;


  programs = {
    hyprlock = {
      enable = true;
      package = config.lib.nixGL.wrap pkgs.hyprlock;
      settings = {
        general = {
          hide_cursor = true;
        };

        background = [
          {
            monitor = "";
            path = "screenshot";
            blur_passes = 3;
            blur_size = 8;
          }
        ];

        auth = {
          "fingerprint:enabled" = true;
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
    hyprpanel = {
      enable = !isDarwin;
      systemd.enable = false;

      settings = {
        theme.font = {
          name = "NotoSans Nerd Font";
        } // lib.optionalAttrs (hostname == "setsuna") {
          size = "14px";
        };

        theme.bar = lib.optionalAttrs (hostname == "setsuna") {
          scaling = 125;
        };

        bar = {
          clock.format = "%a %m-%d %H:%M:%S";
          "customModules.ram.icon" = "󰍛";
          layouts = {
            "DP-1" = {
              left = [
                "dashboard"
                "workspaces"
                "windowtitle"
              ];
              middle = [ "media" ];
              right = [
                "volume"
                "network"
                "bluetooth"
                "systray"
                "ram"
                "clock"
                "notifications"
              ];
            };

            "*" = {
              left = [
                "dashboard"
                "workspaces"
                "windowtitle"
              ];
              middle = [ "media" ];
              right = [
                "volume"
                "network"
                "battery"
                "ram"
                "clock"
                "notifications"
              ];
            };
          };

          network.truncation_size = 30;
          workspaces.show_numbered = true;

          "customModules.storage.paths" = [ "/" ];
        };
      };
    };

    thunderbird = {
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
      settings = {
        hooks = {
          PreToolUse = [
            {
              matcher = ".*";
              hooks = [
                {
                  type = "command";
                  command = "rampart-remote-hook";
                }
              ];
            }
          ];
          PostToolUseFailure = [
            {
              matcher = ".*";
              hooks = [
                {
                  type = "command";
                  command = "rampart-remote-hook";
                }
              ];
            }
          ];
        };
        permissions = {
          allow = [
            "Bash(mise run:*)"
          ];
        };
        skipDangerousModePermissionPrompt = true;
      };
    };

    home-manager.enable = true;

    difftastic = {
      enable = !isDarwin;
      git.enable = true;
      options = {
        # display = "inline";
      };
    };
    firefox = {
      enable = true;
      nativeMessagingHosts = [
        pkgs.tridactyl-native
      ];
      profiles.default = {
        extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
          tridactyl
        ];
        settings = {
          "sidebar.verticalTabs" = true;
          "ui.key.accelKey" = 91;
          "ui.key.textcontrol.prefer_native_key_bindings_over_builtin_shortcut_key_definitions" = true;
          "signon.rememberSignons" = false;
          "browser.newtabpage.enabled" = false;
          "browser.newtab.extensionControlled" = false;
        };
      };
    };
    ghostty = {
      enable = true;
      package = null;
      enableZshIntegration = true;
      systemd.enable = false;
      # installBatSyntax = true;

      settings = {
        keybind = [
          "ctrl+enter=text:\\r"
          "performable:super+c=copy_to_clipboard"
          "performable:super+v=paste_from_clipboard"
          "super+t=new_tab"
          "ctrl+comma=unbind"
        ];
      } // lib.optionalAttrs (hostname == "setsuna") {
        font-size = 14;
      };
    };
    gh = {
      enable = true;
      gitCredentialHelper.enable = true;
    };
    dircolors = {
      enable = true;
      enableZshIntegration = true;
    };
    direnv = {
      enable = true;
      mise.enable = true;
    };
    mise = {
      enable = true;
      enableZshIntegration = false;
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
      options = [ "--cmd cd" ];
    };
    # emacs.enable = !isDarwin;

    fzf.enable = true;

    man = {
      enable = true;
    };

    git = {
      enable = true;
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
        ".claude/worktrees"
        ".claude/scheduled_tasks.lock"
        ".claude/plans"
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
        init.defaulBranch = "main";
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
          editor = "/usr/bin/emacsclient -t";
          # editor = "${pkgs.emacs}/bin/emacsclient -t";
          whitespace = "trailing-space,space-before-tab";
        };
        diff.algorithm = "histogram";
        pull.rebase = true;
        merge.tool = "meld";
        # credential."https://github.com".helper = "!/usr/bin/env gh auth git-credential";
        # credential."https://gist.github.com".helper = "!/usr/bin/env gh auth git-credential";
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

      matchBlocks = {
        "aur" = {
          hostname = "aur.archlinux.org";
          user = "aur";
        };
        "github" = {
          hostname = "github.com";
          user = "git";
        };
        "shizuka" = {
          port = 59049;
        };
        "akane" = {
          port = 59049;
        };
        "kanon" = {
          user = "root";
          hostname = "kanon.alai-ionian.ts.net";
          port = 59048;
        };
        "testserver" = {
          hostname = "35.163.118.10";
          user = "ubuntu";
        };
        "testclient" = {
          hostname = "52.38.68.189";
          user = "ubuntu";
        };
        "tf" = {
          hostname = "kanon.ko.ag";
          port = 59048;
          forwardAgent = true;
          remoteForwards = [
            {
              bind.address = "/run/user/1000/gnupg/S.gpg-agent";
              host.address = "/run/user/1000/gnupg/S.gpg-agent.extra";
            }
          ];
        };
        "prod-db-subnet-router" = {
          user = "ec2-user";
        };
        "bcctl-subnet-router" = {
          user = "ubuntu";
        };

        "coder.*" = {
          userKnownHostsFile = "/dev/null";
          extraOptions = {
            ConnectTimeout = "0";
            StrictHostKeyChecking = "no";
            LogLevel = "ERROR";
            ProxyCommand = "/usr/bin/coder --global-config /home/sauyon/.config/coderv2 ssh --stdio --ssh-host-prefix coder. %h";
          };
        };
        "*.coder-proxy" = {
          match = "host *.coder !exec \"${pkgs.coder}/bin/.coder-wrapped connect exists %h\"";
          extraOptions = {
            ProxyCommand = "${pkgs.coder}/bin/.coder-wrapped --global-config /home/sauyon/.config/coderv2 ssh --stdio --hostname-suffix coder %h";
          };
        };
        "*.coder" = {
          userKnownHostsFile = "/dev/null";
          extraOptions = {
            ConnectTimeout = "0";
            StrictHostKeyChecking = "no";
            LogLevel = "ERROR";
          };
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

        # 	bind "Alt g" { SwitchToMode "Locked"; }
        # 	bind "Alt d" { Detach; }
        # 	bind "Alt q" { Quit; }
        # 	bind "Alt n" { NewPane; }
        # 	bind "Alt w" { CloseFocus; SwitchToMode "Normal"; }
        # 	bind "Alt f" { ToggleFocusFullscreen; SwitchToMode "Normal"; }
        # 	bind "Alt h" "Alt Left" { MoveFocusOrTab "Left"; }
        # 	bind "Alt l" "Alt Right" { MoveFocusOrTab "Right"; }
        # 	bind "Alt j" "Alt Down" { MoveFocus "Down"; }
        # 	bind "Alt k" "Alt Up" { MoveFocus "Up"; }
        # 	bind "Alt =" "Alt +" { Resize "Increase"; }
        # 	bind "Alt -" { Resize "Decrease"; }
        # 	bind "Alt [" { PreviousSwapLayout; }
        # 	bind "Alt ]" { NextSwapLayout; }
      };
    };
  };

  xdg = {
    mime.enable = !isDarwin;

    portal = {
      enable = !isDarwin;
      config = {
        common.default = [ "hyprland;gtk" ];
      };
    };

    mimeApps = {
      enable = !isDarwin;

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

    configFile."xremap/config.yml".text = ''
      keymap:
        - name: Electron apps (Super to Ctrl shortcuts)
          application:
            only:
              - Slack
              - vesktop
          remap:
            Super_L-a: C-a
            Super_L-c: C-c
            Super_L-v: C-v
            Super_L-x: C-x
            Super_L-z: C-z
            Super_L-Shift-z: C-Shift-z
            Super_L-w: C-w
            Super_L-t: C-t
            Super_L-l: C-l
            Super_L-k: C-k
            Super_L-s: C-s
            Super_L-Shift-v: C-Shift-v
    '';

    configFile."newtab.html".text = newtabHtml;

    configFile."agent-orchestrator/config.yaml".text = let
      # Indent a multiline string for embedding inside a YAML block scalar.
      # orchestratorRules is used under keys indented 8 spaces, so each line
      # needs 8 extra spaces to stay inside the YAML | block.
      indentYaml = n: s:
        let pad = lib.concatStrings (builtins.genList (_: " ") n);
        in lib.concatMapStringsSep "\n" (line: if line == "" then "" else pad + line) (lib.splitString "\n" s);
      orchestratorRules = ''
        ## TDD-First Workflow (Test-Driven Development)

        Every task follows this multi-stage TDD workflow — no exceptions:

        ### Phase 1: Test Design & Implementation (RED)
        1. **Spawn a Test Engineer worker** to write ONLY the failing tests that
           capture the requirement or bug. They must NOT implement the fix or
           the feature yet.
        2. **Once the test PR is open**, spawn a **Test Review agent** with this prompt:
           "You are a Quality Engineer. Review these tests. Do they accurately
           capture the requirement? Are they sufficiently exhaustive (edge cases,
           error paths, performance constraints)? Do they fail as expected against
           the current codebase? Rate 1–5 and give an overall score. If the score
           is below 5/5, list specific changes required to make the test suite
           a perfect descriptor of the goal."
        3. **Iterate** until the Test Review agent gives a 5/5 score.

        ### Phase 2: Code Implementation (GREEN)
        4. **Only after the tests are approved (5/5)**, spawn an **Implementation worker**.
           Their task is to implement the code necessary to make the previously
           approved tests pass. They must not modify the tests unless they find
           a genuine error in the test design (which requires a new Phase 1 review).
        5. **Once the implementation PR is open**, spawn a **Code Review agent**
           with the standard checklist (correctness, readability, performance).

        ### Phase 3: Final Approval
        6. **Spawn a Code Review agent** for general correctness and readability.
        7. **If the task touches performance-sensitive paths** (data processing,
           hot loops, public APIs), also spawn a **Perf Review agent**:
           "Review this PR for performance regressions: unnecessary allocations,
           O(n²) patterns, missing indexes, and blocking I/O. Flag any degradation
           in p99 latency or memory usage."
        8. **The Code Review agent** must also verify that the tests from Phase 1
           still pass and are properly integrated.

        ### Deploy Preview Validation (all PRs)
        Every review agent MUST identify and use the deploy preview for the PR:
        1. Run `gh pr checks <PR>` and `gh pr view <PR> --comments` to find the
           deploy preview URL (look for Vercel, Netlify, or any bot-posted URL).
        2. If a preview URL exists, validate that it loads and the changed
           functionality works as described in the PR.
        3. If no preview URL is available after checks complete, note it in the
           review — do not block, but flag that preview validation was skipped.

        ### UI/Frontend QA (required when PR touches UI)
        If the PR modifies frontend code (components, pages, styles, assets),
        the QA agent MUST perform end-to-end validation against the live preview:
        1. Use the **Playwright MCP** server (available as `playwright` in your
           MCP tools) to launch a headless Chromium browser against the deploy
           preview URL. Use its navigation, click, fill, and screenshot tools.
        2. Write and run ad-hoc test flows that exercise the changed UI paths:
           navigate to affected pages, interact with modified components, verify
           visual correctness and functional behavior.
        3. Check for regressions: confirm that pages adjacent to the change still
           render and behave correctly.
        4. Capture screenshots of before/after states where relevant and attach
           them to the review.
        5. Rate the UI separately (UX correctness, accessibility, responsiveness)
           in addition to the code review score. Both must be 5/5.
        If Playwright is not available or the preview is down, escalate to the
        user — do NOT silently skip UI validation for frontend PRs.

        ### Remote Environment Testing (bcctl)
        The **bcctl MCP** server is available for provisioning and managing
        BentoCloud dev environments. Use it when:
        - The change requires a running backend to validate (API changes, model
          serving, deployment configs).
        - You need to test against a real BentoCloud cluster rather than local mocks.
        - The deploy preview is insufficient (e.g. backend-only changes with no
          preview URL).
        Use `bcctl` tools to create/attach to a dev environment, sync the PR
        branch, and run the relevant test suite or manual verification there.

        9. **Stop only when all required review agents (Code/Perf/QA) give 5/5.**
           Then notify the user that the PR is ready for the user to manually promote. (Drafts forever.)
 
        for the answer. The call blocks until the user replies, so only use
        it when you genuinely need their input.

        Use this for:
        - Design decisions the user must make
        - Ambiguous requirements that need clarification
        - Notifying the user that a PR is ready for review (5/5 score)
        - Escalating issues that workers cannot resolve

        Do NOT use this for routine status updates — those go through `ao status`.
        Only contact the user when you genuinely need their input or attention.
      '';
    in ''
      # managed by home-manager
      defaults:
        orchestrator:
          agent: hermes
        agentRules: |
          Always use TDD (Test-Driven Development).
          Follow this cycle:
          1. Write a failing test (RED).
          2. Run the test to confirm failure.
          3. Implement the minimal code to pass (GREEN).
          4. Refactor and ensure tests remain green.

      plugins:
        - name: hermes
          source: local
          path: /repos/agent-orchestrator/packages/plugins/agent-hermes

      projects:
        mcloud:
          repo: bentoml/modularcloud
          path: /repos/mcloud
          defaultBranch: main
          runtime: tmux
          agent: claude-code
          agentConfig:
            permissions: default
            model: moonshotai/kimi-k2.5
          orchestratorRules: |
${indentYaml 12 orchestratorRules}

        mammoth:
          repo: modularml/mammoth
          path: /repos/mammoth
          defaultBranch: main
          runtime: tmux
          agent: claude-code
          agentConfig:
            permissions: default
            model: moonshotai/kimi-k2.5
          orchestratorRules: |
${indentYaml 12 orchestratorRules}

        modular:
          repo: modularml/modular
          path: /repos/modular
          defaultBranch: main
          runtime: tmux
          agent: claude-code
          agentConfig:
            permissions: default
            model: moonshotai/kimi-k2.5
          orchestratorRules: |
${indentYaml 12 orchestratorRules}

      reactions:
        ci-failed:
          auto: true
          action: send-to-agent
          retries: 2
        approved-and-green:
          auto: false
          action: notify
    '';

    configFile."agent-orchestrator/Dockerfile".text = ''
      FROM node:24-bookworm

      RUN apt-get update && apt-get install -y \
          git \
          tmux \
          gh \
          python3 \
          python3-pip \
          python3-venv \
          pipx \
          sqlite3 \
          && rm -rf /var/lib/apt/lists/*

      RUN npm install -g @aoagents/ao@0.2.5 @anthropic-ai/claude-code @playwright/mcp
      RUN npx playwright install --with-deps chromium
      RUN pipx install aider-chat && pipx ensurepath

      # Pre-bake onboarding state so interactive prompts are skipped
      RUN printf '{"hasCompletedOnboarding":true,"lastOnboardingVersion":"2.1.108","numStartups":1,"migrationVersion":11,"projects":{}}\n' > /root/.claude.json

      # MCP servers available to all Claude Code agent sessions
      RUN mkdir -p /root/.claude && cat > /root/.claude/settings.json << 'MCPCFG'
      {
        "permissions": {
          "allow": [
            "mcp__bcctl__*",
            "mcp__playwright__*"
          ]
        },
        "mcpServers": {
          "playwright": {
            "command": "npx",
            "args": ["@playwright/mcp", "--headless", "--browser", "chromium"]
          },
          "bcctl": {
            "command": "/repos/bcctl-mcp/.venv/bin/python",
            "args": ["-m", "bcctl_mcp.server"],
            "env": {
              "BENTOCLOUDCTL_BIN": "/repos/bentocloudctl/bin/bentocloudctl"
            }
          }
        }
      }
      MCPCFG

      # Bootstrap bcctl-mcp venv and inject into project .mcp.json
      RUN cat >> /usr/local/bin/setup-mcps << 'SETUPMCP'
      #!/bin/sh
      if [ -d /repos/bcctl-mcp ]; then
        # Recreate venv if missing or broken (e.g. host Python symlink)
        if [ ! -x /repos/bcctl-mcp/.venv/bin/python ] || \
           ! /repos/bcctl-mcp/.venv/bin/python -c "pass" 2>/dev/null; then
          rm -rf /repos/bcctl-mcp/.venv
          python3 -m venv /repos/bcctl-mcp/.venv
          /repos/bcctl-mcp/.venv/bin/pip install -e /repos/bcctl-mcp 2>/dev/null
        fi
      fi
      # Inject bcctl MCP into project .mcp.json (project-level overrides global settings)
      CWD="$(pwd)"
      if [ -f "$CWD/.mcp.json" ] && command -v node >/dev/null 2>&1; then
        node -e "\
          const fs = require('fs');\
          const f = process.argv[1];\
          let c = {};\
          try { c = JSON.parse(fs.readFileSync(f, 'utf8')); } catch(e) {}\
          if (!c.mcpServers) c.mcpServers = {};\
          if (!c.mcpServers.bcctl) {\
            c.mcpServers.bcctl = {\
              command: '/repos/bcctl-mcp/.venv/bin/python',\
              args: ['-m', 'bcctl_mcp.server'],\
              env: { BENTOCLOUDCTL_BIN: '/repos/bentocloudctl/bin/bentocloudctl' }\
            };\
            fs.writeFileSync(f, JSON.stringify(c, null, 2) + '\\n');\
          }\
        " "$CWD/.mcp.json" 2>/dev/null
      fi
      SETUPMCP
      RUN chmod +x /usr/local/bin/setup-mcps

      # Wrapper: auto-trust workspace + inject API key via apiKeyHelper.
      # ANTHROPIC_API_KEY triggers an interactive confirmation dialog in
      # non-print mode, so we pass the key through apiKeyHelper in --settings
      # which bypasses that prompt. Does NOT use --bare so hooks/plugins
      # (Rampart) remain active.
      RUN mv /usr/local/bin/claude /usr/local/bin/claude-real && \
          cat > /usr/local/bin/claude << 'WRAPPER'
      #!/bin/sh
      CWD="$(pwd)"
      STATE=/root/.claude.json
      if [ -f "$STATE" ] && command -v node >/dev/null 2>&1; then
        node -e "\
          const fs = require('fs');\
          const s = JSON.parse(fs.readFileSync(process.argv[1],'utf8'));\
          if (!s.projects) s.projects = {};\
          const k = process.argv[2];\
          if (!s.projects[k]) s.projects[k] = {};\
          s.projects[k].hasTrustDialogAccepted = true;\
          fs.writeFileSync(process.argv[1], JSON.stringify(s, null, 2));\
        " "$STATE" "$CWD"
      fi
      # Bootstrap bcctl-mcp venv on first run (bind mount not available at build time)
      setup-mcps 2>/dev/null
      if [ -n "$AO_ANTHROPIC_KEY" ]; then
        # Merge apiKeyHelper into global settings (preserves MCP server config)
        SFILE=$(mktemp /tmp/claude-settings.XXXXXX.json)
        node -e "\
          const fs = require('fs');\
          let s = {};\
          try { s = JSON.parse(fs.readFileSync('/root/.claude/settings.json','utf8')); } catch(e) {}\
          s.apiKeyHelper = 'echo ' + process.argv[1];\
          fs.writeFileSync(process.argv[2], JSON.stringify(s));\
        " "$AO_ANTHROPIC_KEY" "$SFILE"
        exec claude-real --settings "$SFILE" "$@"
      fi
      exec claude-real "$@"
      WRAPPER
      RUN chmod +x /usr/local/bin/claude

      ENV PATH="/root/.local/bin:$PATH"

      RUN git config --global --add safe.directory '*'
      RUN git config --global user.name "ao-agent" && \
          git config --global user.email "ao-agent@localhost"

      WORKDIR /work

      EXPOSE 3000

      ENTRYPOINT ["ao"]
    '';


    configFile."tridactyl/tridactylrc".text = ''
      " vim: set filetype=vim

      set smoothscroll true

      unbind d
      bind <A-x> fillcmdline_notrail

      " J/K for tabs, x to close
      bind x tabclose

      " Detach tab to new window
      bind gd tabdetach

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
  };
}
