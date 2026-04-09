{
  config,
  lib,
  pkgs,
  ...
}:

let
  isDarwin = pkgs.stdenv.isDarwin;
  hostname = builtins.replaceStrings [ "\n" ] [ "" ] (builtins.readFile "/etc/hostname");

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
      { name = "ArgoCD";     url = "https://argocd.tail1beac.ts.net"; }
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
rec {
  home.stateVersion = "21.11";

  xdg.userDirs.setSessionVariables = true;

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  home.sessionVariables =
    import ./env.nix (
      args
      // {
        inherit xdg;
        home = home.homeDirectory;
      }
    )
    // (lib.optionalAttrs (builtins.pathExists ./secrets.nix) (import ./secrets.nix));

  systemd.user.sessionVariables = home.sessionVariables;

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

  home.packages = with pkgs; [
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
    vscode
    vesktop
  ] ++ lib.optionals (!isDarwin) [
    hyprpicker
    xremap
  ];

  nixpkgs.config = {
    allowUnfree = true;
    sandbox = true;
  };

  nixpkgs.overlays = [
    (final: prev: {
      nur = import (builtins.fetchTarball {
        url = "https://github.com/nix-community/NUR/archive/7f4366be821b64f130c08dd47cbc22cad3003d97.tar.gz";
        sha256 = "1vkhfqafcyr17r4ghxhrgcgh39rij8avqi0541nm7178c45yiwl2";
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
      # 2.1.88 was yanked from npm; override until nixpkgs catches up to 2.1.89
      claude-code = super.claude-code.overrideAttrs (old: rec {
        version = "2.1.89";
        src = super.fetchzip {
          url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz";
          hash = "sha256-FoTm6KDr+8Dzhk4ibZUlU1QLPFdPm/OriUUWqAaFswg=";
        };
        npmDeps = super.fetchNpmDeps {
          src = super.runCommand "claude-code-${version}-src-with-lock" { } ''
            cp -r ${src}/. $out
            chmod -R u+w $out
            cp ${./claude-code-package-lock.json} $out/package-lock.json
          '';
          name = "claude-code-${version}-npm-deps";
          hash = "sha256-NI4F5bq0lEuMjLUdkGrml2aOzGbGkdyUckgfeVFEe8o=";
        };
        postPatch = ''
          cp ${./claude-code-package-lock.json} package-lock.json
          substituteInPlace cli.js \
                --replace-fail '#!/bin/sh' '#!/usr/bin/env sh'
        '';
      });
    })
  ];

  gtk = lib.optionalAttrs (!isDarwin) {
    enable = true;
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

  services = {
    hyprpaper = {
      enable = !isDarwin;
      settings = {
        path = "${home.homeDirectory}/images/wallpapers/${hostname}.png";
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

  wayland.windowManager.hyprland = lib.optionalAttrs (!isDarwin) (import ./hyprland.nix (pkgs));

  fonts.fontconfig.enable = true;


  programs = {
    hyprlock = {
      enable = true;
      package = null;
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

      settings = {
        bar = {
          clock.format = "%a %m-%d %H:%M:%S";
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
                "ram"
                "clock"
                "notifications"
              ];
            };
          };

          workspaces.show_numbered = true;

          "customModules.storage.paths" = [ "/" ];
        };
      };
    };

    claude-code = {
      enable = true;
      # enableMcpIntegration = true;
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
          "signon.rememberSignons" = false;
          "browser.newtabpage.enabled" = false;
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
        ];
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
          hostname = "kanon.tail30335.ts.net";
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
        inherit xdg;
        home = home.homeDirectory;
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
      };
    };

    dataHome = "${home.homeDirectory}/.local/share";
    configHome = "${home.homeDirectory}/.config";
    cacheHome = "${home.homeDirectory}/.cache";

    userDirs = {
      enable = true;

      desktop = "${home.homeDirectory}/desktop";
      documents = "${home.homeDirectory}/documents";
      download = "${home.homeDirectory}/downloads";
      music = "${home.homeDirectory}/drive/music";
      pictures = "${home.homeDirectory}/images";
    };

    configFile."xremap/config.yml".text = ''
      modmap:
        - name: Terminals (keep Super as-is)
          application:
            only:
              - com.mitchellh.ghostty
          remap:
            Super_L: Super_L

        - name: Everything else (Super to Ctrl)
          application:
            not:
              - com.mitchellh.ghostty
              - firefox
          remap:
            Super_L: Control_L
    '';

    configFile."newtab.html".text = newtabHtml;

    configFile."tridactyl/tridactylrc".text = ''
      " vim: set filetype=vim

      " Config file is the single source of truth — settings removed here revert to default
      sanitize tridactyllocal tridactylsync

      set newtab https://sauyon.github.io/dotfiles/newtab.html
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
