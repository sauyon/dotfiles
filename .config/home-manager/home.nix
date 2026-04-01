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
in
rec {
  home.stateVersion = "21.11";

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

  home.packages = with pkgs; [
    bfs
    btop
    coder
    comma
    ripgrep
    lnav
    bat
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
  ];

  nixpkgs.config = {
    allowUnfree = true;
    sandbox = true;
  };

  nixpkgs.overlays = [
    (self: super: {
      any-nix-shell-s = super.any-nix-shell.overrideAttrs (old: {
        src = super.fetchFromGitHub {
          owner = "sauyon";
          repo = "any-nix-shell";
          rev = "3a99be0b3d76a691c940608c477955d122f37e75";
          sha256 = "1g735n0xr50vgcw30igldhmjvb40jgk65x5qjnnxidvm1i3vykw9";
        };
      });
    })
  ];

  gtk = lib.optionalAttrs (!isDarwin) {
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
      package = pkgs.nerdfonts;
    };
  };

  qt = lib.optionalAttrs (!isDarwin) {
    enable = true;
    platformTheme.name = "gtk2";
  };

  services = {
    hyprpaper = {
      enable = true;
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

    gnome-keyring = {
      enable = true;
      components = [
        "pkcs11"
        "secrets"
      ];
    };

    # emacs.enable = !isDarwin;
  };

  wayland.windowManager.hyprland = import ./hyprland.nix (pkgs);

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
      enable = true;

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
      enable = true;
      git.enable = true;
      options = {
        # display = "inline";
      };
    };
    firefox = {
      enable = true;
      profiles.default = {
        settings = {
          "sidebar.verticalTabs" = true;
          "ui.key.accelKey" = 91;
          "signon.rememberSignons" = false;
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
        ".claude/worktrees"
      ];

      signing = {
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
    mime.enable = true;

    portal = {
      enable = true;
      config = {
        common.default = [ "hyprland;gtk" ];
      };
    };

    mimeApps = {
      enable = true;

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
  };
}
