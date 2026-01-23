{
  config,
  lib,
  pkgs,
  ...
}:

let
  isDarwin = pkgs.stdenv.isDarwin;

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
    cosign
    ripgrep
    lnav
    bat
    mosh
    rustup
    nixfmt-rfc-style
    kubectl
    kube-capacity
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

  wayland.windowManager.hyprland = {
    enable = true;
    settings = {
      monitor = [
        "eDP-1,preferred,auto,1.57"
        "DP-1,preferred,auto,1"
      ];

      general = {
        gaps_in = 0;
        gaps_out = 0;

        border_size = 1;
      };

      input = {
        repeat_delay = 200;
        repeat_rate = 60;
      };

      decoration = {
        shadow.enabled = false;
        blur.enabled = false;
      };

      animations = {
        enabled = "no";
      };

      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
      };

      "$terminal" = "ghostty";
      "$menu" = "hyprlauncher";
      "$mainMod" = "SUPER";

      bind = [
        "$mainMod, return, exec, $terminal"
        "$mainMod, K, killactive,"
        "$mainMod SHIFT, E, exit,"
        "$mainMod SHIFT, space, togglefloating,"
        "$mainMod, space, exec, hyprctl dispatch focuswindow $(if [[ $(hyprctl activewindow -j | jq .\"floating\") == \"true\" ]]; then echo \"tiled\"; else echo \"floating\"; fi;)"
        "$mainMod, R, exec, $menu"
        # "$mainMod SHIFT, P, pseudo, # dwindle"
        # "$mainMod, V, togglesplit, # dwindle"
        "$mainMod, G, togglegroup"

        "$mainMod, O, exec, hyprpanel clearNotifications"
        # "$mainMod, O, exec, hyprpanel "

        "$mainMod, B, movefocus, l"
        "$mainMod, F, movefocus, r"
        "$mainMod, P, movefocus, u"
        "$mainMod, N, movefocus, d"
        "$mainMod SHIFT, B, movewindoworgroup, l"
        "$mainMod SHIFT, F, movewindoworgroup, r"
        "$mainMod SHIFT, P, movewindoworgroup, u"
        "$mainMod SHIFT, N, movewindoworgroup, d"

        "$mainMod, L, fullscreen"

        "$mainMod, 1, workspace, 1"
        "$mainMod, 2, workspace, 2"
        "$mainMod, 3, workspace, 3"
        "$mainMod, 4, workspace, 4"
        "$mainMod, 5, workspace, 5"
        "$mainMod, 6, workspace, 6"
        "$mainMod, 7, workspace, 7"
        "$mainMod, 8, workspace, 8"
        "$mainMod, 9, workspace, 9"
        "$mainMod, 0, workspace, 10"

        "$mainMod SHIFT, 1, movetoworkspace, 1"
        "$mainMod SHIFT, 2, movetoworkspace, 2"
        "$mainMod SHIFT, 3, movetoworkspace, 3"
        "$mainMod SHIFT, 4, movetoworkspace, 4"
        "$mainMod SHIFT, 5, movetoworkspace, 5"
        "$mainMod SHIFT, 6, movetoworkspace, 6"
        "$mainMod SHIFT, 7, movetoworkspace, 7"
        "$mainMod SHIFT, 8, movetoworkspace, 8"
        "$mainMod SHIFT, 9, movetoworkspace, 9"
        "$mainMod SHIFT, 0, movetoworkspace, 10"

        "$mainMod, mouse_down, workspace, e-1"
        "$mainMod, mouse_up, workspace, e+1"
      ];
      bindm = [
        "$mainMod, mouse:272, movewindow"
        "$mainMod, mouse:273, resizewindow"
      ];
      bindel = [
        ",XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"
        ",XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ",XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ",XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
        ",XF86MonBrightnessUp, exec, brightnessctl -e4 -n2 set 5%+"
        ",XF86MonBrightnessDown, exec, brightnessctl -e4 -n2 set 5%-"
      ];
      bindl = [
        ", XF86AudioNext, exec, playerctl next"
        ", XF86AudioPause, exec, playerctl play-pause"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioPrev, exec, playerctl previous"
      ];
    };
  };

  fonts.fontconfig.enable = true;

  programs = {
    hyprpanel = {
      enable = true;

      settings = {
        "DP-1" = {
          left = ["dashboard" "workspaces" "windowtitle"];
          middle = ["media"];
          right = [
            "volume"
            "network"
            "bluetooth"
            "systray"
            "clock"
            "notifications"
          ];
        };

        "*" = {
          left = ["dashboard" "workspaces" "windowtitle"];
          middle = ["media"];
          right = ["volume" "clock" "notifications"];
        };
      };
    };

    home-manager.enable = true;
    alacritty = import ./alacritty.nix pkgs.hello;
    broot.enable = true;
    dircolors.enable = true;
    dircolors.enableZshIntegration = true;
    direnv = {
      enable = true;
      # mise.enable = true;
    };
    mise = {
      enable = true;
      enableZshIntegration = true;
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
      options = [ "--cmd cd" ];
    };
    # emacs.enable = !isDarwin;

    fzf.enable = true;

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
        ".aider*"
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
        diff-so-fancy = {
          markEmptyLines = false;
          stripLeadingSymbols = false;
        };
        pull.rebase = true;
        merge.tool = "meld";
        credential."https://github.com".helper = "!/usr/bin/env gh auth git-credential";
        credential."https://gist.github.com".helper = "!/usr/bin/env gh auth git-credential";
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
      enable = false;
      enableZshIntegration = true;
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
