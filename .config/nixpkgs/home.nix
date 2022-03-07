{ config, lib, pkgs, ... }:

let
  firefoxPkg = pkgs.firefox-bin;

  isDarwin = pkgs.stdenv.isDarwin;

  args = { inherit config lib pkgs firefoxPkg; };
in rec {
  home.stateVersion = "21.11";

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  home.sessionVariables = import ./env.nix (args // {
    inherit xdg;
    home = home.homeDirectory;
  });

  systemd.user.sessionVariables = home.sessionVariables;

  home.packages = with pkgs; [
    # packages for sway
    waybar wofi swaylock swayidle wl-clipboard alacritty mako
    kanshi pipewire

    # chromium

    elvish cowsay bat bfs ripgrep rm-improved killall slurp grim
    any-nix-shell-s gh nix-index

    go coreutils gnumake htop btop mosh

    discord
    slack cryptomator drive evince quodlibet pavucontrol
    networkmanagerapplet playerctl nheko bitwarden
    signal-desktop vscode zoom-us

    lutris

    google-drive-ocamlfuse

    drive

    font-awesome
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
    enable = true;
    theme = {
      name = "Plano";
      package = pkgs.plano-theme;
    };
    iconTheme = {
      name = "Yaru";
      package = pkgs.yaru-theme;
    };
    font = {
      name = "Noto Sans 12 for Powerline";
      package = pkgs.powerline-fonts;
    };
  };

  qt = lib.optionalAttrs (!isDarwin) {
    enable = true;
    platformTheme = "gtk";
  };

  services = {
    gpg-agent = lib.optionalAttrs (!isDarwin) {
      enable = true;
      defaultCacheTtl = 600;
      maxCacheTtl = 1200;
      pinentryFlavor = "curses";
    };

    emacs.enable = !isDarwin;
  };

  fonts.fontconfig.enable = true;

  programs = {
    home-manager.enable = true;
    alacritty = import ./alacritty.nix;
    broot.enable = true;
    dircolors.enable = true;
    dircolors.enableZshIntegration = true;
    emacs.enable = !isDarwin;

    waybar.enable = true;

    firefox = lib.optionalAttrs (!isDarwin) {
      enable = true;
      package = firefoxPkg;
    };

    fzf.enable = true;

    git = {
      enable = true;
      userName = "Sauyon Lee";
      userEmail = "git@sjle.co";
      ignores = [
        "*~" "\\#*#" "*.orig" ".#*" ".dir-locals.el"
        "*.zip" "*.tar" "*.out" "*.xz" "*.gz" "*.7z"
        "shell.nix"
        "flake.nix" "flake.lock"
      ];

      signing = {
        signByDefault = true;
        key = "git@sjle.co";
      };

      lfs.enable = true;

      extraConfig = {
        commit = { verbose = true; };
        push = { default = "current"; };
        color = { ui = "auto"; };
        core = {
          pager = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less -RFx4";
          editor = "${pkgs.emacs}/bin/emacsclient -t";
          whitespace = "trailing-space,space-before-tab";
        };
        diff.algorithm = "histogram";
        diff-so-fancy = {
          markEmptyLines = false;
          stripLeadingSymbols = false;
        };
        pull.rebase = true;
        url."semmle:".insteadOf = "https://git.semmle.com/";
        merge.tool = "meld";
      };
    };

    gpg = {
      enable = true;
      settings = {
        keyserver = "keys.openpgp.org";
      };
    };

    ssh = lib.optionalAttrs (!isDarwin) {
      enable = true;
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
      };
    };

    starship = {
      enable = true;

      settings = {
        add_newline = false;
        format = lib.concatStrings [
          "$username"
          "$hostname"
          "$shlvl"
          # "$singularity"
          # "$kubernetes"
          "$directory"
          # "$vcsh"
          "$git_branch"
          "$git_commit"
          "$git_state"
          "$git_metrics"
          "$git_status"
          "$hg_branch"
          "$docker_context"
          "$package"
          "$cmake"
          "$cobol"
          "$dart"
          "$deno"
          "$dotnet"
          "$elixir"
          "$elm"
          "$erlang"
          "$golang"
          # "$helm"
          # "$java"
          # "$julia"
          # "$kotlin"
          # "$lua"
          # "$nim"
          # "$nodejs"
          # "$ocaml"
          # "$perl"
          # "$php"
          # "$purescript"
          "$python"
          # "$rlang"
          # "$red"
          # "$ruby"
          "$rust"
          # "$scala"
          # "$swift"
          # "$terraform"
          # "$vlang"
          # "$vagrant"
          # "$zig"
          "$nix_shell"
          # "$conda"
          # "$memory_usage"
          # "$aws"
          # "$gcloud"
          # "$openstack"
          "$env_var"
          "$crystal"
          "$custom"
          "$cmd_duration"
          "$line_break"
          "$jobs"
          # "$battery"
          "$time"
          "$status"
          "$shell"
          "$character"
        ];
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
      };
    };

    zsh = import ./zsh.nix (args // {
      inherit xdg;
      home = home.homeDirectory;
    });
  };

  xdg = {
    mime.enable = true;

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
