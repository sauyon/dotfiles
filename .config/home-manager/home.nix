{ config, lib, pkgs, ... }:

let
  isDarwin = pkgs.stdenv.isDarwin;

  args = { inherit config lib pkgs; };
in rec {
  home.stateVersion = "21.11";

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  home.sessionVariables = import ./env.nix (args // {
    inherit xdg;
    home = home.homeDirectory;
  });

  systemd.user.sessionVariables = home.sessionVariables;

  home.packages = with pkgs; [ nixfmt ];

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
    # enable = true;
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
    platformTheme = "gtk";
  };

  services = {
    gpg-agent = lib.optionalAttrs (!isDarwin) {
      enable = true;
      defaultCacheTtl = 600;
      maxCacheTtl = 1200;
      # pinentryFlavor = "/usr/bin/pinentry";
    };

    gnome-keyring = {
      enable = true;
      components = [ "pkcs11" "secrets" ];
    };

    # emacs.enable = !isDarwin;
  };

  fonts.fontconfig.enable = true;

  programs = {
    home-manager.enable = true;
    alacritty = import ./alacritty.nix pkgs.hello;
    broot.enable = true;
    dircolors.enable = true;
    dircolors.enableZshIntegration = true;
    direnv.enable = true;
    # emacs.enable = !isDarwin;

    # waybar.enable = true;

    fzf.enable = true;

    git = {
      enable = true;
      userName = "Sauyon Lee";
      userEmail = "git@sjle.co";
      ignores = [
        ".vscode"
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
        init.defaulBranch = "main";
        commit = { verbose = true; };
        push = { default = "current"; };
        color = { ui = "auto"; };
        core = {
          pager = "${pkgs.gitAndTools.diff-so-fancy}/bin/diff-so-fancy | ${pkgs.less}/bin/less -RFx4";
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
        url."semmle:".insteadOf = "https://git.semmle.com/";
        # url."github:".insteadOf = "https://github.com/";
        merge.tool = "meld";
        credential."https://github.com".helper = "!/usr/bin/gh auth git-credential";
        credential."https://gist.github.com".helper = "!/usr/bin/gh auth git-credential";
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
        "akane" = {
          port = 59049;
        };
        "kanon" = {
          user = "root";
          port = 59049;
        };
        "testserver" = {
          hostname = "35.163.118.10";
          user = "ubuntu";
        };
        "testclient" = {
          hostname = "52.38.68.189";
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

    zsh = import ./zsh.nix (args // {
      inherit xdg;
      home = home.homeDirectory;
    });
  };

  xdg = {
    mime.enable = true;

    mimeApps = {
      enable = true;

      defaultApplications = {
        "text/html" = "microsoft-edge.desktop";
        "x-scheme-handler/http" = "microsoft-edge.desktop";
        "x-scheme-handler/https" = "microsoft-edge.desktop";
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
