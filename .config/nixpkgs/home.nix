{ config, lib, pkgs, ... }:

let
  firefoxPkg = pkgs.firefox-bin;

  isDarwin = pkgs.stdenv.isDarwin;

  args = { inherit config lib pkgs firefoxPkg; };
in {
  home.stateVersion = "19.09";

  home.sessionVariables = import ./env.nix args;

  gtk = lib.optionalAttrs (!isDarwin) {
    enable = true;
    theme = {
      name = "Plano";
      package = pkgs.plano-theme;
    };
    font = {
      name = "Noto Sans 12";
      package = pkgs.noto-fonts;
    };
  };

  qt = lib.optionalAttrs (!isDarwin) {
    enable = true;
    platformTheme = "gnome";
  };

  services = {
    gpg-agent = lib.optionalAttrs (!isDarwin) {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 600;
      maxCacheTtl = 1200;
      sshKeys = [ "FDC94CC535DF2557" ];
    };

    emacs.enable = true;
  };

  # systemd.user.startServices = true;
  # systemd.user.services = {
  #   gitstatusd = {
  #     Unit = {
  #       Description = "10x faster implementation of git status";
  #     };

  #     Service = {
  #       Type = "simple";
  #       ExecStart = "${pkgs.gitAndTools.gitstatus}/bin/gitstatusd";
  #     };

  #     Install = {
  #       WantedBy = [ "default.target" ];
  #     };
  #   };
  # };

  programs = {
    home-manager.enable = true;
    alacritty = import ./alacritty.nix;
    broot.enable = true;
    dircolors.enable = true;
    emacs.enable = !isDarwin;
    fish = import ./fish.nix args;

    firefox = lib.optionalAttrs (!isDarwin) {
      enable = true;
      package = firefoxPkg;
      profiles.default = {
        userChrome = ''
          /* Hide tab bar in FF Quantum */
          #main-window[tabsintitlebar="true"]:not([extradragspace="true"]) #TabsToolbar > .toolbar-items {
            opacity: 0;
            pointer-events: none;
          }
          #main-window:not([tabsintitlebar="true"]) #TabsToolbar {
            visibility: collapse !important;
          }
        '';
        settings = {
          "extensions.webextensions.restrictedDomains" = "";
          # "general.useragent.override" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.120 Safari/537.36";
        };
      };
    };

    fzf.enable = true;

    git = {
      enable = true;
      userName = "Sauyon Lee";
      userEmail = "sauyon@github.com";
      ignores = [
        "*~" "\\#*#" "*.orig" ".#*" ".dir-locals.el"
        "*.zip" "*.tar" "*.out" "*.xz" "*.gz" "*.7z"
      ];

      signing = {
        signByDefault = true;
        key = "142D 8892 6B57 DD7D 7000  6647 7181 1ABC 7EF1 15B2";
      };

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
      } // lib.optionalAttrs (!isDarwin) {
        merge.tool = "nixmeld";
        mergetool.nixmeld.cmd = "${pkgs.meld}/bin/meld \"$LOCAL\" \"$REMOTE\"";
      };
    };

    gpg.enable = true;

    ssh = lib.optionalAttrs (!isDarwin) {
      enable = true;
      matchBlocks = {
        "aur" = {
          hostname = "aur.archlinux.org";
          user = "aur";
        };
        "office.semmle.com" = {
          user = "jenkins";
        };
        "semmle" = {
          hostname = "git.semmle.com";
          user = "git";
        };
        "profiling" = {
          hostname = "profiling-sauyon-lee.northeurope.cloudapp.azure.com";
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
        prompt_order = [
          "username"
          "hostname"
          "directory"
          "custom.git"
          "git_branch"
          # "git_commit"
          "git_state"
          # "git_status"
          "docker_context"
          "package"
          "nix_shell"
          "memory_usage"
          "env_var"
          "custom"
          "jobs"
          "character"
        ];
        rprompt_order = [
          "cmd_duration"
          "time"
        ];
        character.symbol = "";
        suffix = "";
        scan_timeout = 10;

        username.style = "fg:yellow bg:black";
        hostname.style = "fg:yellow bg:black";
        directory.style = "fg:black bg:white";
        git_branch.prefix = "";
        git_branch.symbol = "";
        git_state.prefix = "";

        custom.git = {

        };
      };
    };

    zsh = import ./zsh.nix args;
  };
}
