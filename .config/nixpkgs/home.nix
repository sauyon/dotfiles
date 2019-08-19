{ config, pkgs, ... }:

{
  gtk = {
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

  qt = {
    enable = true;
    platformTheme = "gnome";
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 600;
      maxCacheTtl = 1200;
      sshKeys = [
        "8F517C9DB64C3501E026B4375F1F39329F92D634"
        "5971188B0E62D499E1F6D72C80C776AB8D6ACC2C"
      ];
      extraConfig = ''
        pinentry-program ${pkgs.pinentry}/bin/pinentry
      '';
    };
  };

  programs = {
    home-manager = {
      enable = true;
      path = "/home/sauyon/devel/home-manager";
    };

    alacritty = import ./alacritty.nix;
    firefox = {
      package = pkgs.firefox-beta-bin;
      enable = true;
      profiles.default = {
        userChrome = ''
          /* Hide tab bar in FF Quantum */
          @-moz-document url("chrome://browser/content/browser.xul") {
          #TabsToolbar {
            visibility: collapse !important;
              margin-bottom: 21px !important;
            }

          #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
            visibility: collapse !important;
            }
          }
        '';
      };
    };

    emacs.enable = true;

    git = {
      enable = true;
      userName = "Sauyon Lee";
      userEmail = "s@uyon.co";
      ignores = [ "*~" "\#*\#" "*.orig" ".\#*" ".dir-locals.el" ];

      signing = {
        signByDefault = true;
        key = "5842 D2C4 8C3C BBC0 65C0 DB8F 74A6 DD28 4BF8 A1D7";
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
        merge.tool = "meld1";
        merge.meld1.cmd = "${pkgs.meld}/bin/meld";
        pull.rebase = true;
      };
    };

    gpg.enable = true;

    ssh = {
      enable = true;
      matchBlocks = {
        "aur" = {
          hostname = "aur.archlinux.org";
          user = "aur";
        };
        "semmle" = {
          hostname = "git.semmle.com";
          user = "git";
        };
        "github" = {
          hostname = "github.com";
          user = "git";
        };
        "lyrica" = { port = 59049; };
        "lyrica.sjl.re" = { port = 59049; };
        "s6.vc" = { port = 59049; };
        "shizuka" = { port = 59049; };
        "shizuka.sjl.re" = { port = 59049; };
      };
    };
  };
}
