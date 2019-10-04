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
      sshKeys = [ "FDC94CC535DF2557" ];
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
      userEmail = "sauyon@semmle.com";
      ignores = [ "*~" "\#*\#" "*.orig" ".\#*" ".dir-locals.el" ];

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
        merge.tool = "meld1";
        merge.meld1.cmd = "${pkgs.meld}/bin/meld";
        pull.rebase = true;
        url."semmle:".insteadOf = "https://git.semmle.com/";
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
        "shizuka" = { port = 59049; };
      };
    };
  };
}
