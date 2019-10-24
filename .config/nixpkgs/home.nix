{ config, pkgs, ... }:

let
  firefoxPkg = pkgs.wrapFirefox pkgs.latest.firefox-bin {
    browserName = "firefox";
    name = "firefox-" + (builtins.parseDrvName pkgs.latest.firefox-bin.name).version;
    desktopName = "Firefox";
  };
in {
  home.stateVersion = "19.09";

  home.sessionVariables = rec {
    EDITOR = "emacsclient";

    # Environment vars for use in scripts
    # I'm not actually sure this is meaningful...
    XDG_DESKTOP_DIR = "$HOME/desktop";
    XDG_DOCUMENTS_DIR = "$HOME/documents";
    XDG_DOWNLOAD_DIR = "$HOME/downloads";
    XDG_MUSIC_DIR = "$HOME/music";
    XDG_PICTURES_DIR = "$HOME/images";
    XDG_PUBLICSHARE_DIR = "$HOME/public";
    XDG_TEMPLATES_DIR = "$HOME/.local/templates";
    XDG_VIDEOS_DIR = "$HOME/videos";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";

    # Unclutter home directory
    LESSHISTFILE = "${XDG_DATA_HOME}/less/history";
    GTK2_RC_FILES = "${XDG_DATA_HOME}/gtk-2.0/gtkrc";
    WINEPREFIX = "${XDG_DATA_HOME}/wineprefixes/default";
    CARGO_HOME = "${XDG_DATA_HOME}/cargo";
    RUSTUP_HOME = "${XDG_DATA_HOME}/rustup";
    WEECHAT_HOME = "${XDG_CONFIG_HOME}/weechat";
    ASPELL_CONF = "per-conf ${XDG_CONFIG_HOME}/aspell/aspell.conf; personal ${XDG_CONFIG_HOME}/aspell/en.personal; repl ${XDG_CONFIG_HOME}/aspell/en.prepl";
    RIPGREP_CONFIG_PATH = "${XDG_CONFIG_HOME}/ripgrep.conf";
    ZSH_CACHE_DIR = "${XDG_CACHE_HOME}/zsh";

    XDG_CURRENT_DESKTOP = "Unity";

    ANDROID_HOME = "/opt/android-sdk";
    GOPATH = "$HOME/devel/go";

    PATH = "$PATH:$GOPATH/bin:${XDG_DATA_HOME}/cargo/bin:$HOME/.local/bin";

    PAGER = "${pkgs.bat}/bin/bat --paging=always --color=always --";
    BAT_PAGER = "${pkgs.less}/bin/less";
    LESS = "-RFx4";

    BROWSER = "${firefoxPkg}/bin/firefox";

    # SEMMLE
    SEMMLE_CODE = "$HOME/devel/code";
    SEMMLE_DIST = "$HOME/devel/code/target/intree/go";
    SEMMLE_HOME = "$HOME/semmle";
    SEMMLE_DATA = "/data";
    SEMMLE_CACHE = "$XDG_CACHE_HOME/semmle";

    # gpg for ssh
    GPG_TTY = "$(tty)";
    SSH_AGENT_PID = "";
    SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh";
  };

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

      package = firefoxPkg;

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
        settings = {
          "extensions.webextensions.restrictedDomains" = "";
          # "general.useragent.override" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.120 Safari/537.36";
        };
      };
    };

    emacs.enable = true;

    git = {
      enable = true;
      userName = "Sauyon Lee";
      userEmail = "sauyon@github.com";
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
        merge.tool = "nixmeld";
        mergetool.nixmeld.cmd = "${pkgs.meld}/bin/meld";
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
        "office.semmle.com" = {
          user = "jenkins";
        };
        "semmle" = {
          hostname = "git.semmle.com";
          user = "git";
        };
        "github" = {
          hostname = "github.com";
          user = "git";
        };
        "shizuka" = {
          hostname = "shizuka.ko.ag";
          user = "sauyon";
          proxyCommand = "${pkgs.cloudflared}/bin/cloudflared access ssh --hostname %h";
        };
      };
    };

    zsh = import ./zsh.nix pkgs;
  };
}
