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
    iconTheme = {
      name = "Numix";
      package = pkgs.numix-icon-theme;
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
    };

    emacs.enable = true;
  };

  fonts.fontconfig.enable = true;

  programs = {
    home-manager.enable = true;
    alacritty = import ./alacritty.nix;
    broot.enable = true;
    dircolors.enable = true;
    dircolors.enableZshIntegration = true;
    emacs.enable = !isDarwin;

    fzf.enable = true;

    gh.enable = true;
    git = {
      enable = true;
      userName = "Sauyon Lee";
      userEmail = "2347889+sauyon@users.noreply.github.com";
      ignores = [
        "*~" "\\#*#" "*.orig" ".#*" ".dir-locals.el"
        "*.zip" "*.tar" "*.out" "*.xz" "*.gz" "*.7z"
      ];

      signing = {
        signByDefault = true;
        key = "2347889+sauyon@users.noreply.github.com";
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
        # rprompt_order = [
        #   "cmd_duration"
        #   "time"
        # ];
        # character.symbol = "";
        scan_timeout = 10;

        # username.style = "fg:yellow bg:black";
        # hostname.style = "fg:yellow bg:black";
        # directory.style = "fg:black bg:white";
      };
    };

    zsh = import ./zsh.nix args;
  };

  xdg = {
    mime.enable = true;

    userDirs = {
      enable = true;

      desktop = "\$HOME/desktop";
      documents = "\$HOME/documents";
      download = "\$HOME/downloads";
      pictures = "\$HOME/images";
    };
  };
}
