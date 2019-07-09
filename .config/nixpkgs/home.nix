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
    firefox.enable = true;

    git = {
      enable = true;
      userName = "Sauyon Lee";
      userEmail = "s@uyon.co";
      ignores = [ "*~" "\#*\#" "*.orig" ".\#*" ".dir-locals.el" ];

      # signing.signByDefault = true;

      extraConfig = {
        commit = { verbose = true; gpgsign = true; };
        push = { default = "current"; };
        color = { ui = "auto"; };
        core = { pager = "less -x2"; editor = "emacsclient -t"; };
        merge.tool = "meld";
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
        "lyrica" = { port = 59049; };
        "lyrica.sjl.re" = { port = 59049; };
        "shizuku" = { port = 59049; };
        "shizuku.sjl.re" = { port = 59049; };
      };
    };
  };
}
