{
  pkgs,
  lib,
  home,
  xdg,
  config,
  ...
}:
rec {
  # Unclutter home directory
  LESSHISTFILE = "${xdg.dataHome}/less/history";
  CARGO_HOME = "${xdg.dataHome}/cargo";
  RUSTUP_HOME = "${xdg.dataHome}/rustup";
  ASPELL_CONF = "per-conf ${xdg.configHome}/aspell/aspell.conf; personal ${xdg.configHome}/aspell/en.personal; repl ${xdg.configHome}/aspell/en.prepl";
  ZSH_CACHE_DIR = "${xdg.cacheHome}/zsh";

  GOPATH = "${home}/devel/go";

  PATH = "$PATH:/snap/bin:$GOPATH/bin:${xdg.dataHome}/cargo/bin:${home}/.local/bin:${home}/.krew/bin";

  GRAVEYARD = "$XDG_RUNTIME_DIR/trash";

  PAGER = "${pkgs.bat}/bin/bat --paging=always --color=always --decorations=never --";
  BAT_PAGER = "${pkgs.less}/bin/less";
  LESS = "-RFx4";

  # gpg for ssh
  GPG_TTY = "$(tty)";

  "_JAVA_AWT_WM_NONREPARENTING" = "1";

  QT_QPA_PLATFORM = "wayland";
  QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";

  MOZ_ENABLE_WAYLAND = "1";
  MOZ_LEGACY_PROFILES = "1";

  TG_PROVIDER_CACHE = "1";
}
// lib.optionalAttrs (!pkgs.stdenv.isDarwin) {
  BROWSER = "firefox";
  SSH_AGENT_PID = "";
  SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh";

  # Nix-built ncurses doesn't search the distro paths by default, and
  # home-manager's generic-linux only wires TERMINFO_DIRS for systemd units,
  # not interactive shells. Set it here so Nix-built tools (less, etc.)
  # find both Nix-installed terminfo (e.g. ghostty) and the distro's.
  TERMINFO_DIRS = "${home}/.nix-profile/share/terminfo:/usr/share/terminfo";
}
