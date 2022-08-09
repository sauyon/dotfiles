{ pkgs, lib, home, xdg, ... }: rec {
  EDITOR = "emacsclient";

  # Unclutter home directory
  LESSHISTFILE = "${xdg.dataHome}/less/history";
  GTK2_RC_FILES = "${xdg.dataHome}/gtk-2.0/gtkrc";
  WINEPREFIX = "${xdg.dataHome}/wineprefixes/default";
  CARGO_HOME = "${xdg.dataHome}/cargo";
  RUSTUP_HOME = "${xdg.dataHome}/rustup";
  WEECHAT_HOME = "${xdg.configHome}/weechat";
  ASPELL_CONF = "per-conf ${xdg.configHome}/aspell/aspell.conf; personal ${xdg.configHome}/aspell/en.personal; repl ${xdg.configHome}/aspell/en.prepl";
  RIPGREP_CONFIG_PATH = "${xdg.configHome}/ripgrep.conf";
  ZSH_CACHE_DIR = "${xdg.cacheHome}/zsh";

  ANDROID_HOME = "/opt/android-sdk";
  GOPATH = "${home}/devel/go";

  PATH = "$PATH:$GOPATH/bin:${xdg.dataHome}/cargo/bin:${home}/.local/bin";

  GRAVEYARD = "$XDG_RUNTIME_DIR/trash";

  PAGER = "${pkgs.bat}/bin/bat --paging=always --color=always --";
  BAT_PAGER = "${pkgs.less}/bin/less";
  LESS = "-RFx4";

  # gpg for ssh
  GPG_TTY = "$(tty)";

  NIXPKGS="${home}/devel/nixpkgs";

  "_JAVA_AWT_WM_NONREPARENTING" = "1";

  QT_QPA_PLATFORM = "wayland";
  QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";

  STUDIO_JDK = "/usr/lib/jvm/java-11-openjdk/";

  XDG_CURRENT_DESKTOP = "Unity";

  MOZ_ENABLE_WAYLAND = "1";
} // lib.optionalAttrs (!pkgs.stdenv.isDarwin) {
  BROWSER = "firefox";
  SSH_AGENT_PID = "";
  SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh";
}
