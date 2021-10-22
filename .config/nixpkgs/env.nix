{ pkgs, lib, firefoxPkg, ... }: rec {
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

  GRAVEYARD = "$XDG_RUNTIME_DIR/trash";

  PAGER = "${pkgs.bat}/bin/bat --paging=always --color=always --";
  BAT_PAGER = "${pkgs.less}/bin/less";
  LESS = "-RFx4";

  # gpg for ssh
  GPG_TTY = "$(tty)";

  NIXPKGS="$HOME/devel/nixpkgs";

  "_JAVA_AWT_WM_NONREPARENTING" = "1";

  QT_QPA_PLATFORM = "wayland";
  QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";

  STUDIO_JDK = "/usr/lib/jvm/java-11-openjdk/";
} // lib.optionalAttrs (!pkgs.stdenv.isDarwin) {
  BROWSER = "${firefoxPkg}/bin/firefox";
  SSH_AGENT_PID = "";
  SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh";
}
