set paths = [
  $@paths
  $E:GOPATH/bin
  $E:XDG_DATA_HOME/cargo/bin
  $E:HOME/.local/bin
]

set E:XDG_DESKTOP_DIR = $E:HOME"/desktop";
set E:XDG_DOCUMENTS_DIR = $E:HOME"/documents";
set E:XDG_DOWNLOAD_DIR = $E:HOME"/downloads";
set E:XDG_MUSIC_DIR = $E:HOME"/music";
set E:XDG_PICTURES_DIR = $E:HOME"/images";
set E:XDG_PUBLICSHARE_DIR = $E:HOME"/public";
set E:XDG_TEMPLATES_DIR = $E:HOME"/.local/templates";
set E:XDG_VIDEOS_DIR = $E:HOME"/videos";
set E:XDG_STATE_HOME = $E:HOME"/.local/state";
set E:XDG_DATA_HOME = $E:HOME"/.local/share";
set E:XDG_CONFIG_HOME = $E:HOME"/.config";
set E:XDG_CACHE_HOME = $E:HOME"/.cache";

set E:LESSHISTFILE = $E:XDG_DATA_HOME"/less/history"
set E:GTK2_RC_FILES = $E:XDG_DATA_HOME"/gtk-2.0/gtkrc";
set E:WINEPREFIX = $E:XDG_DATA_HOME"/wineprefixes/default";
set E:CARGO_HOME = $E:XDG_DATA_HOME"/cargo";
set E:RUSTUP_HOME = $E:XDG_DATA_HOME"/rustup";
set E:WEECHAT_HOME = $E:XDG_CONFIG_HOME"/weechat";
set E:ASPELL_CONF = "per-conf "$E:XDG_CONFIG_HOME"/aspell/aspell.conf; personal "$E:XDG_CONFIG_HOME"/aspell/en.personal; repl "$E:XDG_CONFIG_HOME"/aspell/en.prepl";
set E:RIPGREP_CONFIG_PATH = $E:XDG_CONFIG_HOME"/ripgrep.conf";

set E:GOPATH = $E:HOME"/devel/go";

set E:GRAVEYARD = $E:XDG_RUNTIME_DIR"/trash"

set E:PAGER = "bat --paging=always --color=always --";
set E:BAT_PAGER = "less";
set E:LESS = "-RFx4";

# gpg for ssh
set E:GPG_TTY = (tty);

set E:NIXPKGS = $E:HOME"/devel/nixpkgs";

set E:_JAVA_AWT_WM_NONREPARENTING = "1";

set E:QT_QPA_PLATFORM = "wayland";
set E:QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";

set E:MOZ_ENABLE_WAYLAND = "1";

set E:BROWSER = "firefox";

set E:SHELL = "elvish";
