HISTFILESIZE=1000000000
HISTSIZE=100000
SAVEHIST=$HISTSIZE
HISTFILE="$XDG_DATA_HOME/zsh/history"

export ZDOTDIR="$HOME/.config/zsh"

# PATH is path.
PATH=$HOME/.nix-profile/bin:$PATH
PATH=$PATH:$GOPATH/bin:$XDG_DATA_HOME/cargo/bin
[[ $platform == 'osx' ]] && PATH=/opt/local/bin:/opt/local/sbin:/usr/local/bin:$PATH
PATH=$PATH:$HOME/.local/bin
export PATH

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MOUDLE=ibus

export MOZ_USE_XINPUT2=1

# Environment vars for use in scripts
# I'm not actually sure this is meaningful...
export XDG_DESKTOP_DIR="$HOME/desktop"
export XDG_DOCUMENTS_DIR="$HOME/documents"
export XDG_DOWNLOAD_DIR="$HOME/downloads"
export XDG_MUSIC_DIR="$HOME/music"
export XDG_PICTURES_DIR="$HOME/images"
export XDG_PUBLICSHARE_DIR="$HOME/public"
export XDG_TEMPLATES_DIR="$HOME/.local/templates"
export XDG_VIDEOS_DIR="$HOME/videos"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"

# Unclutter home directory
export LESSHISTFILE="$XDG_DATA_HOME/less/history"
export GTK2_RC_FILES="$XDG_DATA_HOME/gtk-2.0/gtkrc"
export WINEPREFIX="$XDG_DATA_HOME/wineprefixes/default"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export RUSTUP_HOME="$XDG_DATA_HOME/rustup"
# fuck gnupg it's stupid
# export GNUPGHOME="$XDG_CONFIG_HOME/gnupg"
export WEECHAT_HOME="$XDG_CONFIG_HOME/weechat"
export ASPELL_CONF="per-conf $XDG_CONFIG_HOME/aspell/aspell.conf; personal $XDG_CONFIG_HOME/aspell/en.personal; repl $XDG_CONFIG_HOME/aspell/en.prepl"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep.conf"

export XDG_CURRENT_DESKTOP=Unity

export __HM_SESS_VARS_SOURCED=1
