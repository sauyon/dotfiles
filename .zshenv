#!/bin/zsh

# make go work :D
export GOPATH=$HOME/go

# emacs!
export EDITOR='emacsclient'

# Yes, I'm a chromium user. Deal with it.
export BROWSER='/usr/bin/chromium'

# 2 space tabs are best tabs
export PAGER='/usr/bin/less -x2'

# Environment vars for use in scripts
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"

# Unclutter home directory
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export QUODLIBET_USERDIR="$XDG_CONFIG_HOME/quodlibet"
export LESSHISTFILE="$XDG_DATA_HOME/less/history"
export HISTFILE="$XDG_DATA_HOME/zsh/history"
export WINEPREFIX="$XDG_DATA_HOME/wineprefixes/default"

# Compilation flags
export ARCHFLAGS="-arch x86_64"
