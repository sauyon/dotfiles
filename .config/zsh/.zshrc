#!/bin/zsh
# Utility stuff ----------------------------------------------------------------------------

is_ssh() { [[ -n $SSH_CONNECTION ]] || [[ -n $SSH_CLIENT ]] || [[ -n $SSH_TTY ]] }
non_gui() { is_ssh }
include() { [[ -f "$@" ]] && source "$@" }

platform=unknown
local unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
	platform=linux
elif [[ "$unamestr" == 'Darwin' ]]; then
	platform=osx
fi

# Oh My Zsh config -------------------------------------------------------------------------
ZSH=/usr/share/oh-my-zsh
if [[ ! -d $ZSH ]]; then
	ZSH=~/.oh-my-zsh
fi

DISABLE_AUTO_UPDATE="true"
ZSH_CACHE_DIR="$XDG_CACHE_HOME/zsh"

plugins=(archlinux git github go sudo)

include $ZSH/oh-my-zsh.sh

# User configuration -----------------------------------------------------------------------

# PATH is path. DAMMIT OHMYZSH AGAIN GR
PATH=$PATH:/usr/local/heroku/bin:~/.gem/ruby/2.1.0/bin:$GOPATH/bin
[[ $platform == 'osx' ]] && PATH=/usr/local/bin:$PATH
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:.
export PATH

# Smart command-not-found with pkgfile
include /usr/share/doc/pkgfile/command-not-found.zsh

# Zsh preferences
include $XDG_CONFIG_HOME/zsh/aliases
include $XDG_CONFIG_HOME/zsh/prompt

# Yay for syntax highlighting
include /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Emacs ftw!
non_gui && export EDITOR='emacsclient -t'

# Ok, fine, sometimes emacs is stupid. But at least it knows it.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

# Goodbye rprompt space thingy
ZLE_RPROMPT_INDENT=0

# fuck
if (( $+commands[thefuck] )); then
	eval $(thefuck --alias)
fi

# The greeting. Yeah, yeah, I'm unimaginative. :'(
echo "Hello, $(hostname)"'!'
