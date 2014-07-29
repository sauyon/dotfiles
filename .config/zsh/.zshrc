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

DISABLE_AUTO_UPDATE="true"

plugins=(archlinux git github go sudo)

include $ZSH/oh-my-zsh.sh

# User configuration -----------------------------------------------------------------------

# PATH is path. DAMMIT OHMYZSH AGAIN GR
PATH=$PATH:/usr/local/heroku/bin:~/.gem/ruby/2.1.0/bin:$GOPATH/bin:.
[[ $platform == 'osx' ]] && PATH=/usr/local/bin:$PATH
export PATH

# Zsh preferences
include $XDG_CONFIG_HOME/zsh/aliases
include $XDG_CONFIG_HOME/zsh/prompt

# Yay for syntax highlighting
include /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Emacs ftw!
non_gui && export EDITOR='emacsclient -t'

# Ok, fine, sometimes emacs is stupid. But at least it knows it.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

# The greeting. Yeah, yeah, I'm unimaginative. :'(
echo "Hello, $(hostname)"'!'
