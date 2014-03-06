#!/bin/zsh
# Oh My Zsh config
ZSH=/usr/share/oh-my-zsh

ZSH_THEME=
DISABLE_AUTO_UPDATE="true"
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(archlinux git github go sudo)

# Custom functions to check for ssh and tty
is_ssh() { [[ -n $SSH_CONNECTION ]] || [[ -n $SSH_CLIENT ]] || [[ -n $SSH_TTY ]] }
is_tty() { [[ $(tty) == /dev/tty* ]] }
non_gui() { is_ssh || is_tty }

source $ZSH/oh-my-zsh.sh

# User configuration

# PATH is path. DAMMIT OHMYZSH AGAIN GR
export PATH=$PATH:/usr/local/heroku/bin:~/.gem/ruby/2.1.0/bin:$GOPATH/bin:.

# Zsh preferences
source $XDG_CONFIG_HOME/zsh/aliases
source $XDG_CONFIG_HOME/zsh/prompt

# Yay for syntax highlighting
if [[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# Emacs ftw!
non_gui && export EDITOR='emacsclient -t'

# Ok, fine, sometimes emacs is stupid. But at least it knows it.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ '

# The greeting. Yeah, yeah, I'm unimaginative. :'(
if [[ $(hostname) == 'youmu' ]]; then
	echo 'Hello, Youmu!'
elif [[ $(hostname) == 'suwako' ]]; then
	echo 'Hello, Suwako!'
elif [[ $(hostname) == 'remilia' ]]; then
	echo 'Hello, Remilia!'
else
	echo "Hello, $(hostname)"'!'
fi
