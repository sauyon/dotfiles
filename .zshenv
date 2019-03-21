HISTFILESIZE=1000000000
HISTSIZE=100000
SAVEHIST=$HISTSIZE
HISTFILE="$XDG_DATA_HOME/zsh/history"

export ZDOTDIR="$HOME/.config/zsh"

# PATH is path.
PATH=$PATH:/usr/local/heroku/bin:~/.gem/ruby/2.1.0/bin:$GOPATH/bin
[[ $platform == 'osx' ]] && PATH=/usr/local/bin:$PATH
PATH=$PATH:$HOME/.local/bin
export PATH

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MOUDLE=ibus

export MOZ_USE_XINPUT2=1
