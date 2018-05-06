HISTFILESIZE=1000000000
HISTSIZE=100000
SAVEHIST=$HISTSIZE
HISTFILE="$XDG_DATA_HOME/zsh/history"

export ZDOTDIR="$HOME/.config/zsh"

# PATH is path.
PATH=$PATH:/usr/local/heroku/bin:~/.gem/ruby/2.1.0/bin:$GOPATH/bin
[[ $platform == 'osx' ]] && PATH=/usr/local/bin:$PATH
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:.
export PATH
