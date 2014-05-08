[[ -f $HOME/.Xresources ]] && xrdb --merge $HOME/.Xresources
exec i3
