[[ -f $HOME/.config/X11/resources ]] && xrdb --merge $HOME/.config/X11/resources
exec i3
