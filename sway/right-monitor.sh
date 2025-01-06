#!/bin/sh
DISPLAY="$(swaymsg -t get_outputs -p | awk '/Output/ && !/disabled/ {print $2}' | tail -1)"
sed -i '$ d' $HOME/dotfiles/sway/vars.toml
echo "right = \"$DISPLAY\"" >> $HOME/dotfiles/sway/vars.toml

