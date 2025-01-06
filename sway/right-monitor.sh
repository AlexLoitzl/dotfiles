#!/bin/sh
# Config file expected as first argument
CONFIG=$1

# Read display from sway and take last one
DISPLAY="$(swaymsg -t get_outputs -p | awk '/Output/ && !/disabled/ {print $2}' | tail -1)"
# Remove last line
sed -i '$ d' $CONFIG

# If the display is the laptop's monitor, use some dummy instead
if [ "$DISPLAY" = "eDP-1" ]; then
  echo "right = \"HDMI-A-1\"" >> $CONFIG 
else
  echo "right = \"$DISPLAY\"" >> $CONFIG 
fi

