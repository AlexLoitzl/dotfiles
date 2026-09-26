#!/bin/sh
# Config file expected as first argument
CONFIG=$1

# Read display from sway and take last one
OUTPUT="$(swaymsg -t get_outputs -p | awk '/Output/ && !/disabled/ && $2 != "eDP-1" {print $2}' | tail -1)"
OUTPUT="${OUTPUT:-DVI-I-1}" # Set fallback dummy DVI-I-1 if no second screen
# Remove last line
sed -i '$ d' "$CONFIG"

echo "right = \"$OUTPUT\"" >> "$CONFIG"
