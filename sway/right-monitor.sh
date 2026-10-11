#!/bin/sh
# Config file expected as first argument
CONFIG=$1

# Read display from sway and take last one
read -r ROUTPUT RWIDTH RHEIGHT < <(swaymsg -t get_outputs | jq -r '.[] | select(.name != "eDP-1") | "\(.name) \(.modes[0] | "\(.width) \(.height)")"')
ROUTPUT="${ROUTPUT:-DVI-I-1}" # Set fallback dummy DVI-I-1 if no second screen

LY=$(( RWIDTH >= 2500 ? 648 : 0 ))
if (( RWIDTH >= 2500 )); then SCALE=1.25; else SCALE=1; fi
LCONF="left_config = \"resolution 1920x1080 position 0,$LY\""
RCONF="right_config = \"resolution ${RWIDTH}x${RHEIGHT} position 1920,0 scale $SCALE\""

# Remove last 3 lines
for j in {1..3}; do sed -i '$ d' "$CONFIG"; done
# Print the config
printf "$LCONF\nright = \"$ROUTPUT\"\n$RCONF" >> $CONFIG
