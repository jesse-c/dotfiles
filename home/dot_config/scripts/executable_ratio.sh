#!/usr/bin/env bash

# Get the screen width
screen_width=$(swaymsg -r -t get_outputs | jq '.[] | select(.active == true) | .rect.width')

# Calculate the new width for the window (1/3 of the screen width)
new_width=$((screen_width / $1))

# Resize the current window to the new width
# Adjusts the width relative to the current width
swaymsg resize set width $new_width px
