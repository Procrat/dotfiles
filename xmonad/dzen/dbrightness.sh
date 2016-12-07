#!/bin/bash

set -euo pipefail

SECS=1
ICON_FOLDER=~/.config/icons/xbm
PIPE="/tmp/dbrightnesspipe"

dzen_dir=$(dirname $BASH_SOURCE)
height=$(($(cat "$dzen_dir/panel_height") - 1))

usage() {
    echo "Usage: dbrightness.sh [options]"
    echo
    echo "Options are the same for xbacklight"
    exit 1
}

# Actual brightness changing
xbacklight $@

# Find rounded brightness and select (matching) icon
BRIGHTNESS=$(xbacklight | sed 's/\..*//')
ICON="$ICON_FOLDER/brightness.xbm"

# Using named pipe to determine whether previous call still exists
# Also prevents multiple volume bar instances
if [ ! -e "$PIPE" ]; then
    mkfifo "$PIPE"

    geometry=( $(herbstclient monitor_rect 0) )
    screen_x=${geometry[0]}
    screen_y=${geometry[1]}
    screen_width=${geometry[2]}
    dzen_x=$(((screen_width - 124) / 2))
    (dzen2 -x $dzen_x -y $screen_y -w 124 -h $height < "$PIPE"; rm -f "$PIPE") &
fi

# Feed the pipe!
(echo "$BRIGHTNESS" | gdbar -l "^i($ICON)  " ; sleep "$SECS") > "$PIPE"
