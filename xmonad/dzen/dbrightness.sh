#!/bin/bash

set -euo pipefail

SECS=1
ICON_FOLDER=~/.config/icons/xbm
PIPE="/tmp/dbrightnesspipe"
WIDTH=124

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

    geometry="$(wmctrl -d | awk '/^...\*/ { print $9 }')"
    screen_width="${geometry%x*}"
    dzen_x=$(((screen_width - WIDTH) / 2))
    (dzen2 -x $dzen_x -w $WIDTH -h $height < "$PIPE"; rm -f "$PIPE") &
fi

# Feed the pipe!
(echo "$BRIGHTNESS" | gdbar -l "^i($ICON)  " ; sleep "$SECS") > "$PIPE"
