#!/bin/bash

set -euo pipefail

SECS=1
ICON_FOLDER=~/.config/icons/xbm
PIPE="/tmp/dvolumepipe"

dzen_dir=$(dirname $BASH_SOURCE)
height=$(($(cat "$dzen_dir/panel_height") - 1))

usage() {
    echo "usage: dvolume.sh [option] [argument]"
    echo
    echo "Options:"
    echo "     -i, --increase - increase volume by \`argument'"
    echo "     -d, --decrease - decrease volume by \`argument'"
    echo "     -t, --toggle   - toggle mute on and off"
    echo "     -h, --help     - display this"
    exit
}

err() {
    echo "$1"
    exit 1
}

# Argument parsing
case "$1" in
    '-i'|'--increase')
        [ -z "$2" ] && err "No argument specified for increase."
        [ -n "$(tr -d [0-9] <<<$2)" ] && err "The argument needs to be an integer."
        AMIXARG="${2}%+ unmute"
        ;;
    '-d'|'--decrease')
        [ -z "$2" ] && err "No argument specified for decrease."
        [ -n "$(tr -d [0-9] <<<$2)" ] && err "The argument needs to be an integer."
        AMIXARG="${2}%- unmute"
        ;;
    '-t'|'--toggle')
        AMIXARG="toggle"
        ;;
    ''|'-h'|'--help')
        usage
        ;;
    *)
        err "Unrecognized option \`$1', see dvolume.sh --help"
        ;;
esac

# Actual volume changing (readability low)
AMIXOUT="$(amixer set Master $AMIXARG | tail -n 1)"

# Parse output to see volume
MUTE="$(cut -d '[' -f 3 <<<"$AMIXOUT")"
if [ "$MUTE" = "off]" ]; then
    VOLUME=0
else
    VOLUME="$(cut -d '[' -f 2 <<<"$AMIXOUT" | sed 's/%.*//g')"
fi
ROUNDED_VOLUME=$(echo "($VOLUME + 24) / 25 * 25" | bc)
ICON="$ICON_FOLDER/volume${ROUNDED_VOLUME}.xbm"

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
(echo "$VOLUME" | gdbar -l "^i($ICON) " ; sleep "$SECS") > "$PIPE"
