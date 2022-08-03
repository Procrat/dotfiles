#!/bin/bash

set -euo pipefail

shopt -s lastpipe

SECS=1
ICON_FOLDER=~/.config/icons/xbm
PIPE="/tmp/dvolumepipe"
WIDTH=124

dzen_dir=$(dirname "${BASH_SOURCE[0]}")
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
        [ -n "$(tr -d '0-9' <<<"$2")" ] && err "The argument needs to be an integer."
        amixer_args=("${2}%+" unmute)
        ;;
    '-d'|'--decrease')
        [ -z "$2" ] && err "No argument specified for decrease."
        [ -n "$(tr -d '0-9' <<<"$2")" ] && err "The argument needs to be an integer."
        amixer_args=("${2}%-" unmute)
        ;;
    '-t'|'--toggle')
        amixer_args=(toggle)
        ;;
    ''|'-h'|'--help')
        usage
        ;;
    *)
        err "Unrecognized option \`$1', see dvolume.sh --help"
        ;;
esac

amixer set Master -M "${amixer_args[@]}" | \
    tail -n 1 | \
    sed -n 's/[^[]\+ \[\([^]]*\)%\] \[[^]]*\] \[\(on\|off\)\]/\1\n\2/p' | \
    {
        read -r volume
        read -r on_off_state
    }

if [ "$on_off_state" = "on" ]; then
    rounded_volume=$(echo "($volume + 24) / 25 * 25" | bc)
    icon="${ICON_FOLDER}/volume${rounded_volume}.xbm"
else
    icon="${ICON_FOLDER}/volume-muted.xbm"
    volume=0
fi

# Using named pipe to determine whether previous call still exists
# Also prevents multiple volume bar instances
if [ ! -e "$PIPE" ]; then
    mkfifo "$PIPE"

    geometry="$(wmctrl -d | awk '/^...\*/ { print $4 }')"
    screen_width="${geometry%x*}"
    dzen_x=$(((screen_width - WIDTH) / 2))
    (dzen2 -x $dzen_x -w $WIDTH -h $height < "$PIPE"; rm -f "$PIPE") &
fi

# Feed the pipe!
(echo "$volume" | gdbar -l "^i($icon) " ; sleep "$SECS") > "$PIPE"
