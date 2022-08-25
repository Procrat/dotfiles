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
        volume_arg="+${2}%"
        mute_arg='0'
        ;;
    '-d'|'--decrease')
        [ -z "$2" ] && err "No argument specified for decrease."
        [ -n "$(tr -d '0-9' <<<"$2")" ] && err "The argument needs to be an integer."
        volume_arg="-${2}%"
        mute_arg=
        ;;
    '-t'|'--toggle')
        volume_arg=
        mute_arg='toggle'
        ;;
    ''|'-h'|'--help')
        usage
        ;;
    *)
        err "Unrecognized option \`$1', see dvolume.sh --help"
        ;;
esac

sink=$(pactl get-default-sink)

if [[ -n "$volume_arg" ]]; then
    pactl set-sink-volume "$sink" "$volume_arg"
fi
if [[ -n "$mute_arg" ]]; then
    pactl set-sink-mute "$sink" "$mute_arg"
fi

volume=$(pactl get-sink-volume "$sink" | sed -En 's_^Volume: front-left: [^/]+ / +([0-9]+)% /.*_\1_p')
muted=$(pactl get-sink-mute "$sink" | sed -En 's/^Mute: (.*)$/\1/p')

if [[ "$muted" = 'no' ]]; then
    rounded_volume=$(echo "($volume + 24) / 25 * 25" | bc)
    icon="${ICON_FOLDER}/volume${rounded_volume}.xbm"
elif [[ "$muted" = 'yes' ]]; then
    icon="${ICON_FOLDER}/volume-muted.xbm"
    volume=0
else
    err 'Failed to parse pactl output.'
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
