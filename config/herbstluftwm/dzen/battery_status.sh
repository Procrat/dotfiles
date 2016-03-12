#!/bin/bash
#
# Colorized battery status for dzen

set -euo pipefail

source $HOME/.colors

battery=/sys/class/power_supply/BAT0
icon_dir=$HOME/.config/icons/xbm

capacity_color() {
    capacity=$1
    if [[ $capacity -gt 66 ]]; then
        echo $OK_COLOR
    elif [[ $capacity -gt 33 ]]; then
        echo $WARNING_COLOR
    else
        echo $URGENT_WARNING_COLOR
    fi
}

capacity_icon() {
    capacity=$1
    rounded=$(echo "($capacity + 5) / 10 * 10" | bc)
    # We only have icons for 10 to 90 (not for 0 and 100)
    if [[ $rounded -lt 10 ]]; then
        rounded=10
    elif [[ $rounded -gt 90 ]]; then
        rounded=90
    fi
    echo $icon_dir/battery${rounded}.xbm
}

if [[ "$(cat $battery/status)" = 'Charging' ]]; then
    echo -n "^fg($SECONDARY_CONTENT_COLOR)^p(;+5)^i($icon_dir/ac_02.xbm)^p()"
    capacity=$(cat $battery/capacity)
    if [[ $capacity -lt 100 ]]; then
        echo -n " $capacity%"
    fi
else
    capacity=$(cat $battery/capacity)
    color=$(capacity_color $capacity)
    icon=$(capacity_icon $capacity)
    echo -n "^fg($color)^p(;+5)^i($icon)^p() $capacity%"
fi

echo '^fg()'
