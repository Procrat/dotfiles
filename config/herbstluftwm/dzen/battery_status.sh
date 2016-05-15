#!/bin/bash
#
# Colorized battery status for dzen

set -euo pipefail

source $HOME/.colors

battery=/sys/class/power_supply/BAT0
icon_dir=$HOME/.config/icons/xbm

status="$(cat $battery/status)"
capacity="$(cat $battery/capacity)"

capacity_color() {
    if [[ $capacity -gt 66 ]]; then
        echo $OK_COLOR
    elif [[ $capacity -gt 33 ]]; then
        echo $WARNING_COLOR
    else
        echo $URGENT_WARNING_COLOR
    fi
}

capacity_icon() {
    local rounded=$(echo "($capacity + 5) / 10 * 10" | bc)
    # We only have icons for 10 to 90 (not for 0 and 100)
    if [[ $rounded -lt 10 ]]; then
        rounded=10
    elif [[ $rounded -gt 90 ]]; then
        rounded=90
    fi
    echo "$icon_dir/battery${rounded}.xbm"
}

if [[ "$status" = 'Full' || "$capacity" -ge 100 ]]; then
    echo -n "^fg($SECONDARY_CONTENT_COLOR)^i($icon_dir/ac_02.xbm)"
elif [[ "$status" = 'Charging' ]]; then
    echo -n "^fg($SECONDARY_CONTENT_COLOR)^i($icon_dir/ac_02.xbm) $capacity%"
else  # Discharging
    echo -n "^fg($(capacity_color))^i($(capacity_icon)) $capacity%"
fi

echo '^fg()'
