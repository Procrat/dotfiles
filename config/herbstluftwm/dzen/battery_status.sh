#!/bin/bash
#
# Colorized battery status for dzen

set -e

source $HOME/.colors

ac_info=/sys/class/power_supply/AC
battery_info=/sys/class/power_supply/BAT
icon_dir=$HOME/.config/icons/xbm

capacity_color() {
    capacity=$1
    if [[ $capacity -gt 66 ]]; then
        echo $GREEN
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

if [[ $(cat $ac_info/online) -eq 1 ]]; then
    echo "^fg($SECONDARY_CONTENT_COLOR)^pa(;4)^i($icon_dir/ac_02.xbm)^fg()"
elif [[ $(cat $battery_info/present) -eq 0 ]]; then
    echo 'Wtf, you don'\''t work on AC and you don'\''t have a battery?! o.0' >&2
    exit 1
else
    capacity=$(cat $battery_info/capacity)
    color=$(capacity_color $capacity)
    icon=$(capacity_icon $capacity)
    echo "^fg($color)^pa(;4)^i($icon) ^pa(;0)$capacity%^fg()"
fi