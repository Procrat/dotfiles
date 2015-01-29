#!/bin/bash
#
# Colorized battery status for dzen

set -e

source $HOME/.colors

ac_info=/sys/class/power_supply/AC
battery_info=/sys/class/power_supply/BAT

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
# echo $(capacity_color 20)

if [[ $(cat $ac_info/online) -eq 1 ]]; then
    echo "^fg($SECONDARY_CONTENT_COLOR)AC^fg()"
elif [[ $(cat $battery_info/present) -eq 0 ]]; then
    echo 'Wtf, you don'\''t work on AC and you don'\''t have a battery?! o.0' >&2
    exit 1
else
    capacity=$(cat $battery_info/capacity)
    color=$(capacity_color $capacity)
    echo "^fg($color)$capacity%^fg()"
fi
