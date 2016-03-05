#!/bin/bash
#
# Colorized wifi status for dzen

set -euo pipefail

source $HOME/.colors

WIFI_DEVICE='wlp58s0'
ICON_DIR="$HOME/.config/icons/xbm"

# This should be between -10 en -100. -10 is best.
find_signal_strength() {
    iw dev "$WIFI_DEVICE" link | \
        grep '^	signal:' | \
        sed 's_.*signal: \(-[0-9]*\) dBm_\1_'
}

find_signal_strength_order() {
    local signal_strength="$(find_signal_strength)"

    if [[ -z "$signal_strength" || "$signal_strength" -eq 0 ]]; then
        # Don't show an image
        return
    fi

    echo $(((signal_strength + 100) / 12))
}

icon() {
    local signal_strength_order="$(find_signal_strength_order)"

    if [[ -n "$signal_strength_order" ]]; then
        echo -n "^fg($SECONDARY_CONTENT_COLOR)"
        echo -n "^ca(1, $HOME/bin/wifi_info.sh | xargs -0 notify-send -a Wifi)"
        echo -n "^i($ICON_DIR/wireless${signal_strength_order}.xbm)"
        echo -n '^ca()'
        echo '^fg()'
    fi
}

icon
