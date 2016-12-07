#!/bin/bash
#
# Colorized wifi status for dzen

set -euo pipefail

source $HOME/.colors

ICON_DIR="$HOME/.config/icons/xbm"

# This is a number in [0,70]. Below 20, the connection seems to drop though.
find_link_quality() {
    awk '/^\s*w/ { if (sub(/\./, "", $3)) { print $3 } }' /proc/net/wireless
}

# Converts a link quality ([0,70]) to a link quality order ([1,5])
find_link_quality_order() {
    local link_quality="$(find_link_quality)"

    if [[ -z "$link_quality" ]]; then
        # Don't show an image
        return
    fi

    local order=$(((link_quality - 11) / 10))
    echo $((order > 0 ? order : 1))
}

icon() {
    local link_quality_order="$(find_link_quality_order)"

    if [[ -n "$link_quality_order" ]]; then
        echo -n "^fg($SECONDARY_CONTENT_COLOR)"
        echo -n "^ca(1, $HOME/bin/wifi_info.sh | xargs -0 notify-send -a Wifi)"
        echo -n "^i($ICON_DIR/wireless${link_quality_order}.xbm)"
        echo -n '^ca()'
        echo '^fg()'
    fi
}

icon
