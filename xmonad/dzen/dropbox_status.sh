#!/bin/bash
#
# Dropbox status icon for dzen

set -euo pipefail

source $HOME/.colors
SCRIPT_DIR=$(dirname "$BASH_SOURCE")
DROPBOX_DIR="$HOME/.dropbox/Dropbox/"
ICON_DIR="$HOME/.config/icons"
NOT_RUNNING_MSG="Dropbox isn't running!"
IDLE_MSG="up to date"
SYNCING_MSG="syncing"

find_db_icon() {
    db_status=$(dropbox-cli filestatus "$DROPBOX_DIR" 2>/dev/null || true)
    if [[ "$db_status" == *"$NOT_RUNNING_MSG" ]]; then
        echo 'x'
    elif [[ "$db_status" == *"$IDLE_MSG" ]]; then
        echo 'idle'
    elif [[ "$db_status" == *"$SYNCING_MSG" ]]; then
        echo 'busy'
    else
        echo 'logo'
    fi
}

icon=$(find_db_icon)
if [[ "$icon" != 'x' ]]; then
    echo -n '^ca(dropbox-cli status | xargs -0 notify-send -a Dropbox)'
    echo -n "^i($SCRIPT_DIR/db_images/dropboxstatus-${icon}.xpm)"
    echo '^ca()'
fi
