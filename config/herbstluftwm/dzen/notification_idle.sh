#!/bin/bash
#
# Colorized notifications for dzen

set -eu


source $HOME/.colors


delay=4  # in seconds
msg_format='%a%A^fg('$WARNING_COLOR')%s^fg()%B%b^fg()'
app_s=': '
body_s='  '
new_msg_waiting='^fg('$URGENT_WARNING_COLOR')*^fg()'

sind -d -l 2 -s '%N' --new_s="$new_msg_waiting" \
    -f "$msg_format" --app_s="$app_s" --body_s="$body_s" | \
    # Remove first line
    sed -u '1d' | \
    # Backup notification, just in case I missed it
    tee $HOME/.notifications
    # Make links in the message clickable
    sed -u 's/http:[^]) ]*/^ca(1, xdg-open "&")&^ca()/g' | \
    # Show amount of pending messages on the same line
    sed -u 'N;s/\n/ /g' | \
    # Show one at a time
    while read notification; do
        echo "$notification"
        sleep $delay
    done
