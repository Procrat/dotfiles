#!/bin/bash
#
# Colorized notifications for dzen

set -e


source $HOME/.colors

echo -n "^fg($EMPHASIZED_CONTENT_COLOR)"

notification=$(
    xprop -root WM_NAME \
    | sed 's/^\S* = \("\(.*\)"\)\?$/\2/' \
    | sed 's/\^/^^/g' \
    | sed 's/http:[^]) ]*/^ca(1, xdg-open "&")&^ca()/g')
echo -n $notification

echo '^fg()'
