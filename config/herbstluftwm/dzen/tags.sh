#!/bin/bash
#
# HLWM tag status for dzen

source $HOME/.colors

monitor=${1:0}


IFS=$'\t' read -ra tags <<< "$(herbstclient tag_status $monitor)"
for tag in "${tags[@]}" ; do
    case ${tag:0:1} in
        '.')  # Empty
            echo -n "^fg($SECONDARY_CONTENT_COLOR)"
            ;;
        '#')  # On this monitor, focused
            echo -n "^pa(;0)^fg($ACCENT_COLOR)^r(5x3)^p(-5;)^p()^fg($SLIGHTLY_EMPHASIZED_CONTENT_COLOR)"
            ;;
        '+')  # On this monitor, not focused
            echo -n "^ib(0)^bg($BACKGROUND_HIGHLIGHT_COLOR)"
            ;;
        '!')  # Contains urgent window
            echo -n "^fg($WARNING_COLOR)"
            ;;
        *)
            ;;
        # ':')  # Not empty
            # ;;
        # '%')  # Other monitor, focused
            # ;;
        # '-')  # Other monitor, not focused
            # ;;
    esac
    # Clickable tags
    on_click="herbstclient focus_monitor $monitor && herbstclient use ${tag:1}"
    echo -n "^ca(1,$on_click) ${tag:1} ^ca()"
    if [[ "${tag:0:1}" = "#" ]]; then
        echo -n "^p(-5)^pa(;23)^fg($ACCENT_COLOR)^r(5x3)^p()"
    fi
    echo -n "^fg()^bg()^ib(1)"
done
