#!/bin/bash
#
# HLWM tag status for dzen

source $HOME/.colors

monitor=$1


IFS=$'\t' read -ra tags <<< "$(herbstclient tag_status $monitor)"
for tag in "${tags[@]}" ; do
    case ${tag:0:1} in
        '.')  # Empty
            echo -n "^fg($SECONDARY_CONTENT_COLOR)"
            ;;
        '#')  # On this monitor, focused
            echo -n "^fg($INVERSE_CONTENT_COLOR)"
            echo -n "^bg($INVERSE_BACKGROUND_COLOR)"
            ;;
        '+')  # On this monitor, not focused
            echo -n "^bg($BACKGROUND_HIGHLIGHT_COLOR)"
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
    echo -n "^fg()^bg()"
done
