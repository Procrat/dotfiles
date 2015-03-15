#!/bin/bash
#
# HLWM bar using dzen2

script_dir=$(dirname "$BASH_SOURCE")

hc() { herbstclient "$@" ; }

source $HOME/.colors


monitor=${1:-0}
geometry=( $(herbstclient monitor_rect "$monitor") )
if [ -z "$geometry" ] ;then
    echo "Invalid monitor $monitor" >&2 && exit 1
fi
# geometry has the format W H X Y
x=${geometry[0]}
y=${geometry[1]}
panel_width=${geometry[2]}
panel_height=$(cat "$script_dir/panel_height")
font="Trebuchet MS:size=10"


uniq_linebuffered() {
    awk '$0 != l { print ; l=$0 ; fflush(); }' "$@"
}

without_dzen_tags() {
    echo -n "$@" | sed 's/\^[^(^]*([^)]*)//g'
}

extract_icon_width() {
    echo "$@" | \
        # Extract a list of "^i(....xbm)" tags
        sed 's/\^i([^)]*)/\n&\n/g' | \
        grep '^\^i(' | \
        # Grep for the defined width in these XBM-files
        sed 's/\^i(\([^)]*\))/grep width \1/' | \
        bash | \
        # Sum these widths
        sed 's/.*\s\+\([0-9]\+\)\s*/\1/' | \
        sed ':l;N;tl;s/\n/+/g' | \
        bc
}


hc pad $monitor $panel_height

{
    ### Event generator ###
    # based on different input data (date, hlwm hooks, ...) this generates
    # events, formed like this:
    #   <eventname>\t<data> [...]
    # e.g.
    #   date    ^fg(#efefef)18:33^fg(#909090), 2013-10-^fg(#efefef)29

    # Date generator
    while true ; do
        echo -en 'date\t'
        date +"%H:%M^fg($SECONDARY_CONTENT_COLOR):%S   %a %d %b %Y^fg()"
        sleep 1 || break
    done > >(uniq_linebuffered) &
    dateloop=$!

    # Notification generator
    while true ; do
        echo -en 'notification\t'
        $script_dir/notifications.sh
        sleep 2 || break
    done > >(uniq_linebuffered) &
    notificationloop=$!

    # Battery status generator
    while true ; do
        echo -en 'battery\t'
        $script_dir/battery_status.sh
        sleep 8 || break
    done > >(uniq_linebuffered) &
    batteryloop=$!

    hc --idle
    kill $dateloop
    kill $notificationloop
    kill $batteryloop

} 2> /dev/null | {

    IFS=$'\t' read -ra tags <<< "$(hc tag_status $monitor)"
    visible=true
    date=""
    battery=""
    notification=""
    windowtitle=""

    while true; do
        ### Output ###
        # This part prints dzen data based on the _previous_ data handling run,
        # and then waits for the next event to happen.

        separator=" ^fg($BACKGROUND_HIGHLIGHT_COLOR)|^fg()"

        # Arch icon
        echo -n "  ^fg($S_BASE2)^i($HOME/.config/icons/xbm/arch_10x10.xbm)^fg()"
        echo -n "^p(;-1)"
        echo -n "$separator"

        # Tags
        for i in "${tags[@]}" ; do
            case ${i:0:1} in
                '.')  # Empty
                    echo -n "^fg($SECONDARY_CONTENT_COLOR)"
                    ;;
                '#')  # On this monitor, focused
                    echo -n "^fg($INVERSE_CONTENT_COLOR)"
                    echo -n "^bg($INVERSE_BACKGROUND_COLOR)"
                    ;;
                '!')  # Contains urgent window
                    echo -n "^fg($WARNING_COLOR)"
                    ;;
                *)
                    ;;
                # ':')  # Not empty
                    # echo -n "^bg()^fg()"
                    # ;;
                # '+')  # On this monitor, not focused
                    # ;;
                # '%')  # Other monitor, focused
                    # echo -n "^bg()^fg()"
                    # ;;
                # '-')  # Other monitor, not focused
                    # echo -n "^bg()^fg()"
                    # ;;
            esac
            # clickable tags
            echo -n "^ca(1,\"${herbstclient_command[@]:-herbstclient}\" "
            echo -n "focus_monitor \"$monitor\" && "
            echo -n "\"${herbstclient_command[@]:-herbstclient}\" "
            echo -n "use \"${i:1}\") ${i:1} ^ca()"
            echo -n "^fg()^bg()"
        done
        echo -n "$separator"
        echo -n " ^fg($SECONDARY_CONTENT_COLOR)${windowtitle//^/^^}^fg()"

        # Right side
        right="$date  "
        right="$battery^pa(;0)$separator $right"
        if [[ -n $(without_dzen_tags "$notification") ]]; then
            right="$notification $separator $right"
        fi
        right_text_only=$(without_dzen_tags "$right")
        text_width=$($script_dir/xftwidth "$font" "$right_text_only")
        icon_width=$(extract_icon_width "$right")
        width=$((text_width + icon_width))
        echo -n "^pa($(($panel_width - $width)))$right"
        echo

        ### Data handling ###
        # This part handles the events generated in the event loop, and sets
        # internal variables based on them. The event and its arguments are
        # read into the array cmd, then action is taken depending on the event
        # name.
        # "Special" events (quit_panel/togglehidepanel/reload) are also handled
        # here.

        # wait for next event
        IFS=$'\t' read -ra cmd || break
        # find out event origin
        case "${cmd[0]}" in
            tag*)
                IFS=$'\t' read -ra tags <<< "$(hc tag_status $monitor)"
                ;;
            date)
                date="${cmd[@]:1}"
                ;;
            battery)
                battery="${cmd[@]:1}"
                ;;
            notification)
                notification="${cmd[@]:1}"
                ;;
            quit_panel)
                exit
                ;;
            togglehidepanel)
                currentmonidx=$(hc list_monitors | sed -n '/\[FOCUS\]$/s/:.*//p')
                if [ "${cmd[1]}" -ne "$monitor" ] ; then
                    continue
                fi
                if [ "${cmd[1]}" = "current" ] && [ "$currentmonidx" -ne "$monitor" ] ; then
                    continue
                fi
                echo "^togglehide()"
                if $visible ; then
                    visible=false
                    hc pad $monitor 0
                else
                    visible=true
                    hc pad $monitor $panel_height
                fi
                ;;
            reload)
                exit
                ;;
            focus_changed|window_title_changed)
                windowtitle="${cmd[@]:2}"
                ;;
        esac
    done

    ### dzen2 ###
    # After the data is gathered and processed, the output of the previous
    # block gets piped to dzen2.

} 2> /dev/null | dzen2 -w $panel_width -x $x -y $y -h $panel_height -ta l \
    -e 'button3=;button4=exec:herbstclient use_index -1;button5=exec:herbstclient use_index +1'
