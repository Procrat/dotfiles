#!/bin/bash
#
# HLWM bar using dzen2

script_dir=$(dirname "$BASH_SOURCE")
logs="$HOME/.local/share/herbstluftwm/logs"
mkdir -p "$(dirname $logs)"

monitor=${1:-0}
shift
hc() { herbstclient "$@" ; }

source $HOME/.colors


geometry=( $(herbstclient monitor_rect "$monitor") )
if [ -z "$geometry" ] ;then
    echo "Invalid monitor $monitor" >&2 && exit 1
fi
# geometry has the format W H X Y
margin=15
x=$((geometry[0] + margin))
y=${geometry[1]}
panel_width=$((geometry[2] - 2*margin))
panel_height=$(cat "$script_dir/panel_height")
font="Trebuchet MS:size=11"  # Fuck it, I'm hardcoding this shit
XPM_ICONS_WIDTH=0  # Fuck it, I'm hardcoding this shit


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
        date +"^ca(1, xdg-open 'http://calendar.google.com')%H:%M^fg($SECONDARY_CONTENT_COLOR):%S   %a %d %b %Y^fg()^ca()" | \
            paste <(echo date) -
        sleep 1 || break
    done > >(uniq_linebuffered) &
    dateloop=$!

    # Battery status generator
    while true ; do
        $script_dir/battery_status.sh | \
            paste <(echo battery) -
        sleep 8 || break
    done > >(uniq_linebuffered) &
    batteryloop=$!

    # Dropbox status generator
    while true ; do
        $script_dir/dropbox_status.sh | \
            paste <(echo dropbox) -
        sleep 8 || break
    done > >(uniq_linebuffered) &
    dropboxloop=$!

    # Network status generator
    while true ; do
        $script_dir/wifi_status.sh | \
            paste <(echo network) -
        sleep 8 || break
    done > >(uniq_linebuffered) &
    networkloop=$!


    # HLWM changes generator
    hc --idle

    # Kill all generators when the window manager gets killed
    kill $dateloop
    kill $batteryloop
    kill $dropboxloop
    kill $networkloop

} 2>> "$logs" | {

    ### Parser and layouter ###

    SEPARATOR="  ^fg($BACKGROUND_HIGHLIGHT_COLOR)^r(1x$((panel_height - 1)))^fg() "

    visible=true
    date=""
    battery_status=""
    dropbox_status=""
    network_status=""
    windowtitle=""

    while true; do
        ### Output ###
        # This part prints dzen data based on the _previous_ data handling run,
        # and then waits for the next event to happen.

        # Subtle border
        echo -n "^pa(0;)^ib(1)^fg($BACKGROUND_HIGHLIGHT_COLOR)^ro(${panel_width}x${panel_height})"
        echo -n "^pa(1;0)^fg($BACKGROUND_COLOR)^ro($((panel_width - 1))x1)^pa(0;0)"

        # Vertically center text from now on
        echo -n "^p()"

        # Arch icon
        echo -n "   ^fg($ACCENT_COLOR)^i($HOME/.config/icons/xbm/arch_10x10.xbm)^fg()"
        echo -n "$SEPARATOR"

        # Tags
        $script_dir/tags.sh $monitor
        echo -n "$SEPARATOR"
        echo -n " ^fg($SECONDARY_CONTENT_COLOR)${windowtitle//^/^^}^fg()"

        # Right side
        right="$date   "
        right="$battery_status$SEPARATOR $right"
        if [[ -n "$network_status" ]]; then
            right="$network_status$SEPARATOR $right"
        fi
        if [[ -n "$dropbox_status" ]]; then
            right="$dropbox_status$SEPARATOR $right"
        fi

        # Calculate padding between left and right side
        right_text_only=$(without_dzen_tags "$right")
        text_width=$($script_dir/xftwidth "$font" "$right_text_only")
        xbm_icons_width=$(extract_icon_width "$right")
        width=$((text_width + xbm_icons_width + XPM_ICONS_WIDTH))

        # Print padding and right side
        echo "^pa($((panel_width - width)))$right"


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
            date)
                date="${cmd[@]:1}"
                ;;
            battery)
                battery_status="${cmd[@]:1}"
                ;;
            dropbox)
                dropbox_status="${cmd[@]:1}"
                ;;
            network)
                network_status="${cmd[@]:1}"
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
                event_monitor=$(hc list_monitors | grep FOCUS |\
                    sed 's/\([0-9]\+\).*/\1/')
                if [[ $monitor == $event_monitor ]]; then
                    windowtitle="${cmd[@]:2}"
                fi
                ;;
        esac
    done

} 2>> "$logs" | \

    ### dzen2 ###
    # After the data is gathered and processed, the output of the previous
    # block gets piped to dzen2.

    dzen2 -w $panel_width -x $x -y $y -h $panel_height -ta l \
    -e 'button3=;button4=exec:herbstclient use_index -1;button5=exec:herbstclient use_index +1'
