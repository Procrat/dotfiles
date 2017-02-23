#!/bin/bash
#
# Xmonad bar using dzen2

script_dir=$(dirname "$BASH_SOURCE")

source $HOME/.colors


screens=($(xrandr | awk '/ connected/ { if ($3 == "primary") print $4; else print $3 }'))
margin=15
panel_height=$(cat "$script_dir/panel_height")
font="Trebuchet MS:size=11"  # Fuck it, I'm hardcoding this shit


uniq_linebuffered() {
    awk '$0 != l { print ; l=$0 ; fflush(); }' "$@"
}

without_dzen_tags() {
    echo -n "$@" | sed 's/\^[^(^]*([^)]*)//g'
}

extract_icon_width() {
    echo "$@" | \
        # Extract a list of "^i(....x[bp]m)" tags
        sed 's/\^i([^)]*)/\n&\n/g' | \
        grep '^\^i(' | \
        # Grep for the defined width in these XBM-files and XPM-files
        sed -e 's_\^i(\(.*xpm\))_sed -n '\''/^"/{s\/"\\([0-9]\\+\\).*/\\1/p;q}'\'' \1_' \
            -e 's_\^i(\(.*xbm\))_sed -n "/width/{s/.*width \\([0-9]\\+\\)/\\1/p;q}" \1_' | \
        bash | \
        # Sum these widths
        sed -n 'G;h;$s/\n/+/g;s/+$//p' | \
        bc
}

dzen_on_screen() {
    screen=(${1//[x+]/ })
    screen_width=${screen[0]}
    screen_x=${screen[2]}
    screen_y=${screen[3]}

    x=$((screen_x + margin))
    y=$screen_y
    width=$((screen_width - 2*margin))

    dzen2 -w $width -x $x -y $y -h $panel_height -ta l -e 'onstart=lower' -dock
}


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

    # Xmonad changes generator
    sed -u 's/^/wm\t/'

    # Kill all generators when the window manager gets killed
    kill $dateloop
    kill $batteryloop
    kill $dropboxloop
    kill $networkloop

} | {

    ### Parser and layouter ###

    SEPARATOR=" ^fg($BACKGROUND_HIGHLIGHT_COLOR)^r(1x$((panel_height - 1)))^fg() "

    visible=true
    date=""
    battery_status=""
    dropbox_status=""
    network_status=""
    wm_info=""

    while true; do
        ### Output ###
        # This part prints dzen data based on the _previous_ data handling run,
        # and then waits for the next event to happen.

        # Vertically center text from now on
        echo -n "^p()"

        # Arch icon
        echo -n "    ^fg($ACCENT_COLOR)^i($HOME/.config/icons/xbm/arch_10x10.xbm)^fg() "
        echo -n " $SEPARATOR "

        # Tags and window title
        echo -n "^fg()^bg()$wm_info"

        # Right side
        right="$date   "
        right="$battery_status $SEPARATOR $right"
        if [[ -n "$network_status" ]]; then
            right="$network_status $SEPARATOR $right"
        fi
        if [[ -n "$dropbox_status" ]]; then
            right="$dropbox_status $SEPARATOR $right"
        fi

        # Calculate padding between left and right side
        right_text_only=$(without_dzen_tags "$right")
        text_width=$($script_dir/xftwidth "$font" "$right_text_only")
        icons_width=$(extract_icon_width "$right")
        width=$((text_width + icons_width))

        # Print padding and right side
        echo "^p(_RIGHT)^p($((-width - 6)))$right"


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
            wm)
                wm_info="${cmd[@]:1}"
                ;;
        esac
    done

} | \

### dzen2 ###
# After the data is gathered and processed, the output of the previous
# block gets piped to dzen2.

# If there's a second screen, also make a bar there.
# (Only support for up to two screens.)
if [[ "${#screens[@]}" > 1 ]]; then
    tee >(dzen_on_screen "${screens[1]}")
else
    cat
fi | \

dzen_on_screen "${screens[0]}"
