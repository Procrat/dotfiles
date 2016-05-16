#!/bin/bash
#
# Rules for new windows

hc() { herbstclient "$@" ; }


# Reset all rules
hc unrule -F

# Focus new clients
hc rule focus=on pseudotile=on

# Pseudo-tile dialogs and place them on top of the existing window (in a very
# hackish way)
hc rule windowtype~'_NET_WM_WINDOW_TYPE_(DIALOG|UTILITY|SPLASH)' pseudotile=on hook=dialog

# Option 1: Temporarily change the frame layout to max and afterwards change it
# back. Disadvantage: this obviously behaves weird when there is more than one
# window in the frame.
# (hc --idle | {
    # while true; do
        # # Wait till the next dialog spawns or reload
        # read event || break

        # if [[ "$event" = reload ]]; then
            # break
        # elif (echo "$event" | grep '^rule dialog'); then
            # layout=$(hc layout | grep FOCUS | grep -o 'vertical\|horizontal\|max\|grid')
            # hc set_layout max
            # # Turn back to original layout when dialog is dismissed
            # hc --wait focus_changed
            # hc set_layout $layout
        # fi
    # done
# }) &

# Option 2: Temporarily make a new monitor on top of the current one with a new
# tag and add the dialog to this tag. When the dialog is closed, delete the
# tag and monitor again. Disadvantage: This may have some undefined behavior
# when switching tags while a dialog is opened. For starters, the dialog will
# have the focus on every tag.
(hc --idle | {
    while true; do
        # Wait till the next dialog spawns or reload
        read event || break

        if [[ "$event" = reload ]]; then
            break
        elif (echo "$event" | grep '^rule\s*dialog'); then
            hc lock
            dialog_id=$(echo "$event" | sed 's/rule\s*dialog\s*\(.*\)/\1/')
            # hc manage on
            hc add float_tag
            hc floating float_tag on
            hc add_monitor 1920x1080 float_tag float_monitor
            hc lock_tag float_monitor
            hc move float_tag
            hc unlock

            hc --wait tag_flags
            hc focus_monitor 0
            hc remove_monitor float_monitor
            hc merge_tag float_tag
        fi
    done
}) &

# Don't manage dzen2 and the like
hc rule windowtype~'_NET_WM_WINDOW_TYPE_(NOTIFICATION|DOCK|DESKTOP)' manage=off

# Move IM shizzle to IM tag
hc rule class~'Skype|Slack|Franz' tag=im
# Move browsers to 'web' tag
hc rule instance~'Navigator|Opera' tag=web
# Turn off default pseudotiling for some programs
hc rule class~'Firefox|Opera|chromium|luakit|qutebrowser|Thun(ar|derbird)|Vlc|mpv|feh|Deluge|Minecraft.*|libreoffice*|Franz|Zeal' \
    pseudotile=off

# Start on 'terms' tag
hc use 3
