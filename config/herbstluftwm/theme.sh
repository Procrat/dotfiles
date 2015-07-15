#!/bin/bash
#
# Sets the look and feel of HLWM


hc() { herbstclient "$@" ; }

source $HOME/.colors


# Frame looks
# hc set always_show_frame 0
hc set frame_gap 0
hc set frame_padding 0
# hc set frame_border_active_color $BACKGROUND_HIGHLIGHT_COLOR
# hc set frame_border_normal_color $BACKGROUND_COLOR
# hc set frame_bg_active_color
# hc set frame_bg_normal_color
hc set frame_bg_transparent 1
hc set frame_transparent_width 0
hc set frame_border_width 0
hc set frame_border_inner_width 0
# hc set frame_border_inner_color
# hc set frame_active_opacity
hc set frame_normal_opacity 100

# Window looks
hc set window_gap 15
hc set window_border_width 1
hc set window_border_active_color $SECONDARY_CONTENT_COLOR
hc set window_border_normal_color $BACKGROUND_COLOR
hc set window_border_urgent_color $WARNING_COLOR
hc set window_border_inner_width 0
# hc set window_border_inner_color

# Floating mode
# hc set snap_distance
# hc set snap_gap
# hc set raise_on_focus
# hc set raise_on_focus_temporarily
# hc set raise_on_click
# hc set update_dragged_clients 1

# Other settings
# hc set mouse_recenter_gap
# hc set focus_crosses_monitor_boundaries
# hc set tree_style '╾│ ├└╼─┐'
hc set default_frame_layout 3
hc set_layout grid  # The above doesn't change the root frame
# hc set default_direction_external_only
# hc set gapless_grid 1
hc set smart_frame_surroundings 1
hc set smart_window_surroundings 0
# hc set focus_follows_mouse
hc set focus_stealing_prevention 0
hc set swap_monitors_to_get_tag 1
# hc set auto_detect_monitors 1
# hc set wmname
# hc set pseudotile_center_threshold
