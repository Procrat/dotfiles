#!/bin/bash
#
# Key and mouse bindings for HLWM


script_dir=$(dirname $BASH_SOURCE)

hc() { herbstclient "$@" ; }


# Remove all existing keybindings
hc keyunbind --all

# Main modifier
Mod=Mod4   # Use the super key as the main modifier

# HLWM
hc keybind $Mod-Shift-q spawn poweroff
hc keybind $Mod-Shift-r reload

# Launchers
hc keybind $Mod-Return       spawn urxvtc -e tmux
hc keybind $Mod-c            spawn urxvtc -e zsh -c ". $HOME/bin/choose_session_outside_of_tmux"
hc keybind $Mod-Shift-Return spawn xterm  # Fallback
hc keybind $Mod-t            spawn j4-dmenu-desktop --dmenu="$HOME/bin/mydmenu -q -f" --term="urxvtc"
hc keybind $Mod-e            spawn urxvtc -e ranger

# Focus
hc keybind $Mod-Left        focus left
hc keybind $Mod-Down        focus down
hc keybind $Mod-Up          focus up
hc keybind $Mod-Right       focus right
hc keybind $Mod-h           focus left
hc keybind $Mod-j           focus down
hc keybind $Mod-k           focus up
hc keybind $Mod-l           focus right
hc keybind $Mod-BackSpace   cycle_monitor
hc keybind $Mod-Tab         cycle_all +1
hc keybind $Mod-Shift-Tab   cycle_all -1
hc keybind $Mod-i           jumpto urgent

# Layout
## Moving windows around
hc keybind $Mod-Shift-Left  shift left
hc keybind $Mod-Shift-Down  shift down
hc keybind $Mod-Shift-Up    shift up
hc keybind $Mod-Shift-Right shift right
hc keybind $Mod-Shift-h     shift left
hc keybind $Mod-Shift-j     shift down
hc keybind $Mod-Shift-k     shift up
hc keybind $Mod-Shift-l     shift right
## Splitting frames
hc keybind $Mod-u       split   bottom  0.5
hc keybind $Mod-o       split   right   0.5
## Let the current frame explode into subframes
hc keybind $Mod-Control-space split explode
## Resizing frames
resizestep=0.05
hc keybind $Mod-Control-h       resize left +$resizestep
hc keybind $Mod-Control-j       resize down +$resizestep
hc keybind $Mod-Control-k       resize up +$resizestep
hc keybind $Mod-Control-l       resize right +$resizestep
hc keybind $Mod-Control-Left    resize left +$resizestep
hc keybind $Mod-Control-Down    resize down +$resizestep
hc keybind $Mod-Control-Up      resize up +$resizestep
hc keybind $Mod-Control-Right   resize right +$resizestep
## More layout management shizzle
hc keybind $Mod-r     close_and_remove
hc keybind $Mod-space cycle_layout 1
hc keybind $Mod-s     floating toggle
hc keybind $Mod-z     fullscreen toggle
hc keybind $Mod-p     pseudotile toggle

# Tags
## Cycle through tags
hc keybind $Mod-f         use_index +1 --skip-visible
hc keybind $Mod-d         use_index -1 --skip-visible
hc keybind $Mod-quoteleft use_previous
## Move windows through tags
hc keybind $Mod-Shift-f   move_index +1 --skip-visible
hc keybind $Mod-Shift-d   move_index -1 --skip-visible


# Mouse bindings (for floating mode)
hc mouseunbind --all
hc mousebind $Mod-Button1 move
hc mousebind $Mod-Button2 zoom
hc mousebind $Mod-Button3 resize


# Media key bindings (this probably shouldn't be defined here)
hc keybind XF86AudioRaiseVolume  spawn $script_dir/dzen/dvolume.sh -i 2
hc keybind XF86AudioLowerVolume  spawn $script_dir/dzen/dvolume.sh -d 2
hc keybind XF86AudioMute         spawn $script_dir/dzen/dvolume.sh -t
hc keybind XF86AudioPlay         spawn mpc toggle
hc keybind XF86MonBrightnessUp   spawn $script_dir/dzen/dbrightness.sh +15
hc keybind XF86MonBrightnessDown spawn $script_dir/dzen/dbrightness.sh -10
