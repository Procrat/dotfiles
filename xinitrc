#!/bin/sh
#
# Executed by
#  - startx
#  - gdm (through .xprofile)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

export _JAVA_AWT_WM_NONREPARENTING=1

# Load Xresources
xrdb $HOME/.Xresources &

# For some fucked up reason I have the wrong keyboard layout
# and, yes, my localectl is set correctly
setxkbmap be &

# Background
bash $HOME/.fehbg &

# Screenlocker
xautolock -locker i3lock-fancy -corners 0+0- -detectsleep &

# Launch some programs
thunderbird &
firefox &
