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

# Background
([ -s "$HOME/.fehbg" ] && bash "$HOME/.fehbg") &

# Start autorepeat after .3s and repeat 55/s
xset r rate 300 55 &

# Xresources otherwise gets loaded async, which might fuck up some stuff
xrdb "$HOME/.Xresources"

# Screenlocker
xautolock -locker "$HOME/bin/i3lock" -corners 0+0- -detectsleep &

# Enable keyring
eval "$(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)"
export SSH_AUTH_SOCK

# Start urxvt daemon (This is preferred above systemd)
urxvtd -q -o -f &

# Start unclutter daemon
unclutter &

# Start lightweight composite manager
compton -b &

# Start tracking me
rescuetime &

exec xmonad &>>"$XDG_RUNTIME_DIR/xsession-errors"
