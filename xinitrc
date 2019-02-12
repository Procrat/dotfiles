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

# Read environment variables for graphical programs
. "$HOME/.profile"

# Background
([ -s "$HOME/.fehbg" ] && bash "$HOME/.fehbg") &

# Start autorepeat after .3s and repeat 55/s
xset r rate 300 55 &

# Xresources otherwise gets loaded async, which might fuck up some stuff
xrdb "$HOME/.Xresources"

# Screenlocker
xautolock -locker "$HOME/bin/i3lock" -corners 0+0- &

# Enable keyring
eval "$(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)"
export SSH_AUTH_SOCK

# Start urxvt daemon (This is preferred above systemd)
urxvtd -q -o -f &

# Start unclutter daemon
unclutter &

# Start lightweight composite manager
compton -b &

redshift &

# exec herbstluftwm --locked \
# exec gnome-shell \
exec "$HOME/.xmonad/xmonad-x86_64-linux" \
  &>>"$XDG_RUNTIME_DIR/xsession-errors"
