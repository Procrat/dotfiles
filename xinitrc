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
# Better font rendering in Java
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

# Enable Num Lock
(which numlockx 2>/dev/null >&2 && numlockx) &

# Background
([[ -s "$HOME/.fehbg" ]] && bash "$HOME/.fehbg") &

# Xresources otherwise gets loaded async, which might fuck up some stuff
xrdb "$HOME/.Xresources"

# Screenlocker
xautolock -locker "$HOME/bin/i3lock" -corners 0+0- -detectsleep &

# Enable keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

# Start urxvt daemon (This is preferred above systemd)
urxvtd &

# Start unclutter daemon
unclutter &

# Launch some programs
firefox-aurora &
franz &

exec herbstluftwm --locked
