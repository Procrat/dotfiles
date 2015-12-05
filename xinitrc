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
numlockx &

# Background
bash $HOME/.fehbg &

# Xresources otherwise gets loaded async, which might fuck up some stuff
xrdb "$HOME/.Xresources"

# Screenlocker
xautolock -locker i3lock-fancy -corners 0+0- -detectsleep &

# Enable keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

# Launch some programs
opera &
skype &
