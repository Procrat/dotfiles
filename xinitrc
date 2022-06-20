# Sourced by startx and some display managers

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Make Java GUIs work with non-reparenting window managers
export _JAVA_AWT_WM_NONREPARENTING=1

# Use Adwaita Dark theme for Qt applications
export QT_STYLE_OVERRIDE=adwaita-dark

# Set ask-pass utility (password dialog) for sudo
export SUDO_ASKPASS="$HOME/bin/zenity-ask-pass"

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
picom --experimental-backends -b &

# Start daemon to automatically change colour temperature of screen
redshift &

# Start multitouch gesture daemon
touchegg &

# Start window manager
# (Use direct path to XMonad build to ensure we don't recompile on boot.)
exec "$HOME/.cache/xmonad/xmonad-x86_64-linux" \
  &>>"$XDG_RUNTIME_DIR/xsession-errors"
