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

# Load Xresources
xrdb "$HOME/.Xresources"

# Set Xft.dpi to roughly the same as the screen's DPI which is assumed to be
# set correctly by either the X config or xrandr.
dpi="$(xdpyinfo | sed -n 's/\s*resolution:\s*\([0-9]\+\)x\1 dots per inch$/\1/p')"
echo "Xft.dpi: ${dpi}" | xrdb -merge

# Background
([ -s "$HOME/.fehbg" ] && bash "$HOME/.fehbg") &

# Set the minimum brightness of the screen to 1 instead of 0
light -N 1

# Lock screen on suspend and after 3 minutes of inactivity, dimming the screen
# after 2 minutes.
xset s 120 180
xss-lock --notifier=dim-screen --transfer-sleep-lock -- \
  lock-screen --nofork &

# Enable keyring
eval "$(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)"
export SSH_AUTH_SOCK

# Start unclutter daemon
unclutter --fork \
  --timeout 2 \
  --jitter 1 \
  --exclude-root \
  --ignore-scrolling \
  --start-hidden &

# Start lightweight composite manager
picom -b &

# Start daemon to automatically change colour temperature of screen
redshift &

# Start multitouch gesture daemon
touchegg &

# Start window manager
exec xmonad &>>"$XDG_RUNTIME_DIR/xsession-errors"
