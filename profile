# In less, parse colors, show longer prompt
export LESS='-R -F -M -W -i'
export PAGER='less'

# Editor settings
has_nvim="$(which nvim 2>/dev/null)"
if [[ -n "$has_nvim" ]]; then
    export EDITOR=nvim
    export VISUAL=nvim
fi

export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.cargo/bin:./node_modules/.bin:$PATH"

export RESTIC_PASSWORD_COMMAND='pass show restic'

# If we don't have a display server running yet, and this is the first virtual
# console, start a graphical environment here. This allows us to still have
# non-graphical virtual consoles next to TTY1 in case something goes wrong with
# our display manager or window manager.
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx


# vim:filetype=sh
