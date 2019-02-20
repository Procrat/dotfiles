# In less, parse colors, show longer prompt
export LESS='-R -F -M -W -i'
export PAGER='less'

# Editor settings
has_nvim="$(which nvim 2>/dev/null)"
if [[ -n "$has_nvim" ]]; then
    export EDITOR=nvim
    export VISUAL=nvim
fi

export PACMAN=powerpill

# Set the path to the Rust source (for use in Vim)
export RUST_SRC_PATH="$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"

export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.cargo/bin:./node_modules/.bin:$PATH"

# For urxvt, the first 16 colours are already set through ~/.Xresources.
# This also makes it work in TTYs and with more colours.
export BASE16_THEME='base16-mocha'
BASE16_SHELL="$HOME/.config/base16-shell/scripts/${BASE16_THEME}.sh"
[[ -s "$BASE16_SHELL" ]] && source "$BASE16_SHELL"

# If we don't have a display server running yet, and this is the first virtual
# console, start a graphical environment here. This allows us to still have
# non-graphical virtual consoles next to TTY1 in case something goes wrong with
# our display manager or window manager.
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx


# vim:filetype=sh
