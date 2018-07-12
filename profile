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
# Pointer to Java JDK
export JAVA_HOME="/usr/lib/jvm/default"
# Add ruby gem path to PATH and make Bundler install gems per user
export GEM_HOME="$HOME/.gem/ruby/2.4.0"

export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.cargo/bin:$JAVA_HOME/bin:$GEM_HOME/bin:$PATH"

# Set user virtualenv if unset and add to path if it exists
if [[ -z "$VIRTUAL_ENV" && -d "$HOME/.venv" ]]; then
    export VIRTUAL_ENV="$HOME/.venv"
    export PATH="$VIRTUAL_ENV/bin:$PATH"
fi


# vim:filetype=sh
