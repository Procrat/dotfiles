# Fix solarized for tmux
# TERM=xterm-16color

# Don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)HISTSIZE=1000
HISTFILESIZE=2000

# I want to know when I executed my commands
HISTTIMEFORMAT="%F %T "

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

PS1='\[\033[01;32m\]\u\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# eval "$(dircolors -b ~/.solarized/dircolors.ansi-dark)"

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/base16-default.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

. ~/.bash_aliases

export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim
# Disable Ctrl+S for scrolling so we can use it for the save shortcut in vim,
# but only for interactive shells of course
[[ $- == *i* ]] && stty -ixon

# Add ruby gem path to PATH
RUBY_HOME=$(ruby -rubygems -e "puts Gem.user_dir")/bin
export PATH="${PATH}:~/bin:${RUBY_HOME}:"

# Shell options
shopt -s autocd cdspell dirspell

# Better font rendering in Java
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

# Make Bundler install gems per user
export GEM_HOME=$(ruby -e 'puts Gem.user_dir')

# Open keyring
eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK


# !! Keep this the last call of .bashrc!
# If we explicitly call .bashrc with some command, run it in tmux
if [[ $# > 0 ]]; then
    tmux new-session "$@"
# Start tmux if we're in a top level shell
elif [[ $SHLVL == "1" ]]; then
    tmux && exit
fi
