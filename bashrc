# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

PS1='\[\033[01;32m\]\u\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

eval "$(dircolors -b ~/.solarized/dircolors.ansi-dark)"

. ~/.bash_aliases

export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim
# Disable Ctrl+S for scrolling so we can use it for the save shortcut in vim
stty -ixon

# Add ruby gem path to PATH
RUBY_HOME=$(ruby -rubygems -e "puts Gem.user_dir")/bin
export PATH="${PATH}:~/bin:${RUBY_HOME}:"

# Shell options
shopt -s autocd cdspell dirspell

# Better font rendering in Java
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

# Make Bundler install gems per user
export GEM_HOME=$(ruby -e 'puts Gem.user_dir')
