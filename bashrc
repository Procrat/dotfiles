# Non-bash-specific settings and env variables
source $HOME/.shellrc

# Don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=2000
HISTFILESIZE=2000

# I want to know when I executed my commands
HISTTIMEFORMAT="%F %T "

# Don't make me type `cd`
shopt -s autocd
# Spelling correction of directory names
shopt -s cdspell dirspell
# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# Include dotfiles in filename expansion
shopt -s dotglob
# Let me use **
shopt -s globstar
# Append history instead of overwriting
shopt -s histappend
# Case-insensitive filename expansion
shopt -s nocaseglob

# Set prompt
PS1='\[\033[01;32m\]\u\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
