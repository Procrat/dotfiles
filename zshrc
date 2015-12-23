# Non-zsh settings and env variables
source $HOME/.shellrc

# See .preztorc for most of the configuration

# Follow symlinks when cd'ing
setopt chase_links
# Allow comments
setopt interactive_comments
# Implicit tees & cats with multiple redirections
setopt multios

# Git flow completion
if [[ -f /usr/share/git-flow/git-flow-completion.zsh ]]; then
    source /usr/share/git-flow/git-flow-completion.zsh
fi

# rbenv: adjust PATH and add completion for rbenv command
# Looks scary though :/
if which rbenv 2>/dev/null >&2; then
    eval "$(rbenv init -)"
fi

# Prompt
login_info() {
    if [[ -n "$SSH_CONNECTION" ]]; then
        echo '%n@%m:'
    fi
}
dir_info() {
    echo '%B%F{blue}%~%f%b'
}
prompt() {
    echo '%B%(!,#,%(?;%F{yellow}^.^;%F{red}o.0)%f)%b '
}
PROMPT=$(echo -e "\n$(login_info)$(dir_info)\n$(prompt)")
# Optionally show error code in right hand side prompt
RPROMPT="%(?..%F{red}%?%f)"

# Global aliases
alias -g L="| ${PAGER:-less}"
alias -g H='| head'
alias -g T='| tail'
alias -g TF='| tail -f'
alias -g S='| sort'
alias -g F='| sort | uniq -c'
alias -g G='| ag -i'

# Mappings
bindkey '^R' history-incremental-search-backward


# !! Keep this the last call of .zshrc!
if which tmux 2>/dev/null >&2; then
    # If we explicitly call .zshrc with some command, run it in tmux
    if [[ $# > 0 ]]; then
        tmux new-session "$@"
    # Start tmux if we're in an interactive, non-tmux environment
    elif [[ $- == *i* && -z "$TMUX" ]]; then
        tmux && exit
    fi
fi
