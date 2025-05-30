# Non-zsh settings and env variables
source $HOME/.shellrc

# See .preztorc for most of the configuration
# If it isn't installed system wide, load user installation
[[ -d '/usr/lib/prezto' ]] || source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"

# Follow symlinks when cd'ing
setopt chase_links
# Allow comments
setopt interactive_comments

# Prompt
setopt PROMPT_SUBST
login_info() {
    if [[ -n "$SSH_CONNECTION" ]]; then
        echo '%n@%m:'
    fi
}
dir_info() {
    # Show pwd info in blue if writable, else in cyan
    if [[ -w "$PWD" ]]; then
        echo '%B%F{blue}%~%f%b'
    else
        echo '%B%F{cyan}%~%f%b'
    fi
}
[[ -f /usr/share/git/git-prompt.sh ]] && source /usr/share/git/git-prompt.sh
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM=(verbose)
GIT_PS1_STATESEPARATOR=''
git_info() {
    # Call the prompt function form /usr/share/git/git-prompt.sh
    type __git_ps1 2>&1 >/dev/null && __git_ps1 " (%s)"
}
virtualenv_info() {
    # Show wether we're in a virtualenv by adding a "v" to the dir
    if [[ -n "$VIRTUAL_ENV" ]]; then
        echo ' %B%F{green}v%f%b'
    fi
}
terraform_info() {
    if [[ -d ".terraform/" ]]; then
        echo ' (%F{magenta}'$(terraform workspace show)'%f)'
    fi
}
battery_info() {
    # Show when we're below 5% charge
    local battery=/sys/class/power_supply/BAT0
    local capacity="$(cat $battery/capacity)"
    if [[ "$capacity" -le 5 && "$(cat $battery/status)" = 'Discharging' ]]; then
        echo " %B%F{red}$capacity%%%f%b"
    fi
}
job_info() {
    # Show amount of background jobs if any
    echo '%(1j.(%j job%(2j.s.)) .)'
}
prompt() {
    local prompt_color=$(
        if [[ "$KEYMAP" == 'vicmd' ]]; then
            echo 'blue'
        else
            echo 'yellow'
        fi
    )
    # Show smiley face when all went well or suprised face on non-zero exit code
    echo '%B%(!,#,%(?;%F{'"$prompt_color"'}^.^;%F{red}o.0)%f)%b '
}
PROMPT='$prompt_newline$(login_info)$(dir_info)$(git_info)$(virtualenv_info)'\
'$(terraform_info)$prompt_newline$(job_info)$(prompt)'
# Optionally show error code in right hand side prompt
RPROMPT='%(?..%F{red}%?%f)$(battery_info)'
# Disable virtualenv prompt because we just included our own
export VIRTUAL_ENV_DISABLE_PROMPT=plzdont

# Global aliases
alias -g L="| ${PAGER:-less}"
alias -g H='| head'
alias -g T='| tail'
alias -g TF='| tail -f'
alias -g S='| sort'
alias -g F='| sort | uniq -c'
alias -g G='| rg -S'
alias -g C='| columns -t'
alias -g SUM='| awk '\''{ x += $0 } END { print x }'\'
alias -g sprunge='| curl -F "sprunge=<-" http://sprunge.us'

# Mappings
bindkey '^R' history-incremental-search-backward
bindkey -M viins fd vi-cmd-mode
bindkey '^A' beginning-of-line

# Add fzf keybindings (ctrl-t, ctrl-r and alt-c)
source /usr/share/fzf/key-bindings.zsh
# Add fzf '**' completion (files, directories, process ID's, SSH hostnames)
source /usr/share/fzf/completion.zsh

# Add git-extras completion
source /usr/share/doc/git-extras/git-extras-completion.zsh
