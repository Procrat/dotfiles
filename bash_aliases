# Colorized output ^_^
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias gr='grep --color=always'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# ls shortcuts
alias ll='ls -AlhFv'
alias l='ls -Fv'

# Easier directory jumping
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# Frequently used commands
alias vi='/usr/bin/vim'
alias rm='rm -I'
alias ipy='exec ipython'
alias hig='history | grep -i'
alias todo='grep -r TODO .'
alias g='git'
alias hc='herbstclient'
alias open='xdg-open'
alias fuck='$(thefuck $(fc -ln -1))'
alias eduroam='sudo nmcli --ask c up eduroam'
alias yupdate='yaourt -Syua'
alias highlight='pygmentize'
yupdatef() {
    yaourt -Syua --noconfirm
    paccache -r
    $HOME/repos/dotfiles/setup.sh
    yaourt -C
}
psg() {
    ps auxww | grep -i --color=always $* | grep -v grep \
        | sed 's/\s\+/\t/g' | cut -f 2,11- | sed 's/\t/ /g2'
}
cl() {
    dir="${1-$HOME}"
    cd "${dir}" && ls -l
}
man() {
    env \
        LESS_TERMCAP_mb=$'\E[5m' \
        LESS_TERMCAP_md=$'\E[32m' \
        LESS_TERMCAP_me=$'\E[0m' \
        LESS_TERMCAP_so=$'\E[3m\E[48;5;235m' \
        LESS_TERMCAP_se=$'\E[0m' \
        LESS_TERMCAP_us=$'\E[3m' \
        LESS_TERMCAP_ue=$'\E[0m' \
        /usr/bin/man "$@"
}

# Quick cd
alias bu='cd ~/Documenten/bucht'
alias cdm='cd ~/repos/the-little-monks'
alias euler='cd ~/Dropbox/projects/project_euler/'
alias code_eval='cd ~/Dropbox/projects/code_eval/'
