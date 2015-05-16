# Colorized output ^_^
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias gr='grep --color=always'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# ls shortcuts
alias ll='ls -AlFv'
alias l='ls -Fv'

# Easier directory jumping
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# Frequently used commands
alias vi='/usr/bin/vim'
alias rm='rm -I'
alias ipy='ipython && exit'
alias hig='history | grep -i'
alias todo='grep -r TODO .'
alias g='git'
alias hc='herbstclient'
alias open='xdg-open'
alias fuck='$(thefuck $(fc -ln -1))'
alias eduroam='sudo nmcli --ask c up eduroam'
alias yupdate='yaourt -Syua'
function yupdatef() {
    set -e
    yaourt -Syua --noconfirm
    paccache -r
    $HOME/repos/dotfiles/setup.sh
    yaourt -C
}
function psg() {
    ps auxww | grep -i --color=always $* | grep -v grep \
        | sed 's/\s\+/\t/g' | cut -f 2,11- | sed 's/\t/ /g2'
}
function cl() {
    dir="${1-$HOME}"
    cd "${dir}" && ls -l
}

# Quick cd
alias bu='cd ~/Documenten/bucht'
alias cdm='cd ~/repos/the-little-monks'
alias euler='cd ~/Dropbox/projects/project_euler/'
alias code_eval='cd ~/Dropbox/projects/code_eval/'
alias rosalind='cd ~/Dropbox/projects/rosalind/'
