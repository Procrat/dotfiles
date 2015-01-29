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
alias ...='cd ...'
alias ....='cd ....'
alias .....='cd .....'

# Frequently used commands
alias vi='/usr/bin/vim'
alias ipy='ipython && exit'
alias hig='history | grep -i'
alias todo='grep -r TODO .'
alias yupdate='yaourt -Syyua'
alias yupdatef='yaourt -Syyua --noconfirm && ~/repos/dotfiles/setup.sh && yaourt -C'
alias g='git'
function psg() {
    ps auxww | grep -i --color=always $* | grep -v grep \
        | sed 's/\s\+/\t/g' | cut -f 2,11
}

# Quick cd
alias bu='cd ~/Documenten/bucht'
alias cdm='cd ~/Dropbox/Liefje/TLM/the-little-monks'
alias euler='cd ~/Dropbox/projects/project_euler/'
alias code_eval='cd ~/Dropbox/projects/code_eval/'
