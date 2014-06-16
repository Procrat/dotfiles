# Colorized output ^_^
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias gr='grep --color=always'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# ls shortcuts
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Frequently used commands
alias vi='/usr/bin/vim'
alias psg='ps -ef | grep -i'
alias ipy='ipython --TerminalInteractiveShell.confirm_exit=False && exit'
alias hig='history | grep -i'
alias todo='grep -r TODO .'
alias yupdate='yaourt -Syyua'
alias yupdatef='yaourt -Syyua --noconfirm'

# Quick cd
alias bu='cd ~/Documenten/bucht'
alias cdm='cd ~/Dropbox/TLM/the-little-monks'
alias euler='cd ~/Dropbox/projects/project_euler/'
alias code_eval='cd ~/Dropbox/projects/code_eval/'

# SSH aliases
alias sshhelios='ssh svsegher@helios.ugent.be'
alias sshcc='ssh -p 2222 cursuscruisen-informatica@zeus.ugent.be'
alias sshzeus='ssh -p 2222 stijns@kelder.zeus.ugent.be'
alias sshstuw='ssh -p 2222 stuw@zeus.ugent.be'
alias sshpinnoo='ssh -p 2222 procrat@pinnoo.eu'
alias sshking='ssh -p 2222 root@zeus.ugent.be'

# GAE aliases
alias tlmup='appcfg.py -e "stijn.seghers@gmail.com" update ~/Dropbox/TLM/the-little-monks'
alias tlmdev='dev_appserver.py --log_level debug ~/Dropbox/TLM/the-little-monks/'
alias gup='appcfg.py -e "stijn.seghers@gmail.com" update ~/Dropbox/GD/gayducky/'
alias gdev='dev_appserver.py --log_level debug ~/Dropbox/GD/gayducky/'
alias asup='appcfg.py -e "stijn.seghers@gmail.com" update ~/Documenten/teacher-observation/'
alias asdev='dev_appserver.py --log_level debug ~/Documenten/teacher-observation'
