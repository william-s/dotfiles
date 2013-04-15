bin-exist() {[[ -x `which  $1 2>/dev/null` ]]}

#globals
alias -g F=' | fmt -'
alias -g M=' | most'
alias -g G=' | grep'
alias -g T=' | tail --lines'
alias -g H=' | head --lines'
alias -g C=' | xclip -selection c'
alias -g DN='/dev/null'
alias -g PB='| curl --form "sprunge=<-" http://sprunge.us'

alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

#suffixes
#TODO expand 
alias -s txt=vim
for i in rar zip 7z lzma; alias -s $i='7z x'

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias diff='colordiff' 

alias ls='ls --color=auto'
alias l='ls -l  --human-readable'
alias la='ls --almost-all'
alias ll='l --almost-all'
alias lx='ll -BX'           # sort by extension
alias lz='ll -S --reverse'           # sort by size
alias lt='ll -t --reverse'           # sort by date
alias lsd='ls --directory *(/)'

alias tmux='tmux -u -2'
alias open='xdg-open'

alias v='vim --servername base --remote-silent'
compdef v='vim'
alias svim='sudo vim'
alias cleanvim='find ./ -type f -iname ".*.*sw*" -print0 | xargs --interactive -0 rm' # seems safer than find -delete
alias em='emacs --no-window-system'

alias g='git'
compdef g='git'

alias ..='cd ..'
alias du='du -k --human-readable'
alias df='df -k --human-readable'
alias dud='du --summarize *(/)'
alias mkdir='mkdir --parents --verbose'
alias d='dirs -v'

alias ping='ping -c 5'
(bin-exist mtr) && alias traceroute='mtr' #remember mtr!
alias openports='netstat --all --numeric --programs --inet'

alias reload='source ~/.zshrc'
alias reboot='sudo shutdown -r now'
alias halt='sudo shutdown -h now'

alias f='find | grep'
alias c='clear'

alias sshkey='eval $(/usr/bin/keychain --eval --agents ssh --quick --quiet ~/.ssh/id_rsa)'

alias rsync='rsync --progress -h'
alias rsync-ntfs='rsync --archive --verbose --modify-window=1'

#systemd
alias sdc='sudo systemctl'
compdef _sds='systemctl'
alias sds='sudo systemctl status'
compdef _sds='systemctl'
alias sdr='sudo systemctl restart'
compdef _sdr='systemctl'
alias sdls='sudo systemctl list-units'
compdef _sdls='systemctl'
alias sdj='journalctl' #add user to adm group to use journal w/o sudo
alias sdgrep='sudo systemctl list-unit-files | grep'
compdef _sdgrep='grep'

# pacaur helpers
alias pacs='pacaur -Ss'
alias paci='pacaur -Sii'
alias pacq='pacaur -Qs'
alias pacqi='pacaur -Qii'
alias pacd='pacaur -d'
alias pacy='pacaur -Sy'
alias pacc='pacaur -Qu'
alias pacu='pacaur -Syu'
# vim: set ft=zsh:
