zstyle ':omz:*' case-sensitive 'no'
zstyle ':omz:*' color 'yes'
zstyle ':omz:load' omodule 'environment' 'bindkey' 'completion' 'history' 'directory' 'alias' 'prompt' 'git' 'sprunge' 'tmux' 'pacman' 'pacaur'
zstyle ':omz:module:prompt' theme 'archey' '' '' 'n'

autoload omz && omz
# Customize to your needs...
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/opt/java/bin:/opt/java/db/bin:/opt/java/jre/bin:/usr/bin/core_perl:$HOME/bin:$HOME/.cabal/bin:$HOME/code/go/bin

export GOPATH=$HOME/code/go:$GOPATH

bin-exist() {[[ -x `which  $1 2>/dev/null` ]]}

export EDITOR='vim'
export PAGER='most'
bindkey -e

setopt SHARE_HISTORY

# safety features
setopt RM_STAR_WAIT
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

#addendum to git plugin
alias gd='git diff -w'
compdef _git gd=git-diff
gg() { git grep "$*"; }

#globals
alias -g F=' | fmt -'
alias -g M=' | most'
alias -g G=' | grep'
alias -g T=' | tail -n'
alias -g H=' | head -n'
alias -g C=' | xclip -selection c'
alias -g DN='/dev/null'
#alias -g sprunge='| curl -F "sprunge=<-" http://sprunge.us'

#systemd
alias sdc='sudo systemctl'
compdef _sds='systemctl'
alias sds='sudo systemctl status'
compdef _sds='systemctl'
alias sdr='sudo systemctl restart'
compdef _sdr='systemctl'
alias sdls='sudo systemctl list-units'
compdef _sdls='systemctl'
alias sdj='sudo journalctl'
alias sdgrep='sudo systemctl list-unit-files | grep'
compdef _sdgrep='grep'

alias oss-fp='sudo ln -sf /dev/oss/oss_hdaudio0/pcm0 /dev/dsp'
alias oss-bp='sudo ln -sf /dev/oss/oss_hdaudio0/pcm1 /dev/dsp'

#suffixes
#TODO expand 
alias -s txt=vim
for i in rar zip 7z lzma; alias -s $i='7z x'

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias diff='colordiff' 

alias tmux='tmux -u -2'
alias open='xdg-open'

alias v='vim --servername base --remote-silent'
compdef v='vim'
alias cleanvim='find ./ -type f -iname ".*.*sw*" -print0 | xargs --interactive -0 rm' # seems safer than find -delete
alias em='emacs -nw'

alias l='ls -hl '
alias la='ls -A'
alias ll='l -A'
alias lx='ll -BX'           # sort by extension
alias lz='ll -rS'           # sort by size
alias lt='ll -rt'           # sort by date
alias lsd='ls -d .*(/) *(/)'

alias ..='cd ..'
alias dud='du -s *(/)'
alias mkdir='mkdir -pv'

alias ping='ping -c 5'
(bin-exist mtr) && alias traceroute='mtr' #remember mtr!
alias openports='netstat --all --numeric --programs --inet'

alias reload='source ~/.zshrc'
alias reboot='sudo shutdown -r now'
alias halt='sudo shutdown -h now'

alias f='find | grep'
alias c='clear'

function sshtunnel {
    ssh -ND $2 -v $1
}
compdef sshtunnel='ssh' 

function 7z2 {
    7z e -o$2 $1
}

alias rsync='rsync --progress'
alias rsync-ntfs='rsync -av --modify-window=1'

insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo  #makes alt-s insert-sudo

setopt extended_glob

preexec () {
    if [[ "$TERM" == "screen" ]]; then
	local CMD=${1[(wr)^(*=*|sudo|-*)]}
	echo -ne "\ek$CMD\e\\"
    fi
}

sdate() { date +%m.%d.%Y }

#query wikipedia for short txt record
wikidig() { dig +short txt ${1}.wp.dg.cx }
#prepend precise timestamps to tail
tailTS() { tail -f ${1} | while read; do echo -e "$(date +%T.%N) $REPLY"; done }
