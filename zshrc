# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="reprisal"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# export DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git archlinux)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/opt/java/bin:/opt/java/db/bin:/opt/java/jre/bin:/usr/bin/core_perl:/home/reprisal/bin

bin-exist() {[[ -x `which  $1 2>/dev/null` ]]}

export EDITOR='vim'
export PAGER='most'

setopt SHARE_HISTORY

# safety features
setopt RM_STAR_WAIT
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I'                    # 'rm -i' prompts for every file
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

#addendum to zsh git plugin
alias gd='git diff'
compdef _git gd=git-diff

#globals
alias -g F=' | fmt -'
alias -g M=' | most'
alias -g G=' | grep'
alias -g T=' | tail -n'
alias -g H=' | head -n'
alias -g DN='/dev/null'
alias -g sprunge='| curl -F "sprunge=<-" http://sprunge.us'

#suffixes
#TODO expand 
alias -s txt=vim
for i in rar zip 7z lzma; alias -s $i='7z x'
alias -s txt=vim

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias diff='colordiff' 

alias tmux='tmux -u -2'
alias open='xdg-open'

alias cleanvim='find ./ -type f -iname ".*.*sw*" -print0 | xargs --interactive -0 rm' # seems safer than find -delete

# yaourt helpers -replaced by archlinux plugin for now
# alias yaoi='yaourt -Si'
# alias yaoins='yaourt -S'
# alias yaos='yaourt -Ss'
# alias yaoup='yaourt -Syu'
# alias yaoupa='yaourt -Syua' 

alias ls='ls -F --color=always --group-directories-first'
alias lr='ls -R'                    # recursive ls
alias l='ls -hl '
alias la='ls -A'
alias ll='l -A'
alias lx='ll -BX'                   # sort by extension
alias lz='ll -rS'                   # sort by size
alias lt='ll -rt'                   # sort by date
alias lm='ll | most'
alias lsd='ls -d .*(/) *(/)'

alias ..='cd ..'
alias df='df -Th'
alias du='du -c -h'
alias dud='du -s *(/)'
alias mkdir='mkdir -pv'

# alias screen='byobu'
alias tmux='tmux -u -2'
alias v=vim --remote-silent
alias grep='grep --color=auto'
alias diff='colordiff' 

alias ping='ping -c 5'
(bin-exist mtr) && alias traceroute='mtr' #remember mtr!
alias openports='netstat --all --numeric --programs --inet'

alias reload='source ~/.zshrc'
alias reboot='sudo shutdown -r now'
alias halt='sudo shutdown -h now'

alias f='find | grep'
alias c='clear'

export SSH_PORT=22335
alias sshmini='ssh -p $SSH_PORT william@192.168.1.135'
alias sshtunnel="ssh -ND $SSH_PORT -v"

function secure_chromium {
    export SOCKS_SERVER=localhost:$SSH_PORT
    export SOCKS_VERSION=5
    chromium &
    exit
}

export DIRSTACKSIZE=10
alias dh='dirs -v'
setopt autopushd pushdminus pushdtohome

alias rsync='rsync --progress'

insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo  #makes alt-s insert-sudo!

gg() { git grep "$*"; }

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

#better than nocorrect sudo, because corrects file names, correct_ignore didnt seem to work
alias sdvim='sudo /usr/bin/vim' 
