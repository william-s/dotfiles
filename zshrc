zstyle ':omz:*' case-sensitive 'no'
zstyle ':omz:*' color 'yes'
zstyle ':omz:load' omodule 'environment' 'bindkey' 'completion' 'history' 'directory' 'alias' 'prompt' 'git' 'sprunge' 'tmux' 'pacman' 'pacaur'
zstyle ':omz:module:prompt' theme 'archey' '' '' 'n'

autoload omz && omz
# Customize to your needs...

export PATH=$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/code/go/bin:$HOME/.rvm/bin
export GOPATH=$HOME/code/go:$GOPATH

bin-exist() {[[ -x `which  $1 2>/dev/null` ]]}

export EDITOR='vim'
export PAGER='most'
bindkey -e

setopt SHARE_HISTORY

setopt RM_STAR_WAIT

#addendum to git plugin
alias gd='git diff -w'
compdef _git gd=git-diff
gg() { git grep "$*"; }


alias sshkey='eval $(/usr/bin/keychain --eval --agents ssh -Q --quiet ~/.ssh/id_rsa)'

function sshtunnel {
    ssh -ND $2 -v $1
}
compdef sshtunnel='ssh' 

function 7z2 {
    7z e -o$2 $1
}

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

source ~/.aliases
