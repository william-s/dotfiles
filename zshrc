export \
    PATH=$PATH:$HOME/bin:$HOME/.cabal/bin:$HOME/code/go/bin:$HOME/.rbenv/bin \
    GOPATH=$HOME/code/go:$GOPATH

HISTFILE=$HOME/.zsh/zhistory
HISTSIZE=10000
SAVEHIST=10000

autoload -U colors && colors
autoload -U compinit && compinit

export EDITOR='vim'
export PAGER='most'
bindkey -e

for f in $HOME/.zsh/*.zsh; do
    source $f
done

eval "$(rbenv init -)"


function sshtunnel {
    ssh -ND $2 -v $1
}
compdef sshtunnel='ssh' 

function 7z2 {
    7z e -o$2 $1
}

insert_sudo () { zle beginning-of-line; zle -U "sudo " }
zle -N insert-sudo insert_sudo
bindkey "^[r" insert-sudo  #makes alt-r insert-sudo

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
