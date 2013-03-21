setopt prompt_subst
autoload -U vcs_info


zstyle ':vcs_info:*'              enable        git #hg bzr cvs svn
zstyle ':vcs_info:*'              actionformats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*'              formats       '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat  '%b%F{1}:%F{3}%r'


PROMPT=$'%{$fg[yellow]%}%n%{$fg_bold[blue]%}@%m %{$reset_color%}[%~] $(git_prompt_info)\
%{$fg_bold[blue]%}%? %(!.#.λ)%{$reset_color%} '


GIT_CLEAN_COLOR="%{$fg[blue]%}"
GIT_DIRTY_COLOR="%{$fg[red]%}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="$GIT_CLEAN_COLOR ★"
ZSH_THEME_GIT_PROMPT_DIRTY="$GIT_DIRTY_COLOR χ"
