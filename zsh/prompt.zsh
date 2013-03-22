setopt prompt_subst
autoload -Uz add-zsh-hook
autoload -Uz vcs_info

zstyle ':vcs_info:*'              enable        git #hg bzr cvs svn
zstyle ':vcs_info:*'              actionformats '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*'              formats       '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat  '%b%F{1}:%F{3}%r'
zstyle ':vcs_info:*' check-for-changes true

local cmd_status prompt_name host_color
cmd_status="%B%(?,%F{green}▲%f,%F{magenta}▼%f)%b"
prompt_name="%(!,%F{red},%F{cyan})%n%f"
if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
    host_color=yellow
else
    host_color=blue
fi


PROMPT="${prompt_name}@%F{${host_color}}%m%f [%~] $(git_prompt_info)
${cmd_status} %B%(!,%F{red}#,%F{blue}λ)%f%b "


GIT_CLEAN_COLOR="%{$fg[blue]%}"
GIT_DIRTY_COLOR="%{$fg[red]%}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="$GIT_CLEAN_COLOR ★"
ZSH_THEME_GIT_PROMPT_DIRTY="$GIT_DIRTY_COLOR χ"
