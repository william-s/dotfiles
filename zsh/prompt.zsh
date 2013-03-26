autoload -Uz add-zsh-hook
autoload -Uz vcs_info

zstyle ':vcs_info:*'              enable        git #hg bzr cvs svn
zstyle ':vcs_info:*' formats 'on %F{m}%b%c%u%m%F{n}'
zstyle ':vcs_info:*' actionformats "%b%c%u|%F{c}%a%f"
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b|%F{c}%r%f'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:git*+set-message:*' hooks git-status
zstyle ':vcs_info:*' stagedstr '%F{g}●%f'
zstyle ':vcs_info:*' unstagedstr '%F{y}●%f'


function prompt_precmd() {
    vcs_info

    local cmd_status prompt_name host_color
    cmd_status="%(?,%F{green}★%f,%B%F{magenta}χ%b%f)"
    prompt_name="%(!,%F{red},%F{cyan})%n%f"


    if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
        host_color=yellow
    else
        host_color=blue
    fi
    
    PROMPT="${prompt_name}@%F{${host_color}}%m%f [%~]
${cmd_status} %B%(!,%F{red}#,%F{blue}λ)%f%b "
    RPROMPT="${vcs_info_msg_0_}"

}

add-zsh-hook precmd prompt_precmd
