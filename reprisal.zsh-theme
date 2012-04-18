PROMPT=$'%{$fg[yellow]%}%n%{$fg_bold[blue]%}@%m %{$reset_color%}[%~] $(git_prompt_info)\
%{$fg_bold[blue]%}%h %(!.#.λ)%{$reset_color%} '


GIT_CLEAN_COLOR="%{$fg[blue]%}"
GIT_DIRTY_COLOR="%{$fg[red]%}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="$GIT_CLEAN_COLOR ★"
ZSH_THEME_GIT_PROMPT_DIRTY="$GIT_DIRTY_COLOR χ"
