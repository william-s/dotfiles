autoload -U colors && colors
autoload -U compinit && compinit

# directory
setopt auto_cd \
    auto_pushd \
    auto_name_dirs \
    cdable_vars       \
    pushd_ignore_dups \
    pushd_silent \
    pushd_to_home \
    rm_star_wait

# smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# completion
setopt always_to_end \
    auto_menu \
    auto_name_dirs \
    auto_param_keys \
    auto_param_slash \
    auto_remove_slash \
    complete_aliases \
    complete_in_word \
    list_ambiguous \
    list_packed
unsetopt menu_complete

# expansion and globbing
setopt brace_ccl \
    extended_glob \
    long_list_jobs \
    multios \
    prompt_subst

# input/output
setopt correct \
    interactive_comments \
    rc_quotes \
    short_loops
unsetopt clobber \
    flowcontrol

# job control
setopt bg_nice

# zle
unsetopt beep
