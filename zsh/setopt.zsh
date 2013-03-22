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

# directory
setopt auto_cd \
    auto_pushd \
    auto_name_dirs \
    cdable_vars       \
    pushd_ignore_dups \
    pushd_silent \
    pushd_to_home \
    rm_star_wait

# expansion and globbing
setopt brace_ccl \
    extended_glob \
    long_list_jobs \
    multios \
    prompt_subst

# history
setopt \
     inc_append_history \
     append_history \
     bang_hist \
     extended_history \
     no_hist_allow_clobber \
     no_hist_beep \
     hist_expire_dups_first \
     hist_find_no_dups \
     no_hist_ignore_dups \
     hist_ignore_space \
     no_hist_no_functions \
     hist_no_store \
     hist_reduce_blanks \
     hist_save_by_copy \
     hist_save_no_dups \
     hist_verify \
     share_history 
# input/output
setopt correct \
    interactive_comments \
    rc_quotes \
    short_loops
unsetopt clobber \
    flowcontrol

# job control
setopt bg_nice

# smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# zle
unsetopt beep
