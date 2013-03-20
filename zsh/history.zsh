HISTFILE=$HOME/.zsh/zhistory
HISTSIZE=10000
SAVEHIST=10000
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
