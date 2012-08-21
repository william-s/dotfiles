#My dotfiles

Primarily for my own use.  *Sanity check* recommended before use.

To install
* Clone the repo
* run `make` to link the files and install submodules
* to uninstall or remove conflicting symlinks use `make clean` 
* `make destroy` runs clean and then prompts whether to remove conflicting files or directories
* xmonad config depends on `cabal install xmonad xmonad-contrib`

Known issues
* my omz set up and prompt are not handled by script
