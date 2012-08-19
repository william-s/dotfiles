ALLFILES=$(wildcard *) 
FILES=$(patsubst %readme.md,,$(ALLFILES))
SOURCES=$(patsubst %makefile,,$(FILES))
# TODO special handling or refactor zsh prompt
# TODO link is creating folder links in vim/vim and colors/colors
# TODO is there a better way to clean? make uninteractive versions?
# if only find could handle special characters!
# $(foreach file, $^, find $(HOME) -lname '.$(file)' -delete \; )


all: link submodules

link: $(SOURCES)
	$(foreach file, $^, ln -si $(CURDIR)/$(file) ~/.$(file); )

submodules:
	git submodule init
	git submodule update

echo:
	@echo $(SOURCES)

clean: $(SOURCES)
	@echo "*** Notice: confirm that each file is a symlink! ***"
	@echo 
	$(foreach file, $^, rm -i $(HOME)/.$(file) ; )

destroy: $(SOURCES)
	$(foreach file, $^, rm -I $(HOME)/.$(file) ; )
