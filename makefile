SOURCES=$(wildcard *) 
SOURCES=$(patsubst %readme.md,,$(SOURCES))
# TODO special handling or refactor zsh prompt
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
