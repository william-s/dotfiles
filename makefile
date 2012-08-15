SOURCES=$(wildcard *) 
# TODO ignore readme, special handling or refactor zsh prompt
# TODO is there a better way to clean? make uninteractive versions?
# if only find could handle special characters!
# $(foreach file, $^, find $(HOME) -lname '.$(file)' -delete \; )


all: link submodules

link: $(SOURCES)
	$(foreach file, $^, ln -s $(CURDIR)/$(file) ~/.$(file); )

submodules:
	git submodule init
	git submodule update

echo:
	@echo $(SOURCES)

clean: $(SOURCES)
	@echo "*** Notice: confirm that each file is a symlink! ***"
	@echo 
	$(foreach file, $^, rm -i '/home/william/.$(file)' ; )

destroy: $(SOURCES)
	$(foreach file, $^, rm -rI $(HOME)/.$(file) ; )

