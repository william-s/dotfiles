SOURCES=$(wildcard *) 
# TODO ignore readme, special handling or refactor zsh prompt


all: link submodules

link: $(SOURCES)
	$(foreach file, $^, ln -s $(CURDIR)/$(file) ~/.; )

submodules:
	git submodule init
	git submodule update

echo:
	@echo $(SOURCES)

clean: $(SOURCES)
	@echo "WARNING: The search may accidently include other symlinks that include dotfile names as a substring"
	@echo
	$(foreach file, $^, rm -i `find $(HOME) -maxdepth 1 -lname "*$(file)" -print` ;)
	# if only find could handle special characters!
	# $(foreach file, $^, find $(HOME) -lname '.$(file)' -delete \; )

destroy: $(SOURCES)
	$(foreach file, $^, rm -rI $(HOME)/.$(file) ; )

# TODO is there a better way to clean? make uninteractive versions?
