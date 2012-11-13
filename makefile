ALLFILES=$(wildcard *) 
FILES=$(patsubst %readme.md,,$(ALLFILES))
SOURCES=$(patsubst %makefile,,$(FILES))
# TODO special handling or refactor zsh prompt


all: link submodules pathogen 

link: $(SOURCES)
	$(foreach file, $^, ln -si $(CURDIR)/$(file) ~/.$(file); )

submodules:
	git submodule init
	git submodule update

pathogen:
	@mkdir -p vim/autoload
	@ln -s -t vim/autoload/ $(CURDIR)/vim/pathogen/autoload/pathogen.vim

echo:
	@echo $(SOURCES)
	@echo $(CURDIR)

# find with -lname .name wont work despite trying tons of tricks to get
# it to but this seems to effectively limit matches to only symlinks w/
# exact name matches
clean: $(SOURCES)
	$(foreach file, $^, find $(HOME) -name .$(file) -lname '*' -delete ; ) 
	@find $(CURDIR)/vim/autoload -name pathogen.vim -lname '*' -delete

destroy: clean $(SOURCES)
	$(foreach file, $^, find $(HOME) -maxdepth 1 \( -type d -o -type f \) -name .$(file) -print0 | xargs -r --interactive -0 rm -r ; )
