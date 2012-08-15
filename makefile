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
	$(foreach file, $^, rm -i $(HOME)/.$(file) ; )

destroy: $(SOURCES)
	$(foreach file, $^, rm -rI $(HOME)/.$(file) ; )

# TODO reduce safety of clean & destroy?
