EMACS   ?= emacs
CASK    ?= cask
VIRTUAL := $(CASK) exec $(EMACS)
BATCH   := $(VIRTUAL) -batch -Q -L .

PACKAGE := impatient-mode
VERSION := $(shell $(CASK) version)

EL = impatient-mode.el
ELC = $(EL:.el=.elc)
EXTRA_DIST = README.md loading.html jquery.js index.html

.PHONY : all compile package clean

all : compile package

.cask : Cask
	cask install
	touch .cask

compile: .cask $(ELC)

package : $(PACKAGE)-$(VERSION).tar

$(PACKAGE)-pkg.el : Cask
	$(CASK) package

$(PACKAGE)-$(VERSION).tar : $(EL) $(PACKAGE)-pkg.el $(EXTRA_DIST)
	tar -cf $@ --transform "s,^,$(PACKAGE)-$(VERSION)/," $^

clean:
	$(RM) *.tar *.elc $(PACKAGE)-pkg.el

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<
