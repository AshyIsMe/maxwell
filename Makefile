PREFIX ?= $(HOME)/.local
BINDIR ?= $(PREFIX)/bin
DESTDIR ?=

.PHONY: build test clean install uninstall

build:
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl --non-interactive \
	     --eval '(require "asdf")' \
	     --eval '(asdf:load-asd (truename "maxwell.asd"))' \
	     --eval '(asdf:make "maxwell")'

test:
	XDG_CACHE_HOME=$(CURDIR)/.cache sbcl --non-interactive \
	     --eval '(require "asdf")' \
	     --eval '(asdf:load-asd (truename "maxwell.asd"))' \
	     --eval '(asdf:test-system "maxwell")'

install: build
	mkdir -p $(DESTDIR)$(BINDIR)
	install -m 755 maxwell $(DESTDIR)$(BINDIR)/maxwell

uninstall:
	$(RM) $(DESTDIR)$(BINDIR)/maxwell

clean:
	$(RM) maxwell
