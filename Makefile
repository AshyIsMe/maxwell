.PHONY: build test clean

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

clean:
	$(RM) maxwell
