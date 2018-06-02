EMACS ?= emacs
MAKEFLAGS += k

.PHONY: test compile clean lint

compile:
	cask exec $(EMACS) -Q -batch \
	-L ./src/elisp \
	-f batch-byte-compile ./src/elisp/*.el; \

test:
	cask exec ert-runner --verbose --reporter ert --win && \
	cask exec ert-runner --verbose --reporter ert --no-win

clean:
	rm -f ./src/elisp/*.elc

lint: compile clean
