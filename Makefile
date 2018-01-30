EMACS ?= emacs

compile:
	cask exec $(EMACS) -Q -batch \
	-L ./src/elisp \
	--eval '(setq byte-compile-error-on-warn t)' \
	-f batch-byte-compile ./src/elisp/*.el

test:
	cask exec ert-runner --verbose --reporter ert --win && \
	cask exec ert-runner --verbose --reporter ert --no-win

clean:
	rm -f ./src/elisp/*.elc

.PHONY: test
