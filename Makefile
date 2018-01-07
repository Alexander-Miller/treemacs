# ADD_ELPA="(mapc (lambda (d) (when (file-directory-p d) (add-to-list 'load-path d))) (nthcdr 2 (directory-files \"~/.emacs.d/elpa\" t)))"
# LOAD_TREEMACS="(progn (require 'treemacs) (require 'treemacs-evil))"

# .PHONY: compile clean test

# clean:
# 	rm -f ./src/elisp/*.elc

# compile:
# 	emacs -Q --batch -L ./src/elisp/. --eval ${ADD_ELPA} --eval ${LOAD_TREEMACS} -f batch-byte-compile ./src/elisp/*.el

# test: clean compile
# 	emacs -Q --batch -L ./src/elisp --eval ${ADD_ELPA} --eval ${LOAD_TREEMACS} -l ./src/elisp/treemacs-tests.el -f ert-run-tests-batch-and-exit


EMACS ?= emacs

compile:
	cask exec $(EMACS) -Q -batch \
	-L ./src/elisp \
	--eval '(setq byte-compile-error-on-warn t)' \
	-f batch-byte-compile ./src/elisp/*.el

test:
	cask exec ert-runner --win

.PHONY: test
