MAKEFLAGS += k

CASK = cask

EMACS ?= emacs

NO_COLOR_WARNING_FLAG = --eval "(defvar treemacs-no-load-time-warnings t)"
SRCDIR = src/elisp
EMACSFLAGS = -Q -batch -L $(SRCDIR) $(NO_COLOR_WARNING_FLAG)
COMPILE_COMMAND = -f batch-byte-compile $(SRCDIR)/*.el
TEST_COMMAND = buttercup -L . $(NO_COLOR_WARNING_FLAG)

.PHONY: test compile clean lint prepare

.ONESHELL:

compile: prepare
	@$(CASK) exec $(EMACS) $(EMACSFLAGS) $(COMPILE_COMMAND)

.cask: Cask
	@echo Updating external dependencies...
	@$(CASK) install
	@$(CASK) update
	@touch .cask

prepare: .cask

test: prepare
	@$(CASK) exec $(TEST_COMMAND)

clean:
	@rm -f $(SRCDIR)/*.elc

lint: compile clean
