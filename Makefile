MAKEFLAGS += k

CASK = cask
TEST_COMMAND = ert-runner --verbose --reporter ert

EMACS ?= emacs

NO_MISSING_COLOR_WARNING = "(defvar treemacs-no-load-time-warnings t)"
SRCDIR = src/elisp
EMACSFLAGS = -Q -batch -L $(SRCDIR) --eval $(NO_MISSING_COLOR_WARNING)
COMPILE_COMMAND = -f batch-byte-compile $(SRCDIR)/*.el

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
	@echo Running test suite in GUI mode
	@$(CASK) exec $(TEST_COMMAND) --win
	@echo Running test suite in TUI mode
	@$(CASK) exec $(TEST_COMMAND) --no-win

clean:
	@rm -f $(SRCDIR)/*.elc

lint: compile clean
