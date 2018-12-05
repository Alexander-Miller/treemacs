MAKEFLAGS += k

CASK = cask

EMACS ?= emacs

NO_COLOR_WARNING_FLAG = --eval "(defvar treemacs-no-load-time-warnings t)"
SRC_DIR = src/elisp
EXTRA_DIR = src/extra
EMACSFLAGS = -Q -batch -L $(SRC_DIR) -L $(EXTRA_DIR) $(NO_COLOR_WARNING_FLAG)
COMPILE_COMMAND = -f batch-byte-compile $(SRC_DIR)/*.el $(EXTRA_DIR)/*.el
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
	@rm -f $(SRC_DIR)/*.elc
	@rm -f $(EXTRA_DIR)/*.elc

lint: compile clean
