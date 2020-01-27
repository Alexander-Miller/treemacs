.POSIX:

MAKEFLAGS += k
CASK = cask
EMACS ?= emacs

NO_LOAD_WARNINGS = --eval "(defvar treemacs-no-load-time-warnings t)"
SRC_DIR          = src/elisp
EXTRA_DIR        = src/extra
EMACSFLAGS       = -Q -batch -L $(SRC_DIR) -L $(EXTRA_DIR) $(NO_LOAD_WARNINGS)
COMPILE_COMMAND  = -f batch-byte-compile $(SRC_DIR)/*.el $(EXTRA_DIR)/*.el
LINT_DIR         = /tmp/treemacs
LINT_FLAG        = --eval "(setq byte-compile-dest-file-function (lambda (f) (concat \"$(LINT_DIR)\" (file-name-nondirectory f) \"c\")))"
TEST_COMMAND     = buttercup -L . $(NO_LOAD_WARNINGS)

.PHONY: test compile clean lint prepare clean-start .prepare-lint

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

lint: EMACSFLAGS += $(LINT_FLAG)
lint: .prepare-lint compile
	@rm -rf $(LINT_DIR)

clean-start: prepare
	@$(CASK) exec $(EMACS) -Q -L $(SRC_DIR) --eval "(require 'treemacs)" &

.prepare-lint:
	@rm -rf $(LINT_DIR)
	@mkdir -p $(LINT_DIR)
