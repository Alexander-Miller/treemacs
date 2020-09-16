.POSIX:

MAKEFLAGS += k
CASK = cask
EMACS ?= emacs

NO_LOAD_WARNINGS = --eval "(defvar treemacs-no-load-time-warnings t)"
SRC_DIR          = src/elisp
EXTRA_DIR        = src/extra
EMACSFLAGS       = -Q -batch -L $(SRC_DIR) $(NO_LOAD_WARNINGS)
COMPILE_COMMAND  = -f batch-byte-compile
CHECKDOC_COMMAND = -l "test/checkdock.el"
LINT_DIR         = /tmp/treemacs
LINT_FLAG        = --eval "(setq byte-compile-dest-file-function (lambda (f) (concat \"$(LINT_DIR)\" (file-name-nondirectory f) \"c\")))"
TEST_COMMAND     = buttercup -L $(SRC_DIR) -L $(EXTRA_DIR) -L . $(NO_LOAD_WARNINGS)

ELS  = $(SRC_DIR)/treemacs.el
ELS += $(SRC_DIR)/treemacs-async.el
ELS += $(SRC_DIR)/treemacs-bookmarks.el
ELS += $(SRC_DIR)/treemacs-compatibility.el
ELS += $(SRC_DIR)/treemacs-core-utils.el
ELS += $(SRC_DIR)/treemacs-customization.el
ELS += $(SRC_DIR)/treemacs-diagnostics.el
ELS += $(SRC_DIR)/treemacs-dom.el
ELS += $(SRC_DIR)/treemacs-extensions.el
ELS += $(SRC_DIR)/treemacs-faces.el
ELS += $(SRC_DIR)/treemacs-icons.el
ELS += $(SRC_DIR)/treemacs-interface.el
ELS += $(SRC_DIR)/treemacs-filewatch-mode.el
ELS += $(SRC_DIR)/treemacs-follow-mode.el
ELS += $(SRC_DIR)/treemacs-fringe-indicator.el
ELS += $(SRC_DIR)/treemacs-icons.el
ELS += $(SRC_DIR)/treemacs-logging.el
ELS += $(SRC_DIR)/treemacs-macros.el
ELS += $(SRC_DIR)/treemacs-mode.el
ELS += $(SRC_DIR)/treemacs-mouse-interface.el
ELS += $(SRC_DIR)/treemacs-persistence.el
ELS += $(SRC_DIR)/treemacs-rendering.el
ELS += $(SRC_DIR)/treemacs-scope.el
ELS += $(SRC_DIR)/treemacs-tag-follow-mode.el
ELS += $(SRC_DIR)/treemacs-tags.el
ELS += $(SRC_DIR)/treemacs-tags.el
ELS += $(SRC_DIR)/treemacs-themes.el
ELS += $(SRC_DIR)/treemacs-visuals.el
ELS += $(SRC_DIR)/treemacs-workspaces.el
ELS += $(EXTRA_DIR)/treemacs-all-the-icons.el
ELS += $(EXTRA_DIR)/treemacs-evil.el
ELS += $(EXTRA_DIR)/treemacs-icons-dired.el
ELS += $(EXTRA_DIR)/treemacs-persp.el
ELS += $(EXTRA_DIR)/treemacs-perspective.el
ELS += $(EXTRA_DIR)/treemacs-projectile.el
ELCS = $(ELS:.el=.elc)

.PHONY: test compile checkdoc clean lint prepare clean-start .prepare-lint

.ONESHELL:

%.elc: %.el
	@printf "Compiling $<\n"
	$(CASK) exec $(EMACS) $(EMACSFLAGS) $(COMPILE_COMMAND) $<

compile: prepare $(ELCS)

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
lint: .prepare-lint compile checkdoc
	@rm -rf $(LINT_DIR)

checkdoc:
	@$(CASK) exec $(EMACS) $(EMACSFLAGS) $(CHECKDOC_COMMAND)

clean-start: prepare
	@$(CASK) exec $(EMACS) -Q -L $(SRC_DIR) --eval "(require 'treemacs)" &

.prepare-lint:
	@rm -rf $(LINT_DIR)
	@mkdir -p $(LINT_DIR)
