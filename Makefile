EMACS ?= emacs
USER_DIR := home/dot_config/emacs/user
TEST_DIR := tests

.PHONY: test
test: ## Run the Emacs Lisp unit tests
	$(EMACS) -Q --batch \
	  --eval '(setq load-prefer-newer t native-comp-enable-subr-trampolines nil)' \
	  -L $(USER_DIR) -L $(TEST_DIR) \
	  -l my-agent-shell-safe-tests \
	  -l my-agent-shell-patches-tests \
	  -l my-tab-groups-tests \
	  -l my-eglot-startup-tests \
	  -l my-org-startup-tests \
	  -l my-easysession-agent-shell-tests \
	  -f ert-run-tests-batch-and-exit
