EMACS ?= emacs
USER_DIR := home/dot_config/emacs/user
TEST_DIR := tests

.PHONY: test
test: ## Run the Emacs Lisp unit tests
	$(EMACS) -Q --batch \
	  -L $(USER_DIR) -L $(TEST_DIR) \
	  -l my-agent-shell-safe-tests \
	  -f ert-run-tests-batch-and-exit
