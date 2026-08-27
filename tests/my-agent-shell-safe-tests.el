;;; my-agent-shell-safe-tests.el --- Tests for my-agent-shell-safe -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test' from the repository root.

;;; Code:

(require 'ert)
(require 'my-agent-shell-safe)

(defconst my-agent-shell-safe-tests--safe
  '("git status"
    "`git status`"
    "`ls -la`"
    "`rg -n foo`"
    "`head -20 README.md`"
    "`git log | rg foo`"
    "`ps ax | rg foo || true`"
    "`git diff && git status`"
    "`pnpm --filter web test`"
    "`pnpm exec tsc`"
    "`gh pr view 1`"
    "`gh search prs --author me`"
    ;; Web tools — URLs/queries may contain & and ? which are not shell operators here.
    "Fetch Web Fetch"
    "Fetch Web Fetch https://example.com"
    "Fetch Web Fetch https://example.com?foo=bar&baz=qux"
    "Find Web Search"
    "Find Web Search some query terms"
    "Find Web Search query with & ampersand"
    ;; "Web search:" / "Web fetch:" prefix format.
    "Web search: Peter Naur Programming as Theory Building full text PDF HTML"
    "Web fetch: https://example.com"
    ;; Bare Fetch <URL> format (alternative WebFetch title style).
    "Fetch https://example.com"
    "Fetch http://example.com"
    "Fetch https://pages.cs.wisc.edu/~remzi/Naur.pdf"
    ;; Read tool — file reads are always safe.
    "Read /Users/jesse.claven/.claude/skills/pr-review/review-guide.md"
    "Read ~/some/file.el"
    ;; Linear read-only tools (get/list/search).
    "mcp__plugin_linear_linear__get_issue"
    "mcp__plugin_linear_linear__list_issues"
    "mcp__plugin_linear_linear__search_documentation"
    ;; Notion fetch — read-only page retrieval.
    "mcp__plugin_Notion_notion__notion-fetch"
    "mcp__plugin_Notion_notion__notion-fetch https://app.notion.com/p/SomePage"
    ;; make test is a read-only test runner.
    "make test"
    "`make test`"
    "`make test 2>&1`"
    ;; find with /dev/null redirect — 2>/dev/null is not a file write.
    "`find /tmp -name foo 2>/dev/null`"
    "`find /Users/jesse.claven/src -name reports_repo.py 2>/dev/null | head -5`")
  "Titles that must be auto-approved without prompting.")

(defconst my-agent-shell-safe-tests--unsafe
  '(;; Redirects must never be read-only, whatever the spacing.
    "`echo pwned >file`"
    "`echo pwned >> ~/.zshrc`"
    "`cat >file`"
    "`tail -f x >>y`"
    "`wc -l < /etc/passwd`"
    "`grep -r a>b .`"
    ;; Newline is a command separator too.
    "ls\nrm -rf /"
    ;; Chaining and substitution.
    "`ls; rm -rf /`"
    "`true && rm -rf /`"
    "`echo $(whoami)`"
    "`ls & rm -rf /`"
    ;; Read-only commands turned arbitrary by a flag.
    "`find . -exec rm {} \;`"
    "`find . -ok rm {} \;`"
    ;; Write subcommands of otherwise-allowed tools.
    "`gh pr merge 1`"
    "`gh pr close 1`"
    "`git push`"
    "`pnpm --filter web publish`"
    ;; Not a shell command at all -- an Edit tool call.
    "home/dot_config/emacs/init.el"
    "`rm -rf /`")
  "Titles that must fall through to the interactive dialog.")

(ert-deftest my-agent-shell-safe/approves-read-only-commands ()
  (dolist (title my-agent-shell-safe-tests--safe)
    (should (my/agent-shell-safe-command-p title))))

(ert-deftest my-agent-shell-safe/rejects-everything-else ()
  (dolist (title my-agent-shell-safe-tests--unsafe)
    (should-not (my/agent-shell-safe-command-p title))))

(ert-deftest my-agent-shell-safe/rejects-empty-and-non-strings ()
  (should-not (my/agent-shell-safe-command-p nil))
  (should-not (my/agent-shell-safe-command-p ""))
  (should-not (my/agent-shell-safe-command-p "   "))
  (should-not (my/agent-shell-safe-command-p 42)))

(defun my-agent-shell-safe-tests--permission (title &optional options)
  "Build a permission alist for TITLE, recording responses in a cons cell.
Returns (PERMISSION . CELL); CELL's car holds the option-id responded with.
OPTIONS defaults to a single `allow_always' entry."
  (let ((cell (list nil)))
    (cons (list (cons :tool-call (list (cons :title title)))
                (cons :options (or options
                                   '(((:kind . "allow_always")
                                      (:option-id . "opt-1")))))
                (cons :respond (lambda (id) (setcar cell id) t)))
          cell)))

(ert-deftest my-agent-shell-safe/responder-approves-safe-call ()
  (let* ((pair (my-agent-shell-safe-tests--permission "`git status`"))
         (handled (my/agent-shell-permission-responder (car pair))))
    (should handled)
    (should (equal (car (cdr pair)) "opt-1"))))

(ert-deftest my-agent-shell-safe/responder-defers-unsafe-call ()
  (let* ((pair (my-agent-shell-safe-tests--permission "`rm -rf /`"))
         (handled (my/agent-shell-permission-responder (car pair))))
    (should-not handled)
    (should-not (car (cdr pair)))))

(ert-deftest my-agent-shell-safe/responder-defers-edit-tool-call ()
  (let* ((pair (my-agent-shell-safe-tests--permission "init.el"))
         (handled (my/agent-shell-permission-responder (car pair))))
    (should-not handled)
    (should-not (car (cdr pair)))))

(ert-deftest my-agent-shell-safe/responder-defers-without-allow-always ()
  (let* ((pair (my-agent-shell-safe-tests--permission
                "`git status`" '(((:kind . "allow_once") (:option-id . "once")))))
         (handled (my/agent-shell-permission-responder (car pair))))
    (should-not handled)
    (should-not (car (cdr pair)))))

(ert-deftest my-agent-shell-safe/responder-defers-on-missing-title ()
  (let ((cell (list nil)))
    (should-not (my/agent-shell-permission-responder
                 (list (cons :tool-call nil)
                       (cons :options '(((:kind . "allow_always")
                                         (:option-id . "opt-1"))))
                       (cons :respond (lambda (id) (setcar cell id) t)))))
    (should-not (car cell))))

;; Regression: the responder used to be a lambda closing over a `let' inside
;; a `use-package' `:config' guarded by `:after'.  That block is evaluated
;; under dynamic binding, so the closure captured nothing and every call
;; signalled `void-variable' -- silently, hanging the tool call.  Depending
;; only on top-level defconsts keeps it callable.
(ert-deftest my-agent-shell-safe/responder-is-callable-without-captured-state ()
  (let ((fn (symbol-function 'my/agent-shell-permission-responder)))
    (should (functionp fn))
    (dolist (title (append my-agent-shell-safe-tests--safe
                           my-agent-shell-safe-tests--unsafe))
      (let ((pair (my-agent-shell-safe-tests--permission title)))
        ;; Must return a verdict rather than signalling.
        (should (memq (not (my/agent-shell-permission-responder (car pair)))
                      '(t nil)))))))

(provide 'my-agent-shell-safe-tests)
;;; my-agent-shell-safe-tests.el ends here
