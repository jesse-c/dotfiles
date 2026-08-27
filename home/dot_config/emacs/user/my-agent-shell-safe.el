;;; my-agent-shell-safe.el --- Auto-approve read-only agent-shell commands -*- lexical-binding: t; -*-

;;; Commentary:

;; Decides whether an agent-shell tool-call title names a read-only shell
;; command that can be auto-approved without prompting.
;;
;; The regexps are top-level `defconst's rather than a closure over a
;; `let'.  This matters: a `use-package' `:config' block guarded by
;; `:after' is evaluated under *dynamic* binding, because `:after'
;; expands to nested `eval-after-load' and the inner registration runs
;; when `lexical-binding' has its global nil value.  A lambda written
;; there captures nothing, so closing over a `let' gives `void-variable'
;; at call time -- and agent-shell swallows the error, leaving the tool
;; call hung forever.  Top-level constants sidestep that entirely.
;;
;; `my/agent-shell-safe-command-p' is pure.  The effect of answering the
;; permission request lives in `my/agent-shell-permission-responder'.
;;
;; Tests: tests/my-agent-shell-safe-tests.el (make test).

;;; Code:

(require 'map)
(require 'rx)
(require 'seq)
(require 'subr-x)

(defconst my/agent-shell-safe-command-re
  (rx bos (or "ls" "grep" "rg" "cat" "bat" "find" "Find" "fd"
              "head" "tail" "wc" "echo" "which" "type"
              "less" "file" "stat" "logfire-trace"
              "ps" "true"
              "git log" "git diff" "git show" "git status"
              "git branch" "git tag" "git rev-parse"
              "git remote" "git stash list"
              "pnpm exec eslint" "pnpm exec jest"
              "pnpm exec tsc"
              "make test"
              ;; gh read-only: view/list/status/checks/diff/search only
              "gh pr view" "gh pr list" "gh pr status"
              "gh pr checks" "gh pr diff"
              "gh issue view" "gh issue list" "gh issue status"
              "gh repo view" "gh repo list"
              "gh run view" "gh run list"
              "gh release view" "gh release list"
              "gh search")
      (or eos " " "\n"))
  "Commands that only read, so need no confirmation.")

(defconst my/agent-shell-safe-web-re
  (rx bos
      (or (seq (or "Fetch" "Find") " Web " (or "Fetch" "Search"))
          (seq "Fetch " (or "https://" "http://"))
          (seq "Web " (or "fetch" "search") ":")))
  "Web tool calls (WebFetch, WebSearch) that are always safe to auto-approve.")

(defconst my/agent-shell-safe-notion-re
  (rx bos "mcp__plugin_Notion_notion__notion-fetch")
  "Notion fetch tool calls that are always safe to auto-approve.")

(defconst my/agent-shell-safe-read-tool-re
  (rx bos "Read " (or "/" "~"))
  "Read tool calls (file reads) that are always safe to auto-approve.")

(defconst my/agent-shell-safe-linear-re
  (rx bos "mcp__plugin_linear_linear__" (or "get_" "list_" "search_"))
  "Linear MCP read-only tool calls (get/list/search) that are safe to auto-approve.")

(defconst my/agent-shell-safe-pnpm-filter-re
  (rx bos "pnpm --filter " (one-or-more (not space)) " "
      (or "test" "lint" "typecheck" "tsc" "build" "check")
      (or eos " " "\n"))
  "Workspace-scoped pnpm scripts that are safe to run unattended.")

(defconst my/agent-shell-safe-exec-re
  (rx (or "-exec" "--exec" "-ok" (seq "-x" (or eos " "))))
  "Flags that let an otherwise read-only command run arbitrary programs.")

(defconst my/agent-shell-safe-dangerous-re
  (rx (or ">" "<" ";" "`" "$(" "&" "\n"))
  "Shell metacharacters that can write files or chain further commands.
Matched anywhere and regardless of spacing, so `echo x >>~/.zshrc' is
not mistaken for a read-only command.")

(defconst my/agent-shell-safe-harmless-redirect-re
  (rx (one-or-more space)
      (optional (one-or-more digit))
      ">"
      (or "/dev/null"
          (seq "&" (one-or-more digit))))
  "Output redirections that suppress output but don't write files.
Covers 2>/dev/null, >/dev/null, and 2>&1 style redirects.")

(defun my/agent-shell-safe--strip-harmless-redirects (s)
  "Return S with harmless output-suppression redirects removed."
  (replace-regexp-in-string my/agent-shell-safe-harmless-redirect-re "" s))

(defconst my/agent-shell-safe-chain-re
  (rx " " (or "||" "&&" "|") " ")
  "Separator for chained commands.  Every segment must be safe on its own.")

(defun my/agent-shell-safe--unquote (title)
  "Return TITLE stripped of wrapping backticks and whitespace."
  (string-trim title "[ \t\n`]+" "[ \t\n`]+"))

(defun my/agent-shell-safe--segment-p (segment)
  "Return non-nil when SEGMENT is a read-only command."
  (let* ((s (string-trim segment))
         ;; Strip harmless output-suppression redirects before checking for
         ;; dangerous metacharacters, so 2>/dev/null and 2>&1 don't block
         ;; otherwise-safe commands like find or make test.
         (s-clean (my/agent-shell-safe--strip-harmless-redirects s)))
    ;; Read-only MCP/tool calls safe regardless of path or URL characters.
    (or (string-match-p my/agent-shell-safe-web-re s)
        (string-match-p my/agent-shell-safe-notion-re s)
        (string-match-p my/agent-shell-safe-read-tool-re s)
        (string-match-p my/agent-shell-safe-linear-re s)
        (and (or (string-match-p my/agent-shell-safe-command-re s-clean)
                 (string-match-p my/agent-shell-safe-pnpm-filter-re s-clean))
             (not (string-match-p my/agent-shell-safe-exec-re s-clean))
             (not (string-match-p my/agent-shell-safe-dangerous-re s-clean))))))

(defun my/agent-shell-safe-command-p (title)
  "Return non-nil when TITLE is a read-only command safe to auto-approve.
TITLE is an agent-shell tool-call title, optionally wrapped in backticks.
A chained command is safe only when every one of its segments is."
  (when (and (stringp title)
             (not (string-empty-p (string-trim title))))
    (let ((segments (split-string (my/agent-shell-safe--unquote title)
                                  my/agent-shell-safe-chain-re)))
      (and segments
           (seq-every-p #'my/agent-shell-safe--segment-p segments)))))

(defun my/agent-shell-permission-responder (permission)
  "Auto-approve PERMISSION when its tool call is a read-only command.
Return non-nil when handled, or nil to fall back to the interactive
dialog, as `agent-shell-permission-responder-function' expects."
  (when-let* ((title (map-elt (map-elt permission :tool-call) :title))
              ((my/agent-shell-safe-command-p title))
              (choice (seq-find (lambda (option)
                                  (equal (map-elt option :kind) "allow_always"))
                                (map-elt permission :options))))
    (funcall (map-elt permission :respond) (map-elt choice :option-id))))

(provide 'my-agent-shell-safe)
;;; my-agent-shell-safe.el ends here
