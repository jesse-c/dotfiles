;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;; -----------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq use-package-enable-imenu-support t
      use-package-always-ensure t)

(require 'use-package)

(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  (major-mode-remap-alist '((git-commit-mode . git-commit-ts-mode)))
  (package-install-upgrade-built-in t)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(use-package helpful
  :bind
  (("C-S-h f" . helpful-callable)
   ("C-S-h F" . helpful-function)
   ("C-S-h v" . helpful-variable)
   ("C-S-h k" . helpful-key)
   ("C-S-h x" . helpful-command)
   ("C-S-h d" . helpful-at-point)))

;; https://emacs.stackexchange.com/a/64551
(defun my/ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.
        Activate this advice with:
          (advice-add 'message :before 'my/ad-timestamp-message)
        Deactivate this advice with:
          (advice-remove 'message 'my/ad-timestamp-message)"
  (if message-log-max
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (format-time-string "[%F %T.%3N] "))))))

(advice-add 'message :before 'my/ad-timestamp-message)

;; Common -----------------------------------------------------------------------

(require 'transient)
(use-package ht)
(use-package dash)
(use-package loop)
(use-package s)
(use-package crux)
(use-package async
  :commands (async-start))
(use-package load-relative)
(use-package f)

;; Server -----------------------------------------------------------------------

;; Hint: emacsclient -n file1 file2 (Use -c to open in a new frame)
;; Example: emacsclient --socket-name ~/.config/emacs/server/server FILE

(setq server-socket-path "~/.config/emacs/server") ; Make the directory if it doesn't exist
(make-directory server-socket-path t)
(setq server-socket-dir server-socket-path)

(load "server")
(unless (< emacs-major-version 23)
  (defun server-ensure-safe-dir (_dir) "Noop" t))

(unless (server-running-p) (server-start))

;; OS ---------------------------------------------------------------------------

;; All

(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)

(setq auth-sources '("~/.authinfo"))

;; Tidy up .emacs.d mess
(use-package no-littering)

(savehist-mode)

;; Disabling suspend-frame binding
;; Very annoying binding, lets get rid of it.
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#disabling-suspend-frame-binding
(global-unset-key (kbd "C-z"))

;; macOS

(when (memq window-system '(mac ns))
  (setq dired-use-ls-dired nil))

(setq delete-by-moving-to-trash t)

;; https://emacs.stackexchange.com/a/41767
;; Alternative to try: https://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
(setq use-dialog-box nil)

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-debug t)
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(when (memq window-system '(mac ns))
  (setq dired-use-ls-dired nil))

;; https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
(defun my/file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; https://mise.jdx.dev/ide-integration.html
;; CLI tools installed by Mise
;; See: https://www.emacswiki.org/emacs/ExecPath
(setenv "PATH" (concat (getenv "PATH") ":/home/user/.local/share/mise/shims"))
(setq exec-path (append exec-path '("/home/user/.local/share/mise/shims")))

;; Project ----------------------------------------------------------------------

(use-package project
  :ensure nil
  :init
  (setq project-vc-extra-root-markers '(;; Org
                                        "TODO.org"
                                        ;; Clojure(Script)
                                        "project.clj" "deps.edn"))
  :config
  (transient-define-prefix project-transient-menu ()
    "Project command menu."
    ["Navigation"
     [("s" "Search" consult-ripgrep)
      ("b" "Buffers" consult-project-buffer)
      ("f" "Files" project-find-file)]
     [("l" "Line" consult-line)
      ("d" "Layout" project-dired)]]
    ["Management"
     [("t" "New tab" tab-new)
      ("c" "Close tab" tab-close)
      ("n" "Rename tab" my/rename-tab-to-project-name)]
     [("p" "Switch (Known)" project-switch-project)
      ("P" "Switch (All)" consult-ghq-switch-project)]])
  :bind
  ("s-p" . project-transient-menu))

(defun my/project-root ()
  (interactive)
  (project-root (project-current)))

(defun my/rename-tab-to-project-name ()
  "Rename the current tab to the project name."
  (interactive)
  (when-let ((project-name (file-name-nondirectory (directory-file-name (my/project-root)))))
    (tab-rename project-name)))

;; VCS -------------------------------------------------------------------------

(use-package magit
  :after transient
  :commands
  (magit-status magit-blame magit-blame-quit)
  :custom
  ;; Setting ‘forge-add-default-bindings’ to nil in ‘evil-collection-forge-setup’.
  ;; To suppress this message you can set this variable to nil in your init.el file.
  (forge-add-default-bindings nil)
  :init
  (setopt magit-format-file-function #'magit-format-file-all-the-icons)
  :bind
  (("s-g" . magit-status)
   :map magit-status-mode-map
   ("*" . th/magit-aux-commands))
  :config
  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)])
  ;; I want my personal commands transient to be bound to * and be shown
  ;; in the Magit dispatch transient (which is bound to ? in Magit status
  ;; buffers and C-x M-g in any Magit enabled buffer)
  (transient-append-suffix 'magit-dispatch "!"
    '("*" "My Magit Cmds" th/magit-aux-commands)))

(use-package magit-popup
  :after magit)

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package forge
  :after (magit transient)
  :defer 1)

(use-package pr-review)

(use-package git-modes
  :after magit)

;; This package is easiest way to open particular link on
;; github/gitlab/bitbucket/stash/git.savannah.gnu.org/sourcehut from
;; Emacs. It supports various kind of emacs buffer.
(use-package browse-at-remote
  :after magit)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02)
  :bind
  (("M-p" . git-gutter:previous-hunk)
   ("M-n" . git-gutter:next-hunk)))

(use-package git-gutter-fringe
  :config
  ;; https://ianyepan.github.io/posts/emacs-git-gutter/
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; Create URLs to files and commits in repository hosting services
(use-package git-link
  :after magit
  :defer 1
  :custom
  {{- if eq .chezmoi.os "linux"}}
  (git-link-default-remote "upstream")
  {{- end}}
  (git-link-use-commit t))

;; Why was this line changed
(use-package git-messenger
  :after magit)

;; View versions of a file
(use-package git-timemachine
  :after transient
  :commands git-timemachine)

(use-package git-commit-ts-mode
  :vc
  (:url "https://github.com/danilshvalov/git-commit-ts-mode" :branch "main")
  :mode ("\\COMMIT_EDITMSG\\'" . git-commit-ts-mode))

(use-package difftastic
  :after (magit transient)
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

;; https://tsdh.org/posts/2022-08-01-difftastic-diffing-with-magit.html

(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))

(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))

(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

;; Tree-sitter ------------------------------------------------------------------

(use-package treesit
  :ensure nil)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; LSP --------------------------------------------------------------------------

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil) ;; The value of nil or 0 means don’t block at all during the waiting period
  :hook
  (clojure-mode . eglot-ensure)
  (clojure-ts-mode . eglot-ensure)
  (elixir-mode . eglot-ensure)
  (elixir-ts-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (swift-mode . eglot-ensure)
  (swift-ts-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (vespa-schema-mode . eglot-ensure)
  (yaml-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure)
  :bind
  ("s-l" . eglot-transient-menu)
  :config
  (add-to-list 'eglot-server-programs
               `(clojure-mode . ,(eglot-alternatives
                                  '(("clojure-lsp")))))
  (add-to-list 'eglot-server-programs
               `(clojure-ts-mode . ,(eglot-alternatives
                                     '(("clojure-lsp")))))
  (add-to-list 'eglot-server-programs
               `(elixir-mode . ,(eglot-alternatives
                                 '(("elixir-ls")))))
  (add-to-list 'eglot-server-programs
               `(elixir-ts-mode . ,(eglot-alternatives
                                    '(("elixir-ls")))))
  (add-to-list 'eglot-server-programs
               `(go-mode . ,(eglot-alternatives
                             '(("gopls")))))
  (add-to-list 'eglot-server-programs
               `(go-ts-mode . ,(eglot-alternatives
                                '(("gopls")))))
  (add-to-list 'eglot-server-programs
               `(typescript-mode . ,(eglot-alternatives
                                     '(("deno" "lsp")))))
  (add-to-list 'eglot-server-programs
               `(typescript-ts-mode . ,(eglot-alternatives
                                        '(("deno" "lsp")))))
  (add-to-list 'eglot-server-programs
               `(yaml-mode . ,(eglot-alternatives
                               '(("yaml-language-server" "--stdio")))))
  (add-to-list 'eglot-server-programs
               `(yaml-ts-mode . ,(eglot-alternatives
                                  '(("yaml-language-server" "--stdio")))))
  (add-to-list 'eglot-server-programs
               `(swift-mode . ,(eglot-alternatives
                                '(("sourcekit-lsp")))))
  (add-to-list 'eglot-server-programs
               `(swift-ts-mode . ,(eglot-alternatives
                                   '(("sourcekit-lsp")))))
  (add-to-list 'eglot-server-programs
               `(rust-mode . ,(eglot-alternatives
                               '(("rust-analyzer")))))
  (add-to-list 'eglot-server-programs
               `(rust-ts-mode . ,(eglot-alternatives
                                  '(("rust-analyzer")))))
  (add-to-list 'eglot-server-programs
               `(python-mode . ,(eglot-alternatives
                                 '(("basedpyright-langserver" "--stdio")))))
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ,(eglot-alternatives
                                    '(("basedpyright-langserver" "--stdio")))))
  (add-to-list 'eglot-server-programs
               `(vespa-schema-mode . ,(eglot-alternatives
                                       (list (list "java" "-jar" (expand-file-name "~/.local/bin/vespa-language-server.jar"))))))

  (transient-define-prefix eglot-server-menu ()
    "Eglot server commands."
    ["Server Commands"
     [("c" "Connect" eglot)
      ("C" "Reconnect" eglot-reconnect)
      ("q" "Shutdown" eglot-shutdown)
      ("Q" "Shutdown all" eglot-shutdown-all)]
     [("e" "Events" eglot-events-buffer)]])

  (transient-define-prefix eglot-transient-menu ()
    "Eglot commands menu."
    ["Navigation"
     [("." "Find definitions" xref-find-definitions
       :description "Find definitions of identifier at point")
      ("," "Pop back" xref-pop-marker-stack
       :description "Return to position before last jump")
      ("/" "Find references" xref-find-references
       :description "Find references to identifier at point")]
     [("i" "Find implementations" eglot-find-implementation
       :description "Find implementations of identifier at point")
      ("t" "Find type definition" eglot-find-typeDefinition
       :description "Find type definition of identifier at point")
      ("D" "Find declaration" eglot-find-declaration
       :description "Find declaration of identifier at point")]]
    ["Code actions"
     [("a" "Code actions" eglot-code-actions)
      ("f" "Format buffer" eglot-format-buffer)
      ("F" "Format region" eglot-format)]
     [("R" "Rename symbol" eglot-rename)]]
    ["Diagnostics"
     [("d" "Show all errors" flycheck-list-errors)
      ("n" "Next error" flycheck-next-error)
      ("p" "Previous error" flycheck-previous-error)
      ("v" "Verify setup" flycheck-verify-setup)]]
    ["Server"
     ("s" "Commands" eglot-server-menu)]))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;; AI --------------------------------------------------------------------------

(require 'auth-source)

(defun my/get-password (hostname username)
  "Return the password for the given HOSTNAME and USERNAME from .authinfo file."
  (interactive "sHostname: \nsUsername: ")
  (require 'auth-source)
  (let ((match (auth-source-search :host hostname
                                   :user username
                                   :require '(:secret)
                                   :max 1)))
    (if match
        (funcall (plist-get (car match) :secret))
      (error "No password found for %s@%s in .authinfo" username hostname))))

(use-package gptel
  :after transient
  :commands (gptel
             gptel-menu
             gptel-mode
             gptel-send
             gptel-backend
             gptel-make-anthropic)
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model 'claude-3-7-sonnet-20250219)
  (gptel-display-buffer-action
   '((display-buffer-reuse-window display-buffer-in-side-window)
     (side . right)
     (window-width . 80)
     (slot . 0)))
  :bind
  ("s-a" . gptel-menu)
  ("<f5>" . gptel-toggle-sidebar)
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude" :stream t :key (my/get-password "anthropic.com" "me")))
  (defun gptel-toggle-sidebar ()
    "Toggle a custom sidebar for a persistent buffer."
    ;; https://github.com/nehrbash/dotfiles/blob/main/Emacs.org#gpt
    (interactive)
    (let ((buffer-name "AI Chat"))
      (if-let* ((window (get-buffer-window buffer-name)))
          ;; If the sidebar is already open, close it.
          (delete-window window)
        ;; Else, create the sidebar
        (let* ((chat-buffer (gptel buffer-name))
               (window (get-buffer-window chat-buffer)))
          (display-buffer-in-side-window
           chat-buffer gptel-display-buffer-action)
          (when window
            (set-window-dedicated-p window t)
            (set-window-parameter window 'no-other-window t)
            (select-window window))
          (setq mode-line-format nil))))))

(use-package org-ai
  :commands (org-ai-mode
             org-ai-global-mode)
  :hook
  (org-mode . org-ai-mode)
  ;; :init
  ;; (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :custom
  (org-ai-openai-api-token (my/get-password "api.openai.com" "me"))
  (org-ai-default-chat-model "gpt-4-turbo"))

(use-package aidermacs
  :after (transient magit)
  :vc
  (:url "https://github.com/MatthewZMD/aidermacs" :branch "main")
  :config
  (setq aider-args '("--model" "anthropic/claude-3-7-sonnet-20250219"))
  (setq aidermacs-backend 'vterm)
  (setenv "ANTHROPIC_API_KEY" (my/get-password "anthropic.com" "me"))
  :bind
  ("C-c a" . aider-transient-menu))

(use-package codeium
  :vc
  (:url "https://github.com/Exafunction/codeium.el" :branch "main")

  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point t)
  ;; codeium-completion-at-point is autoloaded, but you can
  ;; optionally set a timer, which might speed up things as the
  ;; codeium local language server takes ~0.2s to start up
  (add-hook 'emacs-startup-hook
            (lambda () (run-with-timer 0.1 nil #'codeium-init)))

  :defer t
  :config
  (setq use-dialog-box nil) ;; do not use popup boxes

  ;; if you don't want to use customize to save the api-key
  ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

  ;; Limit the string sent to codeium for better performance
  (defun my/codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my/codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point)
                                     (setq codeium/document/text 'my/codeium/document/text)
                                     (setq codeium/document/cursor_offset 'my/codeium/document/cursor_offset))))
  ;; https://github.com/yuzhou721/dotemacs/blob/a1c88c0ee489110f05e7d38ac0006e8f2064a7ab/lisp/config/init-ai.el#L37C1-L40C52
  (defun my/codeium ()
    "Decouple codeium from other completions"
    (interactive)
    (cape-interactive #'codeium-completion-at-point)))

;; Snippets ---------------------------------------------------------------------

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :diminish
  :config
  (yas-reload-all))

;; Terminal ---------------------------------------------------------------------

(use-package eat
  :vc
  (:url "https://codeberg.org/akib/emacs-eat.git")
  :defer 1
  :hook
  (eshell-load-hook . eat-eshell-mode))

;; Notes ------------------------------------------------------------------------

(defvar org-dir "~/Documents/org/")

(defvar org-roam-dir (file-name-concat org-dir "roam/"))

(setq org-log-done t)

(use-package org
  :ensure nil
  :custom
  (org-confirm-babel-evaluate nil)
  (org-startup-with-inline-images t)
  (org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "REVIEW(r)" "|" "DONE(d)" "CANCELLED(c)")))
  (calendar-week-start-day 1)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((org . t)
     (python . t))))

(use-package org-roam
  :defer t
  :after org
  :custom
  (org-roam-directory (file-truename org-roam-dir))
  (org-roam-completion-everywhere t)
  :bind
  (("s-o" . org-transient-menu)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;; RETURN will follow links in org-mode files
  (setq org-return-follows-link  t)
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (transient-define-prefix org-transient-menu ()
    "Org command menu."
    ["Capture"
     [("c" "Capture" org-roam-capture)]]
    ["Navigation"
     [("s" "Search" consult-org-roam-search)
      ("f" "Find" org-roam-node-find)
      ("i" "Insert" org-roam-node-insert)
      ("g" "Graph" org-roam-graph)]]
    ["Dailies"
     [("g" "Goto today" org-roam-dailies-goto-today)
      ("t" "Capture today" org-roam-dailies-capture-today)
      ("y" "Capture yesterday" org-roam-dailies-capture-yesterday)]]))

(use-package org-ql
  :after org
  :defer t)

(use-package org-roam-ql
  :after (org org-roam org-ql)
  :defer t)

(use-package org-roam-ql-ql
  :after (org org-roam org-ql org-roam-ql)
  :defer t
  :config
  (org-roam-ql-ql-init))

(use-package org-modern
  :config
  (global-org-modern-mode))

(use-package ox-pandoc
  :after org)

;; Modal ------------------------------------------------------------------------

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-tree)
  (global-unset-key (kbd "C-u"))
  (global-set-key (kbd "C-u") 'evil-scroll-up)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :defer 1
  :commands evil-collection-init
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-textobj-tree-sitter
  :after (treesit evil evil-collection)
  :config
  ;; Map textobjects
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

  ;; Map gotos
  ;; Goto start of next function
  (define-key evil-normal-state-map
              (kbd "]f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer")))

  ;; Goto start of previous function
  (define-key evil-normal-state-map
              (kbd "[f")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))

  ;; Goto end of next function
  (define-key evil-normal-state-map
              (kbd "]F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))

  ;; Goto end of previous function
  (define-key evil-normal-state-map
              (kbd "[F")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

;; UI ---------------------------------------------------------------------------

(which-key-mode)

;; Indentation
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package aggressive-indent
  :hook
  (prog-mode . aggressive-indent-mode))

;; Windows
(global-set-key (kbd "C-h") 'windmove-left) ; Use F1 instead of C-h for help-command
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

(global-set-key (kbd "C-q") 'delete-window)

(defun my/split-window-below-and-move ()
  "Make the new split focused."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun my/split-window-right-and-move ()
  "Make the new split focused."
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-S-x") 'my/split-window-below-and-move)
(global-set-key (kbd "C-S-v") 'my/split-window-right-and-move)

(use-package zoom-window)

;; This simple library shows the current recursion level in the
;; mode-line. For example, if you enable-recursive-minibuffers.
(use-package recursion-indicator
  :config
  (recursion-indicator-mode))

;; Mode line
(use-package diminish)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding t))

;; Line numbers
(setq column-number-mode t) ;; Show the current line number
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Buffers
(defun my/clean-up-buffers ()
  "Clean-up buffers that have built up."
  (interactive)
  (kill-matching-buffers ".*\.ex" nil t)
  (kill-matching-buffers ".*\.exs" nil t)
  (kill-matching-buffers ".*\.yaml" nil t))

;; Pretty parens
(use-package rainbow-delimiters
  :diminish
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

;; Highlight current line
(global-hl-line-mode t)

;; Provides the frontend UI to display information either on the
;; left/right side of the buffer window.
(use-package sideline
  :hook
  (prog-mode . sideline-mode)
  (eglot-mode . sideline-mode)
  (flycheck-mode . sideline-mode)
  :init
  (setq sideline-backends-left '()
        sideline-backends-right '(sideline-flycheck sideline-eglot)
        sideline-display-backend-name t
        sideline-delay 0.1 ;; Seconds
        sideline-display-backend-name t
        sideline-display-backend-type 'right
        sideline-truncate t))

(use-package sideline-eglot
  :vc
  (:url "https://github.com/emacs-sideline/sideline-eglot" :branch "master")
  :config
  ;; TODO: Remove this once fixed upstream
  (defun my/fix-sideline-eglot-getf (orig-fun &rest args)
    "Use cl-getf instead of getf in sideline-eglot."
    (cl-letf (((symbol-function 'getf) #'cl-getf))
      (apply orig-fun args)))
  (advice-add 'sideline-eglot--async-candidates :around #'my/fix-sideline-eglot-getf))

(use-package sideline-flycheck
  :after (sideline flycheck)
  :hook
  (flycheck-mode . sideline-flycheck-setup))

(use-package breadcrumb
  :hook (after-init . breadcrumb-mode))

;; Themes
(defun my/theme-by-current-time ()
  "Get the light or dark THEME based on the current time."
  (let ((hour (->> (current-time)
                   (decode-time)
                   (nth 2))))
    (if (or (> hour 18) (< hour 5))
        'mocha
      'latte)))

(defun my/load-theme-by-current-time ()
  "Load the right theme based on the current time."
  (progn
    (setq catppuccin-flavor (my/theme-by-current-time))
    (load-theme 'catppuccin t)))

(defun my/disable-all-themes ()
  "Disable all enabled themes."
  (interactive)
  ;; List of enabled Custom Themes, highest precedence
  ;; first.
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my/disable-and-load-theme (theme)
  "Disable all themes and then load theme THEME."
  (interactive)
  (my/disable-all-themes)
  (load-theme theme))

(defun my/catppuccin-set-and-reload (flavour)
  "Sets the Catppuccin FLAVOUR and then reloads the theme."
  (interactive)
  (progn
    (setq catppuccin-flavor flavour)
    (catppuccin-reload)))

(defun my/catppuccin-latte ()
  "Set the Catppuccin latte flavour and then reloads the theme."
  (interactive)
  (my/catppuccin-set-and-reload 'latte))

(defun my/catppuccin-mocha ()
  "Set the Catppuccin mocha flavour and then reloads the theme."
  (interactive)
  (my/catppuccin-set-and-reload 'mocha))

(use-package catppuccin-theme
  :config
  (my/load-theme-by-current-time))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package affe)

(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; Icons
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :after (all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode))
;; This is being setup elsewhere
;; :hook
;; (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package transient
  :after seq)

(use-package hydra)

(use-package casual
  :after (dired re-builder)
  :bind
  ("s-c" . casual-editkit-main-tmenu)
  (:map dired-mode-map
        ("s-c" . casual-dired-tmenu))
  (:map reb-mode-map
        ("s-c" . casual-re-builder-tmenu))
  (:map isearch-mode-map
        ("s-c" . casual-isearch-tmenu))
  (:map Info-mode-map
        ("s-c" . casual-info-tmenu))
  (:map ibuffer-mode-map
        ("s-c" . casual-ibuffer-tmenu))
  ;; (:map calendar-mode-map
  ;;       ("s-c" . casual-calendar))
  (:map calc-mode-map
        ("s-c" . casual-calc-tmenu))
  (:map calc-alg-map
        ("s-c" . casual-calc-tmenu))
  (:map bookmark-bmenu-mode-map
        ("s-c" . casual-bookmarks-tmenu)))
;; (:map org-agenda-mode-map
;;       ("s-c" . casual-agenda-tmenu)))

(use-package casual-avy
  :after (avy casual)
  :commands casual-avy-tmenu)

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;;; Whitespace
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq require-final-newline t) ;; Add new line in the end of a file on save.
(add-hook 'before-save-hook #'delete-trailing-whitespace) ;; Delete trailing spaces
(setq-default show-trailing-whitespace nil) ;; By default, don't underline trailing spaces

;; Setting default coding system
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#setting-default-coding-system
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Font
(set-frame-font "JetBrains Mono 12" nil t)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Initial frame content
(setq inhibit-splash-screen t) ;; No startup screen
(setq inhibit-startup-message t) ;; No startup message
(setq initial-scratch-message nil) ;; Empty scratch buffer

;; Keyboard-centric user interface
(scroll-bar-mode  -1)
(tool-bar-mode    -1)
(tooltip-mode     -1)
(menu-bar-mode    -1)
(setq visible-bell 1)

(setq confirm-kill-emacs 'y-or-n-p); ;; y and n instead of yes and no when quitting
(fset 'yes-or-no-p 'y-or-n-p)

(use-package smex)  ;; show recent commands when invoking Alt-x (or Cmd+Shift+p)

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :custom
  (repeat-too-dangerous '(kill-this-buffer))
  (repeat-exit-timeout 5))

(use-package embark
  :after (evil evil-collection)
  :commands (embark-act embark-dwim embark-bindings)
  :bind
  ("C-:" . embark-act)
  ("C-;" . embark-dwim))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Scrolling
(pixel-scroll-precision-mode) ;; Smooth scrolling
(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll" :branch "main")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Tab-bar
(setq tab-bar-tab-hints t) ;; Show tab numbers
(setq tab-bar-mode t)
(setq tab-bar-show t)
(setq tab-bar-new-tab-choice "*scratch*")

(global-tab-line-mode)

;; Ligatures
;; https://github.com/mickeynp/ligature.el/wiki#jetbrains-mono
(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                       "<:<" ";;;"))
  (global-ligature-mode t))

;; Default frame size
(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 120))

;; Initial frame content
(setq inhibit-splash-screen t) ; No startup screen
(setq inhibit-startup-message t) ; No startup message
(setq initial-scratch-message nil) ; Empty scratch buffer

;; File system ------------------------------------------------------------------

;; Don't bother with auto save and backups
(setq auto-save-default nil) ;; stop creating #autosave# files
(setq make-backup-files nil) ;; stop creating backup~ files

(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))
(setq create-lockfiles nil)

(use-package envrc
  :hook (after-init . envrc-global-mode))

(defun my/copy-buffer-name ()
  "Copy the buffer name."
  (interactive)
  (kill-new (buffer-file-name)))

(defun my/copy-project-buffer-name ()
  "Copy the buffer name relative to the project."
  (interactive)
  (kill-new (file-relative-name buffer-file-name (my/project-root))))

(recentf-mode t)

(save-place-mode 1)

(use-package dired
  :ensure nil
  :commands (dired)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t))

;; Completion ------------------------------------------------------------------

;; Minibuffer

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<")) ;; "C-+"

;; Optionally make narrowing help available in the minibuffer.
;; You may want to use `embark-prefix-help-command' or which-key instead.
;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

(use-package consult-xref-stack
  :vc
  (:url "https://github.com/brett-lempereur/consult-xref-stack" :branch "main")
  :after consult
  :commands consult-xref-stack-backward)

(use-package consult-flycheck
  :after (consult flycheck))

(use-package consult-org-roam
  :after (consult org-roam)
  :config
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep))

(use-package consult-yasnippet
  :after (consult yasnippet))

(use-package consult-eglot
  :after (consult eglot))

(use-package consult-todo
  :after (consult hl-todo))

(use-package consult-gh
  :after consult)

(use-package consult-gh-embark
  :after consult-gh
  :config
  (consult-gh-embark-mode +1))

(use-package consult-gh-forge
  :after consult-gh
  :config
  (consult-gh-forge-mode +1))

(use-package consult-ghq
  :after (consult affe))

(use-package consult-project-extra
  ;; Suggested bindings
  ;; :bind
  ;; (("C-c p f" . consult-project-extra-find)
  ;;  ("C-c p o" . consult-project-extra-find-other-window)))
  :after (consult flycheck))

;; CAPF

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay 0.1)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current t)
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-separator ?\s)          ;; Orderless field separator
  :init
  (global-corfu-mode))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-emoji))

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Editor ----------------------------------------------------------------------

;; Navigation
(defmacro after-packages (packages &rest body)
  "Execute BODY after all PACKAGES are loaded.
PACKAGES should be a list of package names as symbols."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (pkg)
                 `(eval-after-load ',pkg
                    '(progn))) packages)
     ,@body))

(after-packages (evil avy xref consult-todo)
  (transient-define-prefix nav-transient-menu ()
    "Navigation commands menu."
    ["Navigation"
     ["Changes"
      ("c" "Last change" evil-goto-last-change)]
     ["Things"
      ("g" "Imenu" consult-imenu)
      ("m" "Marks" consult-mark)
      ("t" "TODOs" consult-todo)]
     ["Position"
      ("b" "Jump backward" evil-jump-backward)
      ("f" "Jump forward" evil-jump-forward)
      ("w" "Word" avy-goto-word-0)]
     ["Xref"
      ("a" "Back" xref-go-back)
      ("o" "Forward" xref-go-forward)
      ("A" "Back / Stack" consult-xref-stack-backward)
      ("O" "Forward / Stack" consult-xref-stack-forward)]])
  (global-set-key (kbd "s-u") 'nav-transient-menu))

(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package rg
  :config
  ;; Optionally customise this with:
  ;; rg-keymap-prefix
  (rg-enable-default-bindings)
  (rg-enable-menu))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit))

;; Movement
(use-package avy
  :bind
  (("C-'" . avy-goto-word-0)))

(use-package treesit-jump
  :vc (:url "https://www.github.com/dmille56/treesit-jump" :branch "main")
  :after (treesit avy transient gptel)
  :commands
  (treesit-jump-jump
   treesit-jump-select
   treesit-jump-delete
   treesit-jump-parent-jump
   treesit-jump-gptel-describe)
  :config
  ;; Optional: add some queries to filter out of results (since they can be too cluttered sometimes)
  (setq treesit-jump-queries-filter-list '("inner" "test" "param")))

;; Undo
;; Linear undo and redo.
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/tmp/undo"))
          undo-tree-auto-save-history t
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t)))

;; Formatting
(use-package apheleia
  :hook
  (prog-mode . apheleia-mode)
  :config
  (add-to-list 'apheleia-mode-alist '(json-ts-mode . denofmt-ts))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . denofmt-ts)))

;; Spelling & Grammar

;; By default, Emacs thinks a sentence is a full-stop followed by 2 spaces. Let’s make it full-stop and 1 space.
;; http://sriramkswamy.github.io/dotemacs/
(setq sentence-end-double-space nil)

;; Example:
;;  :bind (("M-$" . jinx-correct)
;;         ("C-M-$" . jinx-languages))
(use-package jinx
  :config
  (global-jinx-mode))

;; Comments
(global-set-key (kbd "s-/") 'comment-line)

;; Structured editing
(use-package combobulate
  :vc
  (:url "https://github.com/mickeynp/combobulate" :branch "master")
  :custom
  (combobulate-key-prefix "C-c o")
  :hook (prog-mode . combobulate-mode))

;; Search
(setq isearch-lazy-count t)

(use-package re-builder
  :ensure nil
  :custom
  ;; Recommended. Similar to read but you don’t have the issue of
  ;; backslash plague that haunts the default settings. Example:
  ;; \\(foo\\\|bar\\)
  (reb-re-syntax 'string))

;; Editor Config
(customize-set-variable 'editorconfig-mode t)

;; Language: All ----------------------------------------------------------------

(use-package flycheck
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [0 0 0 0 0 24 60 126 126 60 24 0 0 0 0 0 0 0 0 0])
  (flycheck-define-error-level 'error
    :severity 100
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 10
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity -10
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    :fringe-face 'flycheck-fringe-info)
  (global-flycheck-mode))

(use-package dape
  :custom
  (dape-inlay-hints t)
  (dape-cwd-function 'my/project-root)
  (dape-buffer-window-arrangement 'right)
  :hook
  (dape-start . (lambda () (save-some-buffers t t)))
  (dape-compile . kill-buffer))

(use-package repeat
  :ensure nil
  :config
  (repeat-mode))

;; Language: Documents ----------------------------------------------------------

(use-package pandoc-mode
  :after hydra
  :hook
  (markdown-mode . panodic-mode))

;; Language: Lisps --------------------------------------------------------------

(use-package parinfer-rust-mode
  :diminish
  :hook emacs-lisp-mode)

;; Language: Elixir --------------------------------------------------------------

(use-package elixir-ts-mode)

(use-package heex-ts-mode)

;; Language: Erlang --------------------------------------------------------------

(use-package erlang-ts
  :vc (:url "https://github.com/erlang/emacs-erlang-ts" :branch "main")
  :mode ("\\.erl\\'" . erlang-ts-mode)
  :defer t)

;; Language: Python --------------------------------------------------------------

(use-package python-mode)

(use-package poetry)

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Language: Fish ---------------------------------------------------------------

(use-package fish-mode)

;; Language: Clojure(Script) ----------------------------------------------------

(use-package clojure-mode)

(use-package clojure-ts-mode
  :hook
  (clojure-ts-mode . cider-mode)
  (clojure-ts-mode . rainbow-delimiters-mode))

(use-package flycheck-clojure
  :defer 1
  :after (clojure-mode clojure-ts-mode flycheck)
  :commands flycheck-clojure-setup
  :hook
  (flycheck-mode . flycheck-clojure-setup))

;; Language: Flix ---------------------------------------------------------------

;; Define flix mode
(define-derived-mode flix-mode prog-mode "Flix"
  "Major mode for editing Flix source code."
  :group 'flix
  (setq-local comment-start "//")
  (setq-local comment-end ""))

;; Associate .flix files with flix-mode
(add-to-list 'auto-mode-alist '("\\.flix\\'" . flix-mode))

;; Language: Rust ---------------------------------------------------------------

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package flycheck-rust
  :defer 1
  :after (rust-mode flycheck)
  :hook
  (flycheck-mode . flycheck-rust-setup))

(use-package cargo-mode
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t))

(use-package cargo-transient
  :custom
  (cargo-transient-buffer-name-function #'project-prefixed-buffer-name))

;; Language: Swift --------------------------------------------------------------

(use-package swift-mode)

(use-package swift-ts-mode)

(use-package flycheck-swift
  :defer 1
  :after (swift-mode flycheck)
  :hook
  (flycheck-mode . flycheck-swift-setup))

;; Language: JSON --------------------------------------------------------------

(use-package json-mode)

;; Language: Docker -------------------------------------------------------------

(use-package docker
  :after transient)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

;; Language: YAML ---------------------------------------------------------------

(use-package yaml-mode)

;; Language: CSV ----------------------------------------------------------------

(use-package csv-mode)

;; Language: Just ---------------------------------------------------------------

(use-package just-ts-mode)

;; Language: Markdown ---------------------------------------------------------------

(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer t
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

;; Language: ePub ---------------------------------------------------------------

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;; Language: Vespa --------------------------------------------------------------

(define-derived-mode vespa-schema-mode prog-mode "Vespa Schema"
  "Major mode for editing Vespa schema definition files."
  :group 'vespa
  (setq-local comment-start "#")
  (setq-local comment-end ""))

(add-to-list 'auto-mode-alist '("\\.sd\\'" . vespa-schema-mode))

(provide 'init)

;;; init.el ends here
