;;; init.el --- Emacs init -*- lexical-binding: t; -*-

;; -----------------------------------------------------------------------------

(require 'package)

(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")    t)

(package-initialize)

(setq use-package-enable-imenu-support t
      use-package-always-ensure t)

(require 'use-package)

(setq package-lock-file (expand-file-name "lock.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "user/" user-emacs-directory))

(use-package compile-angel
  :ensure t
  :demand t
  :disabled
  :config
  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose t)

  ;; Uncomment the line below to compile automatically when an Elisp file is saved
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files before they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode 1))

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
  (package-install-upgrade-built-in t)
  (ad-redefinition-action 'accept)
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
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (defun my/save-package-versions-to-file ()
    "Save list of installed packages and versions."
    (interactive)
    (with-temp-buffer  ;; Create a temporary buffer that will be discarded when done
      (package-initialize)  ;; Make sure packages are initialized
      (insert (format ";; Generated: %s. DO NOT EDIT.\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))  ;; Add timestamp at the top
      (let ((packages (sort (mapcar #'car package-alist) #'string<)))  ;; Get sorted list of package names
        (dolist (pkg packages)  ;; Loop through each package
          (let* ((pkg-desc (cadr (assq pkg package-alist)))  ;; Get package descriptor
                 (version (package-version-join (package-desc-version pkg-desc))))  ;; Extract and format version
            (insert (format ";; %s: %s\n" pkg version)))))  ;; Format and insert package info
      (write-file package-lock-file)))  ;; Write buffer contents to the specified file
  (add-hook 'kill-emacs-hook #'my/save-package-versions-to-file))

(defun list-upgradeable-builtin-packages ()
  "List builtin packages that have newer versions available in ELPA."
  (interactive)
  (package-refresh-contents)
  (let ((upgradeable '()))
    (dolist (builtin-pkg package--builtin-versions)
      (let* ((pkg-name (car builtin-pkg))
             (builtin-version (cdr builtin-pkg))
             (available-desc (cadr (assq pkg-name package-archive-contents))))
        (when (and available-desc
                   (package-desc-p available-desc))
          (let ((available-version (package-desc-version available-desc)))
            (when (version-list-< builtin-version available-version)
              (push (list pkg-name builtin-version available-version) upgradeable))))))
    (if upgradeable
        (progn
          (with-current-buffer (get-buffer-create "*Upgradeable Builtins*")
            (erase-buffer)
            (insert "Builtin packages with available upgrades:\n\n")
            (dolist (pkg upgradeable)
              (insert (format "%-20s %s -> %s\n"
                              (car pkg)
                              (package-version-join (cadr pkg))
                              (package-version-join (caddr pkg)))))
            (display-buffer (current-buffer)))
          (message "Found %d upgradeable builtin packages" (length upgradeable)))
      (message "No upgradeable builtin packages found"))))


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

(with-current-buffer "*Messages*"
  (setq-local scroll-conservatively 101))

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

;;; Common

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

(defmacro after-packages (packages &rest body)
  "Execute BODY after all PACKAGES are loaded.
PACKAGES should be a list of package names as symbols."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (pkg)
                 `(eval-after-load ',pkg
                    '(progn))) packages)
     ,@body))

;;; Server

;; Hint: emacsclient -n file1 file2 (Use -c to open in a new frame)
;; Example: emacsclient --socket-name ~/.config/emacs/server/server FILE

(setq server-socket-path "~/.config/emacs/server") ; Make the directory if it doesn't exist
(make-directory server-socket-path t)
(setq server-socket-dir server-socket-path)

(load "server")
(unless (< emacs-major-version 23)
  (defun server-ensure-safe-dir (_dir) "Noop" t))

(unless (server-running-p) (server-start))

;;; OS

;; All

(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)

(setq auth-sources '("~/.authinfo"))

;; Tidy up .emacs.d mess
(use-package no-littering)

(use-package savehist
  :config
  (setq history-length 1000)
  (setq savehist-autosave-interval 60)
  (setq savehist-additional-variables
        '(search-ring                ; Search history
          regexp-search-ring         ; Regexp search history
          extended-command-history   ; M-x history
          mark-ring                  ; Marks
          global-mark-ring           ; Global marks
          shell-command-history      ; Shell command history
          register-alist             ; Registers
          file-name-history          ; Minibuffer file name history
          tablist-named-filter
          evil-jumps-history))
  (savehist-mode +1))

;; Disabling suspend-frame binding
;; Very annoying binding, lets get rid of it.
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#disabling-suspend-frame-binding
(global-unset-key (kbd "C-z"))

;; macOS

(when (memq window-system '(mac ns))
  (setq dired-use-ls-dired nil))

(setq delete-by-moving-to-trash t)
(if (eq system-type 'darwin)
    (setq trash-directory "~/.Trash"))

;; https://emacs.stackexchange.com/a/41767
;; Alternative to try: https://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
(setq use-dialog-box nil)

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-debug t)
  (setq exec-path-from-shell-shell-name "/opt/homebrew/bin/fish")
  (setq exec-path-from-shell-arguments nil)
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; Manually ensure Homebrew paths are available
(when (and (memq window-system '(mac ns))
           (file-directory-p "/opt/homebrew/bin"))
  (add-to-list 'exec-path "/opt/homebrew/bin" t)
  (add-to-list 'exec-path "/opt/homebrew/sbin" t)
  (setenv "PATH" (concat "/opt/homebrew/bin:" "/opt/homebrew/sbin:" (getenv "PATH"))))

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

;;; Project

(use-package project
  :ensure nil
  :init
  (setq project-vc-extra-root-markers '(;; Org
                                        "TODO.org"
                                        ;; Clojure(Script)
                                        "project.clj" "deps.edn"
                                        ;; Jujutsu
                                        ".jj"
                                        ;; Flix
                                        "flix.toml"))
  :config
  (add-to-list 'project-vc-ignores "tmp/")
  (add-to-list 'project-vc-ignores ".jj")
  (add-to-list 'project-vc-ignores ".cache/")
  (add-to-list 'project-vc-ignores ".build/")
  (add-to-list 'project-vc-ignores "target/")
  (setq src-dir (expand-file-name "~/src/"))
  (setq forges-dirs '("github.com" "gitlab.com"))
  (defun my/auto-discover-projects ()
    "Automatically discover and add projects from configured forge directories."
    (interactive)
    (let ((src-dir (expand-file-name "~/src/"))
          (forges-dirs '("github.com" "gitlab.com")))

      ;; Run project-remember-projects-under on each forge directory
      (dolist (forge forges-dirs)
        (let ((forge-path (file-name-concat src-dir forge)))
          (message "Adding projects from %s..." forge-path)
          (project-remember-projects-under forge-path t)
          (message "Done adding projects from %s" forge-path))))

    (message "Project discovery complete"))
  (transient-define-prefix project-transient-menu ()
    "Project command menu."
    [[:description "Navigation"
                   ("s" "Search" consult-ripgrep)
                   ("b" "Buffers" consult-project-buffer)
                   ("f" "Files" project-find-file)
                   ("l" "Line" consult-line)
                   ("d" "Layout" project-dired)
                   ("i" "Sibling" find-sibling-file)]
     [:description "Execution"
                   ("r" "Run" project-run)
                   ("b" "Compile" project-compile)]]
    ["Management"
     [("t" "New tab" tab-new)
      ("c" "Close tab" tab-close)
      ("n" "Rename tab" my/rename-tab-to-project-name)]
     [("p" "Switch (Known)" project-switch-project)
      ("P" "Switch (All)" consult-ghq-switch-project)]])
  (defun project-run (command)
    "Run COMMAND in the current project's root directory."
    (interactive (list (read-shell-command "Run command: "
                                           (or (bound-and-true-p run-command)
                                               (car compilation-history)
                                               ""))))
    (let ((default-directory (project-root (project-current t))))
      (compile command)))
  (add-to-list 'project-switch-commands '(project-run "Run" ?r))
  :bind
  ("s-p" . project-transient-menu))

(defun my/project-root ()
  (interactive)
  (project-root (project-current)))

(defun my/rename-tab-to-project-name ()
  "Rename the current tab to the project name."
  (interactive)
  (when-let* ((project-name (file-name-nondirectory (directory-file-name (my/project-root)))))
    (tab-rename project-name)))

(use-package easysession
  :custom
  (easysession-mode-line-misc-info t)  ; Display the session in the modeline
  (easysession-save-interval (* 10 60))  ; Save every 10 minutes
  (easysession-mode-line-misc-info-format
   '(" Session:"
     easysession-mode-line-session-name " "))

  :init
  (add-hook 'emacs-startup-hook #'easysession-save-mode 103)

  :bind
  (("C-c l" . easysession-switch-to)
   ("C-c s" . easysession-save-as))

  :config
  ;; How to only persist and restore visible buffers
  (defun my-easysession-visible-buffer-list ()
    "Return a list of all visible buffers in the current session.
This includes buffers visible in windows or tab-bar tabs."
    (let ((visible-buffers '()))
      (dolist (buffer (buffer-list))
        (when (or
               ;; Windows
               (get-buffer-window buffer 'visible)
               ;; Tab-bar windows
               (and (bound-and-true-p tab-bar-mode)
                    (fboundp 'tab-bar-get-buffer-tab)
                    (tab-bar-get-buffer-tab buffer t nil)))
          (push buffer visible-buffers)))
      visible-buffers))

  (setq easysession-buffer-list-function #'my-easysession-visible-buffer-list)

  ;; How to create an empty session setup
  (defun my-empty-easysession ()
    "Set up a minimal environment when easysession creates a new session."
    (when (and (boundp 'tab-bar-mode) tab-bar-mode)
      (tab-bar-close-other-tabs))
    (delete-other-windows)
    (scratch-buffer))

  (add-hook 'easysession-new-session-hook #'my-empty-easysession))

;;; VCS

(use-package smerge-mode
  :ensure nil
  :after transient
  :config
  (transient-define-prefix smerge-transient-menu ()
    ["Navigation"
     ("n" "Next conflict" smerge-next)
     ("p" "Previous conflict" smerge-prev)]
    ["Resolve"
     ("a" "Keep all variants" smerge-keep-all)
     ("b" "Keep base" smerge-keep-base)
     ("l" "Keep lower" smerge-keep-lower)
     ("u" "Keep upper" smerge-keep-upper)
     ("c" "Keep current" smerge-keep-current)]
    ["Diff"
     ("E" "Ediff" smerge-ediff)
     ("C" "Combine" smerge-combine-with-next)]
    ["Other"
     ("r" "Resolve" smerge-resolve)
     ("R" "Resolve all" smerge-resolve-all)
     ("q" "Quit" smerge-mode)])
  :bind (:map smerge-mode-map
              ("?" . smerge-transient-menu)))

(use-package magit
  :after transient
  :commands
  (magit-status magit-blame magit-blame-quit)
  :custom
  ;; Setting ‘forge-add-default-bindings’ to nil in ‘evil-collection-forge-setup’.
  ;; To suppress this message you can set this variable to nil in your init.el file.
  (forge-add-default-bindings nil)
  (magit-git-executable "/opt/homebrew/bin/git")
  :init
  (setopt magit-format-file-function #'magit-format-file-all-the-icons)
  :bind
  (("s-g" . magit-status)
   :map magit-status-mode-map
   ("*" . th/magit-aux-commands))
  :config
  (defun my/find-conventional-commit-scopes ()
    "Find all scopes used in conventional commits in the current Git project."
    (interactive)
    (let* ((default-directory (magit-toplevel))
           (cache-dir (expand-file-name ".cache" (or (my/project-root) default-directory)))
           (cache-file (expand-file-name "commit-scopes.txt" cache-dir))
           (temp-buffer (generate-new-buffer " *commit-scopes-temp*")))

      ;; Create cache directory if it doesn't exist
      (unless (file-exists-p cache-dir)
        (make-directory cache-dir t))

      ;; Use Emacs Lisp to extract scopes directly
      (with-current-buffer temp-buffer
        (call-process "git" nil t nil "log" "--pretty=format:%s")
        (goto-char (point-min))

        ;; Extract scopes using a regular expression
        (let ((scopes '()))
          (while (re-search-forward "\\(feat\\|fix\\|docs\\|style\\|refactor\\|perf\\|test\\|build\\|ci\\|chore\\|revert\\)(\\([^)]+\\))" nil t)
            (let ((scope-text (match-string 2)))
              ;; Split by comma and add each scope
              (dolist (scope (split-string scope-text "," t "[ \t]+"))
                (push (string-trim scope) scopes))))

          ;; Write unique scopes to the cache file
          (with-temp-file cache-file
            (insert (mapconcat #'identity (delete-dups scopes) "\n")))))

      (kill-buffer temp-buffer)

      ;; Return the cache file path
      cache-file))
  (defun my/get-commit-scopes ()
    "Get commit scopes from cache or generate them if needed."
    (interactive)
    (let* ((default-directory (magit-toplevel))
           (cache-dir (expand-file-name ".cache" (or (my/project-root) default-directory)))
           (cache-file (expand-file-name "commit-scopes.txt" cache-dir)))
      (unless (and (file-exists-p cache-file)
                   (> (time-to-seconds (time-since (file-attribute-modification-time (file-attributes cache-file))))
                      (* 60 60 24))) ; Cache for 24 hours
        (my/find-conventional-commit-scopes))

      (when (file-exists-p cache-file)
        (with-temp-buffer
          (insert-file-contents cache-file)
          (split-string (buffer-string) "\n" t)))))
  (defun my/conventional-commit-prompt ()
    "Prompt for conventional commit type with scope completion."
    (interactive)
    (let ((commit-types '("feat" "fix" "docs" "style" "refactor" "perf" "test" "build" "ci" "cd" "chore" "revert")))
      (if (y-or-n-p "Use conventional commit format? ")
          (let* ((type (completing-read "Commit type: " commit-types nil t))
                 (scopes (my/get-commit-scopes))
                 ;; Allow multiple selections with comma
                 (scope-input (completing-read "Scope (optional, comma-separated for multiple): " scopes nil nil)))
            (insert type
                    (if (string-empty-p scope-input)
                        ""
                      (concat "(" scope-input ")"))
                    ": ")
            (evil-insert-state))
        (evil-insert-state))))
  (add-hook 'git-commit-setup-hook #'my/conventional-commit-prompt)
  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)])
  ;; I want my personal commands transient to be bound to * and be shown
  ;; in the Magit dispatch transient (which is bound to ? in Magit status
  ;; buffers and C-x M-g in any Magit enabled buffer)
  (transient-append-suffix 'magit-dispatch "!"
    '("*" "My Magit Cmds" th/magit-aux-commands))
  (let ((git-path "/Applications/Xcode.app/Contents/Developer/usr/bin/git"))
    (when (file-exists-p git-path)
      (setq magit-git-executable git-path))))

(use-package magit-prime
  :after magit
  :config
  (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache))

(after-packages (magit)) (require 'git-spice)

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package magit-popup
  :after magit)

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package ghub
  :after (magit transient)
  :defer 1)

(use-package forge
  :after (magit transient ghub)
  :defer 1)

(use-package pr-review
  :defer 1)

(use-package git-modes
  :after magit)

;; This package is easiest way to open particular link on
;; github/gitlab/bitbucket/stash/git.savannah.gnu.org/sourcehut from
;; Emacs. It supports various kind of emacs buffer.
(use-package browse-at-remote
  :after magit)

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-side 'right)
  (diff-hl-show-staged-changes t)
  (diff-hl-update-async t)
  :config
  (diff-hl-margin-mode 1)
  :bind
  (("M-p" . diff-hl-previous-hunk)
   ("M-n" . diff-hl-next-hunk)))

;; Create URLs to files and commits in repository hosting services
(use-package git-link
  :after magit
  :defer 1
  :custom
  (git-link-use-commit t)
  (git-link-default-remote "main"))

;; Why was this line changed
(use-package git-messenger
  :after magit)

;; View versions of a file
(use-package git-timemachine
  :after transient
  :commands git-timemachine)

(use-package git-commit-ts-mode
  :vc
  :disabled
  (:url "https://github.com/danilshvalov/git-commit-ts-mode")
  :init
  (add-to-list 'major-mode-remap-alist
               '(git-commit-mode . git-commit-ts-mode)))

(use-package difftastic
  :after (magit transient)
  :config
  (difftastic-bindings-mode))

;;; Tree-sitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(use-package tree-sitter-langs)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Defer treesit-auto to run after file is opened
  (setq treesit-auto-install 'prompt)
  ;; Don't start it globally.
  ;; (global-treesit-auto-mode)
  (defun my/enable-treesit-auto-deferred ()
    "Enable treesit-auto after file is opened to improve startup performance."
    (run-with-idle-timer 0.1 nil
                         (lambda ()
                           (when (and buffer-file-name (treesit-available-p))
                             (treesit-auto--set-major-remap (current-buffer))))))
  :hook
  (after-change-major-mode . my/enable-treesit-auto-deferred))

;;; LSP

(use-package eglot
  :ensure nil
  :init
  ;; https://github.com/minad/corfu/wiki#filter-list-of-all-possible-completions-with-completion-style-like-orderless
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil) ;; The value of nil or 0 means don’t block at all during the waiting period
  :config
  ;; Defer eglot startup to improve file opening performance
  (defun my/eglot-ensure-deferred ()
    "Start eglot after a short delay to improve file opening performance."
    (run-with-idle-timer 0.5 nil #'eglot-ensure))
  :hook
  (clojure-mode . my/eglot-ensure-deferred)
  (clojure-ts-mode . my/eglot-ensure-deferred)
  (elixir-mode . my/eglot-ensure-deferred)
  (elixir-ts-mode . my/eglot-ensure-deferred)
  (heex-ts-mode . my/eglot-ensure-deferred)
  (flix-mode . my/eglot-ensure-deferred)
  (go-mode . my/eglot-ensure-deferred)
  (go-ts-mode . my/eglot-ensure-deferred)
  (latex-mode . my/eglot-ensure-deferred)
  (markdown-mode . my/eglot-ensure-deferred)
  (markdown-ts-mode . my/eglot-ensure-deferred)
  (python-mode . my/eglot-ensure-deferred)
  (python-ts-mode . my/eglot-ensure-deferred)
  (rust-mode . my/eglot-ensure-deferred)
  (rust-ts-mode . my/eglot-ensure-deferred)
  (swift-mode . my/eglot-ensure-deferred)
  (swift-ts-mode . my/eglot-ensure-deferred)
  (toml-mode . my/eglot-ensure-deferred)
  (toml-ts-mode . my/eglot-ensure-deferred)
  (conf-toml-mode . my/eglot-ensure-deferred)
  (javascript-mode . my/eglot-ensure-deferred)
  (typescript-mode . my/eglot-ensure-deferred)
  (typescript-ts-mode . my/eglot-ensure-deferred)
  (typst-ts-mode . my/eglot-ensure-deferred)
  (vespa-schema-mode . my/eglot-ensure-deferred)
  (yaml-mode . my/eglot-ensure-deferred)
  (yaml-ts-mode . my/eglot-ensure-deferred)
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
               `(javascript-mode . ,(eglot-alternatives
                                     '(("deno" "lsp")))))
  (add-to-list 'eglot-server-programs
               `(typescript-mode . ,(eglot-alternatives
                                     '(("deno" "lsp")))))
  (add-to-list 'eglot-server-programs
               `(typescript-ts-mode . ,(eglot-alternatives
                                        '(("deno" "lsp")))))
  (add-to-list 'eglot-server-programs
               `(latex-mode . ,(eglot-alternatives
                                '(("texlab")))))
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
  (add-to-list 'eglot-server-programs
               `(flix-mode . ,(eglot-alternatives
                               '(("flix" "lsp")))))
  (add-to-list 'eglot-server-programs
               `(markdown-mode . ,(eglot-alternatives
                                   '(("marksman")))))
  (add-to-list 'eglot-server-programs
               `(markdown-ts-mode . ,(eglot-alternatives
                                      '(("marksman")))))
  (add-to-list 'eglot-server-programs
               `(typst-ts-mode . ,(eglot-alternatives
                                   '(("tinymist" "lsp")))))
  (add-to-list 'eglot-server-programs
               `(toml-mode . ,(eglot-alternatives
                               '(("taplo" "lsp" "stdio")))))
  (add-to-list 'eglot-server-programs
               `(toml-ts-mode . ,(eglot-alternatives
                                  '(("taplo" "lsp" "stdio")))))
  (add-to-list 'eglot-server-programs
               `(conf-toml-mode . ,(eglot-alternatives
                                    '(("taplo" "lsp" "stdio")))))

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

(use-package eglot-booster
  :after eglot
  :vc
  (:url "https://github.com/jdtsmith/eglot-booster")
  :config
  (eglot-booster-mode))

(use-package flycheck-eglot
  ;; Disabled at 2025-08-03 since it means that the doom-modeline
  ;; shows 0 for all Flycheck results.
  ;;
  ;; This is even with `flycheck-eglot-exclusive nil`.
  :disabled
  :diminish
  :after (flycheck eglot)
  :custom
  (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

;;; AI

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
  :custom
  (gptel-use-tools t)
  (gptel-stream t)
  (gptel-default-mode 'org-mode)
  (gptel-display-buffer-action
   '((display-buffer-reuse-window display-buffer-in-side-window)
     (side . right)
     (window-width . 80)
     (slot . 0)))
  :init
  (defvar gptel-save-directory (expand-file-name "chats" user-emacs-directory)
    "Directory to save gptel conversations.")
  :config
  (defun my/gptel-use-openai ()
    "Set gptel backend to OpenAI"
    (interactive)
    (setq gptel-model 'gpt-5)
    (setq gptel-backend (gptel-make-openai "OpenAI"
                          :stream t
                          :key (my/get-password "api.openai.com" "me")))
    (message "Switched gptel backend: OpenAI"))
  (defun my/gptel-use-claude-sonnet-4 ()
    "Set gptel backend to Claude Sonnet 4."
    (interactive)
    (setq gptel-model 'claude-sonnet-4-20250514)
    (setq gptel-backend (gptel-make-anthropic "Claude"
                          :stream t
                          :key (my/get-password "anthropic.com" "me")))
    (message "Switched gptel backend: Claude Sonnet 4"))
  (defun my/gptel-use-claude-sonnet-3-7 ()
    "Set gptel backend to Claude Sonnet 3.7."
    (interactive)
    (setq gptel-model 'claude-3-7-sonnet-20250219)
    (setq gptel-backend (gptel-make-anthropic "Claude"
                          :stream t
                          :key (my/get-password "anthropic.com" "me")))
    (message "Switched gptel backend: Claude Sonnet 3.7"))
  (defun my/gptel-use-gemini ()
    "Set gptel backend to Gemini."
    (interactive)
    (setq gptel-model 'gemini-pro-latest)
    (setq gptel-backend (gptel-make-gemini "Gemini"
                          :key (my/get-password "aistudio.google.com" "apikey")
                          :stream t))
    (message "Switched gptel backend: Gemini"))
  (defun my/gptel-use-perplexity ()
    "Set gptel backend to Perplexity."
    (interactive)
    (setq gptel-backend (gptel-make-perplexity "Perplexity"
                          :stream t
                          :key (my/get-password "perplexity.ai" "apikey")))
    (message "Switched gptel backend: Perplexity"))
  (defun my/gptel-use-deepseek-chat ()
    "Set gptel backend to DeepSeek Chat."
    (interactive)
    (setq gptel-model 'deepseek-chat)
    (setq gptel-backend (gptel-make-deepseek "DeepSeek"
                          :stream t
                          :key (my/get-password "deepseek.com" "apikey")))
    (message "Switched gptel backend: DeepSeek Chat"))
  (defun my/gptel-use-deepseek-reasoner ()
    "Set gptel backend to DeepSeek Chat."
    (interactive)
    (setq gptel-model 'deepseek-reasoner)
    (setq gptel-backend (gptel-make-deepseek "DeepSeek"
                          :stream t
                          :key (my/get-password "deepseek.com" "apikey")))
    (message "Switched gptel backend: DeepSeek Reasoner"))
  ;; Set Claude as default
  (my/gptel-use-claude-sonnet-4)
  (defun my/gptel-toggle-sidebar ()
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
          (setq mode-line-format nil)))))
  (defun my/gptel-reset-chat ()
    "Reset the AI Chat buffer by killing and recreating it."
    (interactive)
    (let ((buffer-name "AI Chat"))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)
        (message "AI Chat buffer has been reset"))))
  (defun my/gptel-send-at-eol ()
    "Move to end of line, and then send to gptel."
    (interactive)
    (call-interactively 'end-of-buffer)
    (call-interactively 'gptel-send))
  (require 'gptel-integrations)
  (defun my/gptel-save-chat ()
    "Save current gptel buffer to chat directory with timestamp."
    (interactive)
    (when (and (boundp 'gptel-mode) gptel-mode)
      (unless (file-exists-p gptel-save-directory)
        (make-directory gptel-save-directory t))
      (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
             (buffer-name-clean (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" (buffer-name)))
             (filename (expand-file-name (concat buffer-name-clean "-" timestamp ".org")
                                         gptel-save-directory)))
        (write-region (point-min) (point-max) filename)
        (message "Saved chat to %s" filename))))
  (defun my/gptel-close-chat (&optional buffer)
    "Close current gptel buffer or prompt to select one.
If BUFFER is provided, close that buffer directly."
    (interactive)
    (let* ((gptel-buffers (cl-remove-if-not
                           (lambda (b)
                             (string-match-p "\\*\\(DeepSeek\\|Claude\\|Gemini\\)\\*"
                                             (buffer-name b)))
                           (buffer-list))))
      (cond
       (buffer (kill-buffer buffer))
       ((or (derived-mode-p 'gptel-mode) (bound-and-true-p gptel-mode))
        (kill-buffer (current-buffer)))
       (t (let ((buf (completing-read "Close gptel buffer: "
                                      (mapcar #'buffer-name gptel-buffers)
                                      nil t)))
            (when buf (kill-buffer buf)))))))
  (defun my/gptel-select-chat ()
    "Select a gptel buffer to switch to."
    (interactive)
    (let* ((gptel-buffers (cl-remove-if-not
                           (lambda (b)
                             (string-match-p "\\*\\(DeepSeek\\|Claude\\|Gemini\\)\\*"
                                             (buffer-name b)))
                           (buffer-list)))
           (buffer (completing-read "Select gptel buffer: "
                                    (mapcar #'buffer-name gptel-buffers)
                                    nil t)))
      (when buffer
        (switch-to-buffer buffer))))
  :hook
  (gptel-post-stream . gptel-auto-scroll)
  (gptel-post-response-functions . gptel-end-of-response)
  (gptel-post-rewrite-functions . (lambda (&rest _)
                                    "Reset the rewrite directive after accepting a rewrite."
                                    (setq-local gptel--rewrite-message "Refactor: ")))
  (gptel-mode . (lambda () (org-indent-mode -1)))
  :bind
  ("s-a" . gptel-menu)
  ("<f5>" . my/gptel-toggle-sidebar)
  (:map gptel-mode-map
        ("C-c C-s" . my/gptel-save-chat)
        ("C-c C-c" . my/gptel-send-at-eol)
        ("C-c C-k" . my/gptel-close-chat)
        ("C-c C-b" . my/gptel-select-chat)))

(use-package gptel-quick
  :after (gptel posframe pdf-tools embark)
  :commands gptel-quick
  :vc
  (:url "https://github.com/karthink/gptel-quick")
  :config
  (keymap-set embark-general-map "?" #'gptel-quick))

(use-package mcp-hub
  :vc
  (:url "https://github.com/lizqwerscott/mcp.el")
  :init
  (setq mcp-hub-servers
        `(("desktop-commander" . (:command "npx" :args ("-y" "@wonderwhy-er/desktop-commander")))
          ("github" . (
                       :command "docker"
                       :args ("run" "-i" "--rm" "-e" ,(concat "GITHUB_PERSONAL_ACCESS_TOKEN=" (my/get-password "api.github.com" "jesse-c^mcp")) "ghcr.io/github/github-mcp-server")))
          ("tavily" . (
                       :command "npx"
                       :args ("-y" "tavily-mcp@latest")
                       :env (:TAVILY_API_KEY ,(my/get-password "tavily.com" "apikey"))))
          ("playwright" . (
                           :command "mcp-server-playwright"
                           :args ()))))
  :config
  ;; Defer MCP hub startup to improve Emacs startup performance
  (defun my/mcp-hub-start-deferred ()
    "Start MCP hub after a delay to improve startup performance."
    (run-with-idle-timer 2.0 nil #'mcp-hub-start-all-server))
  :hook
  (after-init . my/mcp-hub-start-deferred))

(defun gptel-mcp-register-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (apply #'gptel-make-tool
                       tool))
            tools)))

(defun gptel-mcp-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (push (gptel-get-tool path)
                        gptel-tools)))
            tools)))

(defun gptel-mcp-close-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (setq gptel-tools
                        (cl-remove-if #'(lambda (tool)
                                          (equal path
                                                 (list (gptel-tool-category tool)
                                                       (gptel-tool-name tool))))
                                      gptel-tools))))
            tools)))

(use-package acp
  :vc
  (:url "https://github.com/xenodium/acp.el"))

(use-package agent-shell
  :after (acp shell-maker)
  :vc
  (:url "https://github.com/xenodium/agent-shell")
  :config
  (setq agent-shell-anthropic-key nil)
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t))
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication
         :api-key (lambda () (my/get-password "api.openai.com" "me"))))
  (setq agent-shell-goose-authentication
        (agent-shell-make-goose-authentication
         :openai-api-key (lambda () (my/get-password "api.openai.com" "me")))))

(use-package codeium
  :vc
  (:url "https://github.com/Exafunction/codeium.el")

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

;;; Snippets

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :diminish
  :config
  (yas-reload-all))

(use-package yasnippet-capf
  :after (cape corfu yasnippet)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;;; Terminal / Shell

(use-package eat
  :vc
  (:url "https://codeberg.org/akib/emacs-eat.git")
  :defer 1
  :hook
  (eshell-load-hook . eat-eshell-mode))

(use-package vterm
  :defer 1)

(use-package shell-maker)

;;; Notes

(setq org-dir (expand-file-name "~/Documents/org/"))
(setq org-tasks-path (file-name-concat org-dir "tasks.org"))

(defvar org-roam-dir (file-name-concat org-dir "roam/"))
(defvar org-roam-dailies-dir "daily/")
(defvar org-roam-dailies-path (file-name-concat org-roam-dir org-roam-dailies-dir))

;; Using tasks
;;
;; 1. Capture them with the <C-o> entry point.
;; 2. Refile with <g> (refresh) or <r> (rebuild) in the agenda view
;;
;; Tip: Use the Casual menu at <C-s>
;; Tip: <C-c C-w> (refile) in the capture buffer
;; Tip: Use tasks as trees of tasks

;; https://xenodium.com/emacs-dwim-do-what-i-mean
(defun ar/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))

(use-package org
  :ensure nil
  :defer 1
  :custom
  (org-log-done t)
  ;; Something instead of "...", such as " ▾ "
  (org-ellipsis " ↲ ")
  ;; Entities are things like \alpha
  (org-pretty-entities t)
  ;; Stop the empty space
  (org-edit-src-content-indentation 0)
  ;; Display it as if with the appropriate major mode
  (org-src-fontify-natively t)
  ;; Tab should behave as if editing that kind of code
  (org-src-tab-acts-natively t)
  ;; A timestamp or a note will be logged when an entry is refiled
  (org-log-refile t)
  (org-confirm-babel-evaluate nil)
  (org-startup-with-inline-images t)
  (org-todo-keywords '((sequence "TODO(t)" "BLOCKED(b)" "IN-PROGRESS(i)" "REVIEW(r)" "|" "DONE(d)" "CANCELLED(c)")))
  (calendar-week-start-day 1)
  (visual-line-fringe-indicators '(nil right-curly-arrow))
  :config
  (setq org-agenda-files (list org-tasks-path))
  (setq diary-show-holidays-flag nil)
  ;; Performance optimizations
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-span 'day)
  (setq org-agenda-use-tag-inheritance t)
  (setq org-agenda-ignore-properties '(effort appt stats category))
  ;; Hide file names and categories in agenda since we only use one file, show tags
  (setq org-agenda-prefix-format "  %i %?-12t% s")
  ;; Show tags in agenda by not hiding them
  (setq org-agenda-remove-tags nil)
  (setq org-agenda-hide-tags-regexp nil)
  ;; Custom agenda commands
  (setq org-agenda-custom-commands
        '(("d" "Today's tasks"
           ((agenda "" ((org-agenda-span 1)
                        (org-agenda-show-log t)
                        (org-agenda-log-mode-items '(closed clock state))))
            (todo "TODO")))
          ("w" "Week view"
           ((agenda "" ((org-agenda-span 7)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "today")
                        (org-agenda-show-log t)
                        (org-agenda-log-mode-items '(closed clock state))))))))
  ;; Auto-save agenda files when modified
  (advice-add 'org-agenda-todo :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-priority :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-schedule :after #'org-save-all-org-buffers)
  (advice-add 'org-agenda-deadline :after #'org-save-all-org-buffers)
  ;; Optional refile for completed tasks (disabled by default)
  (defun my/auto-refile-done-tasks ()
    "Prompt to refile DONE and CANCELLED tasks."
    (when (and (member org-state '("DONE" "CANCELLED"))
               (org-get-heading t t t t)
               (y-or-n-p "Refile this completed task? "))
      (let ((org-refile-targets '((org-agenda-files :maxlevel . 2))))
        (org-refile))))
  ;; Uncomment the line below to enable auto-refile prompting
  ;; (add-hook 'org-after-todo-state-change-hook #'my/auto-refile-done-tasks)
  ;; Function to interactively collect tags
  (defun my/collect-tags ()
    "Collect tags one by one, stopping at empty input, and format them as :tag1:tag2:..."
    (let ((tags '())
          tag)
      (while (not (string-empty-p (setq tag (read-string "Tag (empty to finish): "))))
        (push tag tags))
      (if tags
          (concat ":" (string-join (nreverse tags) ":") ":")
        "")))
  (setq org-capture-templates
        '(("t" "Task" entry
           (file org-tasks-path)
           "* TODO %^{Description} %(my/collect-tags)\n  SCHEDULED: %^t"
           :immediate-finish nil
           :refile-targets ((org-agenda-files :maxlevel . 2)))))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((org . t)
     (python . t)
     (verb . t)
     (emacs-lisp . t)
     (shell . t)
     (calc . t)
     (elixir . t)
     (rust . t)
     (git-permalink . t)))
  (defun my/org-long-lines-checker (checker callback)
    "Custom flycheck checker function for long lines in org-mode."
    (let ((errors '())
          (line-num 1)
          (threshold 1000))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line-length (- (line-end-position) (line-beginning-position))))
            (when (> line-length threshold)
              (push (flycheck-error-new-at
                     line-num 1 'warning
                     (format "Line too long (%d characters, max %d)" line-length threshold)
                     :checker checker
                     :filename (buffer-file-name))
                    errors)))
          (forward-line 1)
          (setq line-num (1+ line-num))))
      (funcall callback 'finished (reverse errors))))
  ;; Define the checker using the custom function
  (flycheck-define-generic-checker 'org-long-lines-custom
    "A custom checker for long lines in org-mode files."
    :start #'my/org-long-lines-checker
    :modes '(org-mode))
  ;; Add the checker to flycheck
  (add-to-list 'flycheck-checkers 'org-long-lines-custom)

  (defun my/warn-long-lines-org ()
    "Warn about long lines in org files with option to wrap them."
    (when (and (derived-mode-p 'org-mode)
               (save-excursion
                 (goto-char (point-min))
                 (re-search-forward "^.\\{801,\\}$" nil t)))
      (let ((choice (read-char-choice
                     "File contains very long lines. (w)rap lines, (s)ave anyway, or (c)ontinue? "
                     '(?w ?s ?c))))
        (cond
         ((eq choice ?w)
          (my/wrap-long-lines-org 800)
          (message "Long lines wrapped to 800 characters."))
         ((eq choice ?s)
          ;; Save anyway, do nothing
          nil)
         ((eq choice ?c)
          (signal 'quit nil))))))

  (defun my/wrap-long-lines-org (limit)
    "Wrap lines longer than LIMIT characters in org-mode buffers."
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (when (> (- line-end line-start) limit)
            (goto-char line-start)
            (while (and (< (current-column) limit)
                        (< (point) line-end))
              (forward-char))
            ;; Move back to find a good break point (space or punctuation)
            (while (and (> (current-column) 0)
                        (not (looking-back "\\s-\\|[.,;!?]" 1)))
              (backward-char))
            ;; If we couldn't find a good break point, use the limit
            (when (= (current-column) 0)
              (goto-char line-start)
              (forward-char limit))
            ;; Insert newline if we're not at the end of line
            (when (< (point) line-end)
              (insert "\n")
              ;; Update line-end since we inserted a newline
              (setq line-end (line-end-position)))))
        (forward-line 1))))
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-after-todo-state-change .
                               (lambda ()
                                 (when (string= org-state "DONE")
                                   (org-cycle))))
  (org-mode .
            (lambda ()
              (flycheck-mode 1)
              (flycheck-select-checker 'org-long-lines-custom)))

  (org-mode .
            (lambda ()
              (add-hook 'before-save-hook #'my/warn-long-lines-org nil t))))

(use-package ob-async
  :after org)

(use-package org-roam
  :defer t
  :after org
  :custom
  (org-roam-directory (file-truename org-roam-dir))
  (org-roam-dailies-directory org-roam-dailies-dir)
  (org-roam-completion-everywhere t)
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;; RETURN will follow links in org-mode files
  (setq org-return-follows-link  t)
  (transient-define-prefix org-structure-transient-menu ()
    ["Structure"
     [("i" "Insert" org-meta-return)
      ("t" "Toggle" org-toggle-heading)]]
    [("p" "Set property" org-set-property)])
  (transient-define-prefix org-transient-menu ()
    "Org command menu."
    ["Capture"
     [("c" "All" org-capture)
      ("C" "All (Roam)" org-roam-capture)]
     [("k" "Task" (lambda () (interactive) (org-capture nil "t")))]]
    ["Navigation"
     [("s" "Search" consult-org-roam-search)
      ("f" "Find" org-roam-node-find)]
     [("i" "Insert" org-roam-node-insert)
      ("g" "Graph" org-roam-graph)]]
    ["Dailies"
     [("t" "Goto today" org-roam-dailies-goto-today)
      ("y" "Goto yesterday" org-roam-dailies-goto-yesterday)]
     [("T" "Capture today" org-roam-dailies-capture-today)
      ("Y" "Capture yesterday" org-roam-dailies-capture-yesterday)]
     [("p" "Find previous" org-roam-dailies-find-previous-note)
      ("n" "Find next" org-roam-dailies-find-next-note)]]
    ["Agenda"
     [("d" "Day view" (lambda () (interactive) (org-agenda nil "d")))
      ("w" "Week view" (lambda () (interactive) (org-agenda nil "w")))]
     [("a" "Default" (lambda () (interactive) (org-agenda nil "a")))]]
    [("S" "Structure" org-structure-transient-menu)])
  ;; Force global keybinding to override macOS system binding for cmd-o
  ;; (global-set-key (kbd "s-o") 'org-transient-menu)
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
  :hook
  (after-init . org-roam-db-autosync-mode)
  (kill-emacs . (lambda ()
                  (when (fboundp 'org-roam-db-sync)
                    (message "Syncing org-roam database...")
                    (org-roam-db-sync)))))

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

;; Example:
;;
;; #+begin_src git-permalink :url https://github.com/emacs-mirror/emacs/blob/a4dcc8b9a94466c792be3743760a4a45cf6e1e61/lisp/emacs-lisp/ring.el#L48-L52
;; #+end_src
(use-package ob-git-permalink
  :defer t
  :commands org-babel-execute:git-permalink)

;; #+begin_src gptel
;; What is the capital of France?
;; #+end_src
;;
;; #+RESULTS:
;; : The capital of France is Paris.
(use-package ob-gptel
  :after org
  :vc
  (:url "https://github.com/jwiegley/ob-gptel")
  :commands org-babel-execute:gptel)

(use-package org-modern
  :after org
  :custom
  (org-modern-tag t)
  (org-modern-star 'replace)
  :config
  (global-org-modern-mode))

(use-package ox-pandoc
  :after org)

(use-package org-download
  :after (org org-roam))

;;; Clipboard

(add-to-list 'load-path "~/src/github.com/lorniu/pdd.el")
(require 'kopya)

;;; Modal

(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  ;; Performance optimizations
  (setq evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-want-fine-undo t)
  :config
  (evil-set-undo-system 'undo-fu)
  (global-unset-key (kbd "C-u"))
  (global-set-key (kbd "C-u") 'evil-scroll-up)
  ;; 1. Use =C-y= instead of your system paste shortcut
  ;;
  ;; 2. Add this to your config to enable clipboard integration with isearch:
  ;;    (define-key isearch-mode-map (kbd "s-v") 'isearch-yank-kill)
  ;;    Or for evil-specific:
  ;;    (define-key evil-ex-search-keymap (kbd "s-v") 'evil-ex-search-paste-from-register)
  ;;
  ;; 3. Consider =evil-search-module= options:
  ;;    (setq evil-search-module 'evil-search) ;; or 'isearch
  (define-key isearch-mode-map (kbd "s-v") 'isearch-yank-kill)
  ;; Get C-u M-x back
  (define-key evil-normal-state-map (kbd "C-c u") 'universal-argument)
  (evil-mode 1))

(use-package evil-org
  :after (org evil)
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-collection
  :after evil
  :defer 1
  :commands evil-collection-init
  :config
  (evil-collection-init))

;; Add surrounding:
;;
;; You can surround in visual-state with S<textobject> or gS<textobject>. Or in normal-state with ys<textobject> or yS<textobject>.
;;
;; Change surrounding:
;;
;; You can change a surrounding with cs<old-textobject><new-textobject>.
;;
;; Delete surrounding:
;;
;; You can delete a surrounding with ds<textobject>.
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

;;; UI

(use-package ansi-color
  :ensure nil
  :config
  ;; Process ANSI colour codes in compilation buffer
  (defun colourise-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))
  :hook
  (compilation-filter . colourise-compilation-buffer))

(add-hook 'emacs-startup-hook
          (lambda () (run-with-timer 0.1 nil #'which-key-mode)))

(add-hook 'emacs-startup-hook
          (lambda () (require 'slow)))

(defun my/set-frame-transparency (alpha-value)
  (if (and (>= alpha-value 0) (<= alpha-value 100))
      (progn
        (set-frame-parameter nil 'alpha alpha-value)
        (message "Frame transparency set to %d%%" alpha-value))
    (message "Invalid transparency value. Please enter a number between 0 and 100.")))

(my/set-frame-transparency 99)

;; https://www.reddit.com/r/emacs/comments/1mrqi6p/emacs_toggle_transparency_with_interactive/
(defun my/toggle-frame-transparency ()
  "Toggle frame transparency with user-specified opacity value.
Prompts user whether to enable transparency. If yes, asks for opacity value (0-100).
If no, restores full opacity. Only affects the active frame."
  (interactive)
  (if (y-or-n-p "Enable frame transparency? ")
      (let ((alpha-value (read-number "Enter transparency value (0-100, default 99): " 99)))
        (my/set-frame-transparency alpha-value))
    (progn
      (set-frame-parameter nil 'alpha 100)
      (message "Frame transparency disabled (full opacity restored)"))))

;; Indentation
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package aggressive-indent
  :disabled
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
  :after (nerd-icons)
  :hook (after-init . doom-modeline-mode))

;; Line numbers
(setq column-number-mode t) ;; Show the current line number
(setq display-line-numbers-type 'relative)
;; https://github.com/daviwil/dotfiles/blob/6a819647464e733446056caabc2f8ba40469178f/.emacs.d/modules/dw-core.el#L90C1-L93C61
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(global-so-long-mode 1)

;; Buffers
(defun my/clean-up-buffers ()
  "Clean-up buffers that have built up."
  (interactive)
  (kill-matching-buffers ".*\.ex" nil t)
  (kill-matching-buffers ".*\.exs" nil t)
  (kill-matching-buffers ".*\.yaml" nil t))

(use-package posframe)

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
        sideline-backends-right '(sideline-flycheck
                                  sideline-eglot
                                  sideline-eros
                                  sideline-blame)
        sideline-display-backend-name t
        sideline-delay 0.3 ;; Seconds
        sideline-backend-delays '((sideline-blame . 2.0))
        sideline-display-backend-name t
        sideline-display-backend-type 'right
        sideline-truncate t))

(use-package sideline-eglot
  :vc
  (:url "https://github.com/emacs-sideline/sideline-eglot")
  :config
  ;; TODO: Remove this once fixed upstream
  (defun my/fix-sideline-eglot-getf (orig-fun &rest args)
    "Use cl-getf instead of getf in sideline-eglot."
    (cl-letf (((symbol-function 'getf) #'cl-getf))
      (apply orig-fun args)))
  (advice-add 'sideline-eglot--async-candidates :around #'my/fix-sideline-eglot-getf))

(use-package sideline-blame
  :after sideline)

(use-package sideline-flycheck
  :after (sideline flycheck)
  :hook
  (flycheck-mode . sideline-flycheck-setup))

;; Emacs headerline indication of where you are in a large project
(use-package breadcrumb
  :hook (after-init . breadcrumb-mode))

;; Show info about the block at the end of the block
(use-package scopeline
  :after treesit
  :diminish scopeline-mode
  :custom
  (scopeline-overlay-prefix " ↰ ")
  :hook
  (prog-mode . (lambda ()
                 (when (and (fboundp 'treesit-available-p)
                            (treesit-available-p)
                            (treesit-parser-list))
                   (scopeline-mode)))))

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

(defun my/theme-by-current-theme ()
  "Get the light or dark theme based on the current theme."
  (let ((appearance (get-macos-appearance-applescript)))
    (cond
     ((eq appearance 'dark) 'mocha)
     ((eq appearance 'light) 'latte))))

(defun my/load-theme-by-current-theme ()
  "Load the right theme based on the current system theme."
  (progn
    (setq catppuccin-flavor (my/theme-by-current-theme))
    (load-theme 'catppuccin t)))

(defun get-macos-appearance ()
  "Get macOS appearance using AppleScript, returns 'dark or 'light."
  (let ((result (ns-do-applescript
                 "tell application \"System Events\" to tell appearance preferences to get dark mode")))
    (if (or (equal result "true"))
        'light
      'dark)))

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
        ("s-c" . casual-bookmarks-tmenu))
  (:map org-agenda-mode-map
        ("s-c" . casual-agenda-tmenu)))

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

(use-package vertico-posframe
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 4)
     (right-fringe . 4)
     (internal-border-width . 4)))
  (vertico-multiform-commands
   '((treemacs (:not posframe))
     (consult-line (:not posframe))
     (consult-ripgrep (:not posframe))
     (t posframe)))
  :config
  (vertico-posframe-mode 1))

;;; Whitespace
(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default evil-shift-width 2)

(use-package dtrt-indent
  :disabled
  :diminish
  :config
  (dtrt-indent-global-mode t))

(setq require-final-newline t) ;; Add new line in the end of a file on save.
(setq-default show-trailing-whitespace nil) ;; By default, don't underline trailing spaces

;; Delete trailing spaces
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)
;; Prefer ws-butler as it's aware about unchanged vs changed trailing
;; whitespace.
(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  (text-mode . ws-butler-mode))

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

;; https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter/
(define-advice keyboard-quit
    (:around (quit) quit-current-context)
  "Quit the current context.

When there is an active minibuffer and we are not inside it close
it.  When we are inside the minibuffer use the regular
`minibuffer-keyboard-quit' which quits any active region before
exiting.  When there is no minibuffer `keyboard-quit' unless we
are defining or executing a macro."
  (if (active-minibuffer-window)
      (if (minibufferp)
          (minibuffer-keyboard-quit)
        (abort-recursive-edit))
    (unless (or defining-kbd-macro
                executing-kbd-macro)
      (funcall-interactively quit))))

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
  ("C-;" . embark-dwim)
  :config

  (defun my/embark-pytest-def-p ()
    "Return non-nil if point is inside a Python test function in a test_*.py file and python-ts-mode is active."
    (and buffer-file-name
         (string-match-p "/test_.*\\.py\\'" buffer-file-name)
         (eq major-mode 'python-ts-mode)
         (require 'treesit nil t)
         (treesit-ready-p 'python)
         (let* ((node (treesit-node-at (point)))
                (func-node (when node
                             (treesit-parent-until
                              node
                              (lambda (n)
                                (string= (treesit-node-type n) "function_definition"))))))
           (when func-node
             (let* ((name-node (treesit-node-child-by-field-name func-node "name"))
                    (func-name (and name-node (treesit-node-text name-node))))
               (and func-name (string-prefix-p "test_" func-name)))))))

  (defun my/embark-python-pytest-run-def-at-point ()
    "Run pytest on the test function at point using treesit."
    (interactive)
    (if (my/embark-pytest-def-p)
        (call-interactively #'python-pytest-run-def-at-point-treesit)
      (user-error "Not on a Python test function in a test_*.py file")))

  (defun my/embark-rust-test-def-p ()
    "Return non-nil if point is inside a Rust test function and rust-ts-mode is active."
    (and buffer-file-name
         (string-match-p "\\.rs\\'" buffer-file-name)
         (eq major-mode 'rust-ts-mode)
         (require 'treesit nil t)
         (treesit-ready-p 'rust)
         (let* ((node (treesit-node-at (point)))
                (func-node (when node
                             (treesit-parent-until
                              node
                              (lambda (n) (equal (treesit-node-type n) "function_item"))))))
           (when func-node
             ;; Check if function has #[test] or #[cfg(test)] attribute
             (let ((func-start (treesit-node-start func-node))
                   (func-text (treesit-node-text func-node)))
               (save-excursion
                 (goto-char func-start)
                 (forward-line -10) ; Look backwards for attributes
                 (re-search-forward "#\\[\\(test\\|cfg(test)\\)\\]" func-start t)))))))

  (defun my/embark-rust-cargo-test-def-at-point ()
    "Run cargo test on the test function at point using treesit."
    (interactive)
    (if (my/embark-rust-test-def-p)
        (let* ((node (treesit-node-at (point)))
               (func-node (treesit-parent-until
                           node
                           (lambda (n) (equal (treesit-node-type n) "function_item"))))
               (name-node (when func-node
                            (treesit-node-child-by-field-name func-node "name")))
               (func-name (when name-node (treesit-node-text name-node))))
          (if func-name
              ;; Run cargo test directly with the function name
              (let ((default-directory (or (locate-dominating-file default-directory "Cargo.toml") default-directory)))
                (compile (format "cargo test %s" (shell-quote-argument func-name))))
            (user-error "Could not determine test function name")))
      (user-error "Not on a Rust test function")))

  (defun my/embark-test-def-at-point ()
    "Run appropriate test command based on major mode."
    (interactive)
    (cond
     ((eq major-mode 'python-ts-mode)
      (my/embark-python-pytest-run-def-at-point))
     ((eq major-mode 'rust-ts-mode)
      (my/embark-rust-cargo-test-def-at-point))
     (t
      (user-error "No test runner configured for %s" major-mode))))

  (define-key embark-identifier-map (kbd "T") #'my/embark-test-def-at-point))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Scrolling
(pixel-scroll-precision-mode) ;; Smooth scrolling
(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
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
  ;; Don't enable globally, for performance
  ;; (global-ligature-mode t)
  :hook
  (prog-mode . ligature-mode))

;; Default frame size
(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 120))

;; Initial frame content
(setq inhibit-splash-screen t) ; No startup screen
(setq inhibit-startup-message t) ; No startup message
(setq initial-scratch-message nil) ; Empty scratch buffer

;;; File system

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

(use-package dirvish
  :after dired)
;; Disable peek as it's slow
;; :config
;; (dirvish-peek-mode))

;;; Completion

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
         ("s-b" . consult-buffer)                ;; orig. switch-to-buffer
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
         ("M-s d" . consult-fd)                  ;; Alternative: consult-find
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
  (:url "https://github.com/brett-lempereur/consult-xref-stack")
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

(use-package consult-eglot-embark
  :after (consult eglot consult-eglot)
  :config
  (defun my/toggle-consult-eglot-embark-mode ()
    "Toggle consult-eglot-embark-mode based on buffer Eglot status."
    (cond
     ((eglot-managed-p)
      (consult-eglot-embark-mode 1))  ;; Enable when buffer is managed by Eglot
     (t
      (consult-eglot-embark-mode -1)))) ;; Disable when buffer is not managed by Eglot
  :hook
  (eglot-managed-mode . my/toggle-consult-eglot-embark-mode))

(use-package consult-todo
  :after (consult hl-todo))

(use-package consult-gh
  :after (consult forge ghub magit pr-review)
  :defer t
  :commands
  (consult-gh-auth-switch
   consult-gh-repo-list
   consult-gh-issue-list
   consult-gh-pr-list
   consult-gh-search-repos
   consult-gh-search-issues
   consult-gh-search-prs
   consult-gh-search-code
   consult-gh-find-file
   consult-gh-repo-create
   consult-gh-issue-create
   consult-gh-pr-create
   consult-gh-dashboard
   consult-gh-notifications
   consult-gh-workflow-list
   consult-gh-workflow-run
   consult-gh-run-list
   consult-gh-run-rerun))
;; :config
;; Disabled, as currently having issues
;; (consult-gh-with-pr-review-mode +1))

(use-package consult-gh-embark
  :after consult-gh
  :config
  (consult-gh-embark-mode +1))

(use-package consult-gh-forge
  :disabled
  :after (consult-gh forge ghub magit)
  :config
  (consult-gh-forge-mode +1))

(use-package consult-gh-transient
  :ensure nil
  :after consult-gh)

(use-package consult-ghq
  :after (consult affe))

(use-package consult-project-extra
  ;; Suggested bindings
  ;; :bind
  ;; (("C-c p f" . consult-project-extra-find)
  ;;  ("C-c p o" . consult-project-extra-find-other-window)))
  :after (consult flycheck))

;; CAPF

(use-package dabbrev
  :ensure nil
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Available since Emacs 29 (Use `dabbrev-ignored-buffer-regexps' on older Emacs)
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package corfu
  :after (dabbrev)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay 0.1)
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current t)
  (corfu-preselect 'prompt)      ;; Pres-elect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-separator ?\s)          ;; Orderless field separator
  (global-corfu-minibuffer t)    ;; Enable Corfu in the minibuffer
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("S-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)                ;; In case "TAB" doesn't work
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)            ;; In case "S-TAB" does't work
              ("RET"        . corfu-insert))
  :hook
  (corfu-mode-hook . corfu-popupinfo-mode)
  (eshell-mode . (lambda ()
                   (setq-local corfu-auto nil)
                   (corfu-mode))))

(use-package completion-preview
  :ensure nil
  :hook
  ((comint-mode-hook
    eshell-mode-hook
    prog-mode-hook
    text-mode-hook) . completion-preview-mode)
  :init
  (setq completion-preview-minimum-symbol-length 2)
  :config
  (cl-pushnew 'org-self-insert-command completion-preview-commands :test #'equal))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :config
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; Complete word from current buffers. See also dabbrev-capf on Emacs 29.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; Complete file name.
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; Complete Elisp symbol.
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-block nil t)))
  ;; Complete Emoji. Available on Emacs 29 and newer.
  ;; (add-hook 'completion-at-point-functions #'cape-emoji)
  ;; Complete Elisp in Org or Markdown code block.
  ;; Add cape-elisp-block only to org-mode and markdown-mode
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-block nil t)))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-block nil t)))
  ;; Consider:
  ;; https://github.com/minad/corfu/wiki#continuously-update-the-candidates
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ElDoc

;; This package displays ElDoc documentations in a childframe. The
;; childframe is selectable and scrollable with mouse, even though the
;; cursor is hidden.
(use-package eldoc-box
  :after (eldoc eglot)
  :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode))

;;; Editor

(global-set-key (kbd "s-r") (lambda () (interactive) (revert-buffer t t)))

;; Navigation
(after-packages (evil avy xref consult-todo treesit-fold)
  (transient-define-prefix nav-transient-menu ()
    "Navigation commands menu."
    [
     ["Changes"
      ("c" "Last change" evil-goto-last-change)
      ("p" "Previous hunk" diff-hl-previous-hunk)
      ("n" "Next hunk" diff-hl-next-hunk)]
     ["Things"
      ("g" "Imenu" consult-imenu)
      ("m" "Marks" consult-mark)
      ("t" "TODOs" consult-todo)]
     ["Position"
      ("b" "Jump backward" evil-jump-backward)
      ("f" "Jump forward" evil-jump-forward)
      ("w" "Word" avy-goto-word-0)]]
    [["Xref"
      ("a" "Back" xref-go-back)
      ("o" "Forward" xref-go-forward)
      ("A" "Back / Stack" consult-xref-stack-backward)
      ("O" "Forward / Stack" consult-xref-stack-forward)
      ("d" "Definitions" xref-find-definitions)]
     ["Structure"
      ("," "Function beginning" beginning-of-defun)
      ("." "Function ending" end-of-defun)]
     ["Folds"
      ("O" "Open" treesit-fold-open)
      ("C" "Close" treesit-fold-close)]])
  (global-set-key (kbd "s-u") 'nav-transient-menu))

(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package wgrep
  :defer t)

(use-package rg
  :after wgrep
  :config
  ;; Optionally customise this with:
  ;; rg-keymap-prefix
  (rg-enable-default-bindings)
  (rg-enable-menu)
  :hook
  (rg-mode . wgrep-rg-setup))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :commands treemacs
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
  :defer t
  :after (treemacs magit))

;; Folding
(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :custom
  (treesit-fold-indicators-fringe 'left-fringe)
  (treesit-fold-indicators-priority 30) ; Lower priority for less frequent updates
  :config
  ;; Add debouncing for better performance
  (setq treesit-fold-indicators-delay 0.5) ; 500ms delay before updating
  ;; Limit to files under a certain size to avoid performance issues
  (defun my/treesit-fold-indicators-maybe ()
    "Enable treesit-fold-indicators only for reasonably sized files."
    (when (and (treesit-parser-list)
               (< (buffer-size) 50000)) ; Only files under 50KB
      (treesit-fold-indicators-mode 1)))
  (add-hook 'prog-mode-hook #'my/treesit-fold-indicators-maybe))

;; Movement
(use-package avy
  :bind
  (("C-'" . avy-goto-word-0)))

(use-package treesit-jump
  :vc (:url "https://www.github.com/dmille56/treesit-jump")
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
(use-package undo-fu)

(use-package vundo
  :vc (:url "https://github.com/casouri/vundo")
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t))

;; Formatting
(use-package apheleia
  :hook
  (prog-mode . apheleia-mode)
  :config
  (add-to-list 'apheleia-mode-alist '(json-ts-mode . denofmt-ts))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . denofmt-ts))
  (add-to-list 'apheleia-mode-alist '(python-base-mode . ruff))
  (add-to-list 'apheleia-formatters '(fish-indent . ("fish_indent")))
  (add-to-list 'apheleia-mode-alist '(fish-mode . fish-indent))
  (add-to-list 'apheleia-mode-alist '(ruby-ts-mode . rufo)))

;; Spelling & Grammar

;; By default, Emacs thinks a sentence is a full-stop followed by 2 spaces. Let’s make it full-stop and 1 space.
;; http://sriramkswamy.github.io/dotemacs/
(setq sentence-end-double-space nil)

(use-package string-inflection
  :defer t)

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
  (:url "https://github.com/mickeynp/combobulate")
  :custom
  (combobulate-key-prefix "C-c o")
  :hook
  (go-mode . combobulate-mode)
  (go-ts-mode . combobulate-mode)
  (json-mode . combobulate-mode)
  (json-ts-mode . combobulate-mode)
  (python-base-mode . combobulate-mode)
  (toml-mode . combobulate-mode)
  (toml-ts-mode . combobulate-mode)
  (yaml-mode . combobulate-mode)
  (yaml-ts-mode . combobulate-mode))

;; Search / Find / Replace
(setq isearch-lazy-count t)

(use-package semext
  :vc (:url "https://github.com/ahyatt/semext")
  :init
  (require 'llm-openai)
  (setopt semext-provider (make-llm-openai :key (my/get-password "api.openai.com" "me")))
  :commands
  (semext-forward-part
   semext-backward-part
   semext-query-replace
   semext-search-forward
   semext-search-backward
   semext-clear-cache))

(use-package ast-grep
  :vc
  (:url "https://github.com/SunskyXH/ast-grep.el"))

(use-package re-builder
  :ensure nil
  :custom
  ;; Recommended. Similar to read but you don’t have the issue of
  ;; backslash plague that haunts the default settings. Example:
  ;; \\(foo\\\|bar\\)
  (reb-re-syntax 'string))

;; Editor Config - only check at project.el root
(use-package editorconfig
  :diminish editorconfig-mode
  :ensure nil
  :custom
  (editorconfig-get-properties-function 'editorconfig-core-get-properties-hash)
  :config
  (defun my/enable-editorconfig-maybe ()
    "Enable editorconfig only when .editorconfig exists at project root."
    ;; Always reset first to clear any previous project's config
    (editorconfig-mode -1)
    (when-let* ((project (project-current))
                (root (project-root project))
                (editorconfig-file (expand-file-name ".editorconfig" root)))
      (when (file-exists-p editorconfig-file)
        (editorconfig-apply))))
  :config
  (defun my/disable-editorconfig-on-project-switch (&rest _)
    "Disable editorconfig when switching projects."
    (editorconfig-mode -1))
  (advice-add 'project-switch-project :before #'my/disable-editorconfig-on-project-switch)
  :hook (find-file . my/enable-editorconfig-maybe))

;;; Email

(use-package himalaya
  :if (file-exists-p "~/src/github.com/jesse-c/himalaya-emacs")
  :load-path "~/src/github.com/jesse-c/himalaya-emacs"
  :after evil
  :custom
  (himalaya-evil-enable-integration t)
  (himalaya-html-browse-function #'eww-open-file))

(use-package himalaya-menu
  :after himalaya
  :if (file-exists-p "~/src/github.com/jesse-c/himalaya-menu-emacs")
  :load-path "~/src/github.com/jesse-c/himalaya-menu-emacs"
  :config
  (himalaya-menu-setup))

;;; Language: All

(setq compilation-max-output-line-length 800)

(use-package flycheck
  :config
  (setq flycheck-indication-mode 'left-fringe)
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

(setq user-emacs-cache-directory (expand-file-name ".cache" user-emacs-directory))

(use-package dape
  :custom
  (dape-inlay-hints t)
  (dape-cwd-function 'my/project-root)
  (dape-buffer-window-arrangement 'right)
  (dape-breakpoint-margin-string "●")
  (dape-default-breakpoints-file (expand-file-name "dap-breakpoints" user-emacs-cache-directory))
  :commands (dape)
  :config
  ;; The default read-process-output-max of 4096 bytes may inhibit performance to some degree, also.
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;; To not display info and/or buffers on startup
  (remove-hook 'dape-on-start-hooks 'dape-info)
  (remove-hook 'dape-on-start-hooks 'dape-repl)
  ;; To display info and/or repl buffers on stopped
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl)
  (transient-define-prefix dape-transient-menu ()
    "Transient for dape."
    [["Stepping"
      ("n"  "Next" dape-next :transient t)
      ("s"  "Step in" dape-step-in :transient t)
      ("o"  "Step out" dape-step-out :transient t)
      ("c"  "Continue" dape-continue :transient t)
      ("r"  "Restart" dape-restart :transient t)]
     ["Breakpoints"
      ("bb" "Toggle" dape-breakpoint-toggle :transient nil)
      ("be" "Expression" dape-breakpoint-expression :transient t)
      ("bd" "Remove at pt" dape-breakpoint-remove-at-point :transient t)
      ("bD" "Remove all" dape-breakpoint-remove-all :transient t)
      ("bl" "Log" dape-breakpoint-log :transient t)]
     ["Info"
      ("ii" "Info" dape-info :transient nil)
      ("im" "Memory" dape-memory :transient t)
      ("is" "Select Stack" dape-select-stack :transient t)
      ("R"  "Repl" dape-repl :transient nil)]
     ["Quit"
      ("qq" "Quit" dape-quit :transient nil)
      ("qk" "Kill" dape-kill :transient nil)]])
  (add-to-list 'dape-configs
               `(py modes (python-mode python-ts-mode)
                    ensure (lambda (config) (dape-ensure-command config)
                             (let ((python (dape-config-get config 'command)))
                               (unless
                                   (zerop
                                    (call-process-shell-command
                                     (format "%s -c \"import debugpy.adapter\"" python)))
                                 (user-error "%s module debugpy is not installed"
                                             python))))
                    command dap-python-executable
                    command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
                    port :autoport
                    :request "launch"
                    :type "python"
                    :mode "debug"
                    :cwd dape-cwd
                    :program dape-buffer-default
                    :args []
                    :justMyCode nil
                    :console "integratedTerminal"
                    :showReturnValue t
                    :stopOnEntry nil))
  (add-to-list 'dape-configs
               `(pytest modes (python-mode python-ts-mode)
                        ensure (lambda (config) (dape-ensure-command config)
                                 (let ((python (dape-config-get config 'command)))
                                   (unless
                                       (zerop
                                        (call-process-shell-command
                                         (format "%s -c \"import debugpy.adapter\"" python)))
                                     (user-error "%s module debugpy is not installed"
                                                 python))))
                        command dap-python-executable
                        command-args ("-m" "debugpy.adapter" "--host" "0.0.0.0" "--port" :autoport)
                        port :autoport
                        :request "launch"
                        :type "python"
                        :mode "test"
                        :cwd dape-cwd
                        :module "pytest"
                        :args [dape-buffer-default]
                        :justMyCode nil
                        :console "integratedTerminal"
                        :showReturnValue t
                        :stopOnEntry nil))
  :hook
  (dape-start . (lambda () (save-some-buffers t t)))
  (dape-compile . kill-buffer)
  ;; Pulse source line (performance hit)
  (dape-display-source . pulse-momentary-highlight-one-line)
  :bind
  (("s-d" . dape-transient-menu)))

(use-package repeat
  :ensure nil
  :config
  (repeat-mode))

;;; Language: Documents

(use-package pandoc-mode
  :after hydra
  :hook
  (markdown-mode . pandoc-mode)
  (org-mode . pandoc-mode)
  (rst-mode . pandoc-mode)
  (latex-mode . pandoc-mode)
  (pandoc-mode . pandoc-load-default-settings))

;;; Language: Lisps

(use-package parinfer-rust-mode
  :diminish
  :hook emacs-lisp-mode)

;;; Language: Elixir

;; Manually do for now
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))

(add-to-list 'treesit-language-source-alist '(elixir "https://github.com/elixir-lang/tree-sitter-elixir"))
(add-to-list 'treesit-language-source-alist '(heex "https://github.com/phoenixframework/tree-sitter-heex"))

;; Elixir: lib/foo.ex <-> test/foo_test.exs
(add-to-list 'find-sibling-rules
             '("\\`\\(.*\\)/lib/\\(.*\\)\\.ex\\'"
               "\\1/test/\\2_test.exs"))
(add-to-list 'find-sibling-rules
             '("\\`\\(.*\\)/test/\\(.*\\)_test\\.exs\\'"
               "\\1/lib/\\2.ex"))

(use-package mix
  :hook
  (elixir-mode . mix-minor-mode)
  (elixir-ts-mode . mix-minor-mode))

(use-package flycheck-credo
  :after (flycheck)
  :config
  (flycheck-credo-setup)
  :hook
  (elixir-mode . flycheck-mode))

(use-package ob-elixir
  :defer t
  :commands org-babel-execute:elixir)

;;; Language: Erlang

(use-package erlang-ts
  :mode ("\\.erl\\'" . erlang-ts-mode)
  :defer t)

;;; Language: Python

;; Python: foo.py <-> test_foo.py
(add-to-list 'find-sibling-rules
             '("\\`\\(.*\\)/\\([^/]*\\)\\.py\\'"
               "\\1/test_\\2.py"))
(add-to-list 'find-sibling-rules
             '("\\`\\(.*\\)/test_\\([^/]*\\)\\.py\\'"
               "\\1/\\2.py"))

(setq-local python-indent-offset 4)

(use-package poetry)

(use-package python-pytest
  :after pet
  :bind
  (:map python-base-mode-map
        ("s-t" . python-pytest-dispatch)))

(use-package pet
  :custom
  (pet-debug 1)
  :config
  (defun my/pet-python-setup ()
    (pet-mode 1)
    (setq-local python-shell-interpreter (pet-executable-find "python")
                python-shell-virtualenv-root (pet-virtualenv-root)
                python-pytest-executable (pet-executable-find "pytest")
                dap-python-executable python-shell-interpreter
                aphelia-formatter (pet-executable-find "ruff"))


    (pet-eglot-setup)
    (pet-flycheck-setup))
  :hook
  (python-base-mode . my/pet-python-setup))

;;; Language: Fish

(use-package fish-mode)

;;; Language: Clojure(Script)

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

;;; Language: Flix

;; Define flix mode
(define-derived-mode flix-mode prog-mode "Flix"
  "Major mode for editing Flix source code."
  :group 'flix
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local compile-command "flix build")
  (setq-local run-command "flix run"))

;; Associate .flix files with flix-mode
(add-to-list 'auto-mode-alist '("\\.flix\\'" . flix-mode))

;;; Language: Rust

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package ob-rust
  :defer t
  :commands org-babel-execute:rust)

(use-package flycheck-rust
  :defer 1
  :after (rust-mode flycheck)
  :hook
  (flycheck-mode . flycheck-rust-setup))

(use-package cargo-mode
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t)
  (defun my/cargo-transient-with-test ()
    "Call cargo-transient and immediately select test."
    (interactive)
    (cargo-transient)
    (call-interactively (key-binding "t")))
  (defun my/cargo-transient-with-run ()
    "Call cargo-transient and immediately select run."
    (interactive)
    (cargo-transient)
    (call-interactively (key-binding "r")))
  :bind
  (:map rust-ts-mode-map
        ("s-t" . my/cargo-transient-with-test)
        ("s-r" . my/cargo-transient-with-run)))

(use-package cargo-transient
  :custom
  (cargo-transient-buffer-name-function #'project-prefixed-buffer-name))

;;; Language: Swift

(use-package swift-mode)

(use-package swift-ts-mode)

(use-package flycheck-swift
  :defer 1
  :after (swift-mode flycheck)
  :hook
  (flycheck-mode . flycheck-swift-setup))

;;; Language: JSON

(use-package json-mode)

;; https://eshelyaron.com/posts/2023-05-17-orientation-in-json-documents-with-emacs-and-tree-sitter.html
(defun esy/json-path-to-position (pos)
  "Return the JSON path from the document's root to the element at POS.

The path is represented as a list of strings and integers,
corresponding to the object keys and array indices that lead from
the root to the element at POS."
  (named-let loop ((node (treesit-node-at pos)) (acc nil))
    (if-let* ((parent (treesit-parent-until
                       node
                       (lambda (n)
                         (member (treesit-node-type n)
                                 '("pair" "array"))))))
        (loop parent
              (cons
               (pcase (treesit-node-type parent)
                 ("pair"
                  (treesit-node-text
                   (treesit-node-child (treesit-node-child parent 0) 1) t))
                 ("array"
                  (named-let check ((i 1))
                    (if (< pos (treesit-node-end (treesit-node-child parent i)))
                        (/ (1- i) 2)
                      (check (+ i 2))))))
               acc))
      acc)))

(defun esy/json-path-at-point (point &optional kill)
  "Display the JSON path at POINT.  When KILL is non-nil, kill it too.

Interactively, POINT is point and KILL is the prefix argument."
  (interactive "d\nP" json-ts-mode)
  (let ((path (mapconcat (lambda (o) (format "%s" o))
                         (esy/json-path-to-position point)
                         ".")))
    (if kill
        (progn (kill-new path) (message "Copied: %s" path))
      (message path))
    path))

;;; Language: Docker

(use-package docker
  :after transient)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

;;; Language: YAML

(use-package yaml-mode)

;;; Language: CSV

(use-package csv-mode)

;;; Language: Just

(use-package just-ts-mode)

(defun just-transient--find-justfile ()
  "Find the Justfile in the project root or VC root."
  (let* ((project-root (or (and (fboundp 'projectile-project-root)
                                (projectile-project-root))
                           (locate-dominating-file default-directory ".git")))
         (justfile (and project-root (expand-file-name "justfile" project-root))))
    (when (and justfile (file-exists-p justfile))
      justfile)))

(defun just-transient--parse-recipes (justfile)
  "Parse the Justfile and return a list of (recipe . docstring)."
  (with-temp-buffer
    (insert-file-contents justfile)
    (let ((recipes '()))
      (goto-char (point-min))
      (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\):" nil t)
        (let ((recipe (match-string 1))
              (docstring (save-excursion
                           (if (re-search-backward "^# ?\\(.*\\)$" (line-beginning-position 0) t)
                               (match-string 1)
                             ""))))
          (push (cons recipe docstring) recipes)))
      (nreverse recipes))))


(defun just-transient--run-recipe-compilation (recipe)
  "Run just RECIPE in the project root with proper environment handling."
  (let* ((default-directory (file-name-directory (just-transient--find-justfile)))
         (buffer-name (format "*just:%s*" recipe))
         (shell (or (getenv "SHELL") "/bin/sh"))
         (cmd (format "%s -c 'cd %s && just %s'"
                      shell
                      (shell-quote-argument default-directory)
                      recipe)))
    ;; Set custom compilation buffer name
    (let ((compilation-buffer-name-function
           (lambda (_mode) buffer-name)))
      ;; Start compilation with shell execution
      (compile cmd))))

(defun just-transient--run-recipe-interactive-shell (recipe)
  "Run just RECIPE in the project root using a proper shell."
  (let ((default-directory (file-name-directory (just-transient--find-justfile)))
        (cmd (format "just %s" recipe))
        (buffer-name (format "*just:%s*" recipe)))

    (cond
     ;; If vterm is available, use it
     ((fboundp 'vterm)
      (let ((vterm-buffer (generate-new-buffer buffer-name)))
        (with-current-buffer vterm-buffer
          (vterm-mode)
          (vterm-send-string (format "cd %s && %s\n"
                                     (shell-quote-argument default-directory)
                                     cmd)))
        (display-buffer vterm-buffer)))

     ;; If shell-pop is available
     ((fboundp 'shell-pop)
      (let ((shell-pop-shell-type '("shell" "*shell*" (lambda () (shell))))
            (shell-pop-term-shell (getenv "SHELL")))
        (shell-pop--shell-buffer-name buffer-name)
        (shell-pop 1)
        (comint-send-string nil (concat cmd "\n"))))

     ;; Otherwise use a normal shell buffer
     (t
      (let ((shell-buffer (get-buffer-create buffer-name)))
        (with-current-buffer shell-buffer
          (shell)
          (goto-char (point-max))
          (comint-send-string nil (concat "cd " (shell-quote-argument default-directory) "\n"))
          (comint-send-string nil (concat cmd "\n")))
        (display-buffer shell-buffer))))))


(defun just-transient--make-transient ()
  "Create and invoke the transient menu for Just recipes."
  (interactive)
  (let* ((justfile (just-transient--find-justfile))
         (recipes (and justfile (just-transient--parse-recipes justfile)))
         (used-prefixes (make-hash-table :test 'equal))
         (menu-items '()))

    (if (not justfile)
        (message "No Justfile found in project root or VC root.")

      ;; Build menu items by looping over all recipes
      (dolist (recipe recipes)
        (let* ((recipe-name (car recipe))
               (doc (cdr recipe))
               ;; Use first character as prefix, fallback to next available if taken
               (prefix (substring recipe-name 0 1))
               (counter 0)
               (description (if (string-empty-p doc)
                                recipe-name
                              (format "%s: %s" recipe-name doc))))

          ;; Find an available prefix if the first character is taken
          (while (and (gethash prefix used-prefixes) (< counter 26))
            (setq counter (1+ counter))
            (if (< counter (length recipe-name))
                (setq prefix (substring recipe-name counter (1+ counter)))
              (setq prefix (char-to-string (+ ?a counter)))))

          ;; Mark prefix as used
          (puthash prefix t used-prefixes)

          ;; Add the menu item
          (push `(,prefix ,description
                          (lambda () (interactive) (just-transient--run-recipe-compilation ,recipe-name)))
                menu-items)))

      ;; Define and invoke the transient menu
      (eval
       `(transient-define-prefix just-transient-menu ()
          "Run a Justfile recipe."
          ["Recipes"
           ,@(nreverse menu-items)]))

      (just-transient-menu))))

;;; Language: Markdown

(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer t
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

;;; Language: ePub

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;;; Language: Vespa

(define-derived-mode vespa-schema-mode prog-mode "Vespa Schema"
  "Major mode for editing Vespa schema definition files."
  :group 'vespa
  (setq-local comment-start "#")
  (setq-local comment-end ""))

(add-to-list 'auto-mode-alist '("\\.sd\\'" . vespa-schema-mode))

;;; Language: HTTP

(use-package verb
  :defer t
  :commands org-babel-execute:verb)

;;; Language: Shell

(use-package modern-sh
  :hook
  (sh-mode . modern-sh-mode)
  (bash-mode . modern-sh-mode))

;;; Language: Typst

(use-package typst-ts-mode
  :vc
  (:url "https://codeberg.org/meow_king/typst-ts-mode")
  :custom
  (typst-ts-watch-options "--open")
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t)
  :config
  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu))

(use-package websocket
  :defer t)

(use-package typst-preview
  :after websocket
  :vc
  (:url "https://github.com/havarddj/typst-preview.el")
  :custom
  (typst-preview-executable "tinymist preview")
  (typst-preview-browser "default"))

;;; Language: Emacs Lisp

(setq-local lisp-indent-offset 2)

(use-package flycheck-package
  :after (flycheck)
  :hook
  (emacs-lisp-mode . flycheck-package-setup))

(use-package elsa
  :defer t)

(use-package flycheck-elsa
  :after (flycheck elsa)
  :hook
  (emacs-lisp-mode . flycheck-elsa-setup))

(use-package eros
  :config
  (eros-mode 1))

(use-package sideline-eros
  :vc
  (:url "https://github.com/emacs-sideline/sideline-eros")
  :after (sideline)
  :hook (emacs-lisp-mode . sideline-eros-setup))

(use-package inspector
  :defer t
  :custom
  (inspector-switch-to-buffer nil))

(use-package tree-inspector
  :after inspector
  :defer t)

(use-package eros-inspector
  :after (inspector eros)
  :defer t)

;; Provide helpful Elisp API examples
(use-package elisp-demos
  :after helpful
  :defer t
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; A few things from isamert:
;;
;; https://isamert.net/2023/08/14/elisp-editing-development-tips.html

(defun im/eval-dwim (lastf regionf defunf)
  "Generate an interactive function that you can bind to a key for evaluating.
The returned function will call LASTF, REGIONF or DEFUNF
depending on the context when called.  If you have an active
region, REGIONF will be called, if you are in middle of an
expression DEFUNF will be called.  If your cursor is near a
closing parenthesis, LASTF will be called."
  (lambda ()
    (interactive)
    (cond
     ((use-region-p)
      (call-interactively regionf))
     ((or (-contains? '(?\) ?\") (char-before))
          (-contains? '(?\ ?\)) (char-after)))
      (call-interactively lastf))
     (t
      (call-interactively defunf)))))

(after-packages (eros)
  (bind-key "C-'" (im/eval-dwim #'eros-eval-last-sexp #'eval-region #'eros-eval-defun)))

(defmacro im/tap (form)
  "Evaluate FORM and return its result.
Additionally, print a message to the *Messages* buffer showing
the form and its result.

This macro is useful for debugging and inspecting the
intermediate results of Elisp code without changing your code
structure. Just wrap the form with `im/tap' that you want to see
it's output without introducing an intermediate let-form."
  `(let ((result ,form))
     (message "[im/tap :: %s] → %s" ,(prin1-to-string form) result)
     result))

(defun im/debug (thing)
  "Like `im/tap' but uses `pp-display-expression' to display the
result instead of `message'."
  (pp-display-expression thing "*im/debug*")
  thing)

;;; Language: Go

;; Go: foo.go <-> foo_test.go
(add-to-list 'find-sibling-rules
             '("\\`\\(.*\\)/\\([^/]*\\)\\.go\\'"
               "\\1/\\2_test.go"))
(add-to-list 'find-sibling-rules
             '("\\`\\(.*\\)/\\([^/]*\\)_test\\.go\\'"
               "\\1/\\2.go"))

(use-package go-ts-mode
  :defer t)

;;; Language: PDF

(use-package pdf-tools
  :defer t)

;;; Language: XML

;; https://www.emacswiki.org/emacs/NxmlMode#h5o-12
(with-eval-after-load 'nxml-mode
  (defun my/nxml-where ()
    "Display the hierarchy of XML elements the point is on as a path."
    (interactive)
    (let ((path nil))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (< (point-min) (point) ;; Doesn't error if point is at beginning of buffer
                         (condition-case nil
                             (progn
                               (nxml-backward-up-element) ; always returns nil
                               t)
                           (error nil))))
            (setq path (cons (xmltok-start-tag-local-name) path))))
        (let ((result (format "/%s" (mapconcat 'identity path "/"))))
          (when (called-interactively-p t)
            (message "%s" result))
          result)))

    (defun my/nxml-where-copy ()
      "Copy the hierarchy of XML elements the point is on to the clipboard."
      (interactive)
      (let ((path (nxml-where))))
      (kill-new path))))

;;; Dotfiles

(use-package chezmoi
  :config
  ;; Use the base file's major mode
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . (lambda ()
                                                   (let ((base-name (file-name-sans-extension buffer-file-name)))
                                                     (set-auto-mode-0 (assoc-default base-name auto-mode-alist 'string-match))))))
  :hook
  ;; Turn off ligatures because they show up poorly.
  (chezmoi-mode-hook . (lambda () (when (require 'ligature)
                                    (ligature-mode (if chezmoi-mode 0 1))))))

(provide 'init)

;;; init.el ends here
