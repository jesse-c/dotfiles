;;; init.el --- My config -*- lexical-binding: t; -*-

;;; Commentary:

;;; My config.

;;; Code:

;; -----------------------------------------------------------------------------
;; Init
;; -----------------------------------------------------------------------------

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))
(setq read-process-output-max (* 1024 1024))

;; -----------------------------------------------------------------------------
;; Package management
;; -----------------------------------------------------------------------------

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Effectively replace use-package with straight-use-package
;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-ensure t)

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; -----------------------------------------------------------------------------
;; Org
;; -----------------------------------------------------------------------------

(use-package org)
(use-package org-contrib)
(use-package org-journal)

;; -----------------------------------------------------------------------------
;; Common
;; -----------------------------------------------------------------------------

(use-package ht)
(use-package dash)
(use-package loop)
(use-package s)
(use-package crux)
(use-package request)
(use-package restclient)
(use-package async
  :commands (async-start))
(use-package load-relative)
(use-package f)

;; -----------------------------------------------------------------------------
;; Server
;; -----------------------------------------------------------------------------

;; Hint: emacsclient -n file1 file2 (Use -c to open in a new frame)
;; Example: emacsclient --socket-name ~/.config/emacs/server/server FILE

(setq server-socket-path "~/.config/emacs/server") ; Make the directory if it doesn't exist
(make-directory server-socket-path t)
(setq server-socket-dir server-socket-path)

(load "server")
(unless (< emacs-major-version 23)
  (defun server-ensure-safe-dir (_dir) "Noop" t))

(unless (server-running-p) (server-start))

;; -----------------------------------------------------------------------------
;; OS
;; -----------------------------------------------------------------------------

;; Disabling suspend-frame binding
;; Very annoying binding, lets get rid of it.
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#disabling-suspend-frame-binding
(global-unset-key (kbd "C-z"))

(use-package chezmoi
  :defer 1)

(setq straight-versions-path "straight/versions/default.el")

(defun my/straight-versions-path ()
  (-> user-init-file
      (file-name-directory)
      (file-name-concat straight-versions-path)
      (message)))

(defun my/chezmoi-copy-package-freeze ()
  (interactive)
  (copy-file (my/straight-versions-path) (file-name-concat "~/.local/share/chezmoi/home/private_dot_config/emacs" straight-versions-path t)))

;; macOS

(when (memq window-system '(mac ns))
  (setq dired-use-ls-dired nil))

(setq delete-by-moving-to-trash t)

;; https://emacs.stackexchange.com/a/41767
;; Alternative to try: https://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
(setq use-dialog-box nil)

;; Ensure PATH is correct when launched as GUI application
(use-package exec-path-from-shell)

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
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

;; All

;; Tidy up .emacs.d mess
(use-package no-littering)

;; -----------------------------------------------------------------------------
;; Terminal
;; -----------------------------------------------------------------------------

(use-package vterm
  :defer 1)

;; -----------------------------------------------------------------------------
;; GUI
;; -----------------------------------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p)

(use-package browse-url
  :defer 1
  :bind
  ("<s-mouse-1>" . browse-url-at-mouse))

;; Setting default coding system
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#setting-default-coding-system
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Focus new frame
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#focus-new-frame
(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
  (when (display-graphic-p)
    (ns-raise-emacs)))

;; Themes
(defun my/theme-by-current-time ()
  "Get the light or dark THEME based on the current time."
  (let ((hour (->> (current-time)
                   (decode-time)
                   (nth 2))))
    (if (or (> hour 18) (< hour 5))
        'spacemacs-dark
        'spacemacs-light)))

(defun my/load-theme-by-current-time ()
  "Load the right theme based on the current time."
  (load-theme (my/theme-by-current-time) t))

(defun disable-all-themes ()
  "Disable all enabled themes."
  (interactive)
  ;; List of enabled Custom Themes, highest precedence
  ;; first.
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(use-package spacemacs-theme
  :defer t ; Don't load it immediately
  :init
  (my/load-theme-by-current-time))

(use-package doom-themes
  :defer t) ; Don't load it immediately

;; Disabled as AppearanceNotifier is used
;; (run-with-timer 0 (* 5 60) 'my/load-theme-by-current-time)

;; Disabled while I use a different distribution
;; (defun my/apply-theme (appearance)
;;   "Load theme, taking current system APPEARANCE into consideration."
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (pcase appearance
;;     ('light (load-theme 'spacemacs-light t))
;;     ('dark (load-theme 'spacemacs-dark t))))
;;
;; (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; Font
(set-frame-font "JetBrains Mono 12" nil t)

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Default frame size
(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 120))

;; Initial frame content
(setq inhibit-splash-screen t) ; No startup screen
(setq inhibit-startup-message t) ; No startup message
(setq initial-scratch-message nil) ; Empty scratch buffer

;; Keyboard-centric user interface
(scroll-bar-mode  -1)
(tool-bar-mode    -1)
(tooltip-mode     -1)
(menu-bar-mode    -1)
(setq visible-bell 1)

(setq confirm-kill-emacs 'y-or-n-p) ; y and n instead of yes and no when quitting

;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Tab-bar
(setq tab-bar-tab-hints t) ; Show tab numbers
(setq tab-bar-mode t)
(setq tab-bar-show nil)
(setq tab-bar-new-tab-choice "*scratch*")

(global-set-key (kbd "C-<tab>") 'tab-bar-switch-to-tab)

;; Mode-line
(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode 1))

(use-package all-the-icons)

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

(use-package zoom-window
  :defer 1)

;; Keymaps
(use-package hydra)

;; Buffers
(defun my/clean-up-buffers ()
  "Clean-up buffers that have built up."
  (interactive)
  (kill-matching-buffers ".*\.ex" nil t) 
  (kill-matching-buffers ".*\.exs" nil t) 
  (kill-matching-buffers ".*\.yaml" nil t)) 

;; -----------------------------------------------------------------------------
;; Editor
;; -----------------------------------------------------------------------------

;; By default, Emacs thinks a sentence is a full-stop followed by 2 spaces. Let’s make it full-stop and 1 space.
;; http://sriramkswamy.github.io/dotemacs/
(setq sentence-end-double-space nil)

;; Bindings
(use-package which-key
  :diminish
  :init (which-key-mode))

;;; Minibuffer
(use-package savehist
  :defer 1
  :after no-littering
  :init
  (savehist-mode))

;; Indentation
(use-package indent-guide
  :defer 1
  :config
  (indent-guide-global-mode))

(setq-default indent-tabs-mode nil)

(use-package aggressive-indent
  :hook
  (elixir-mode-hook . aggressive-indent-mode)
  (rust-mode-hook . aggressive-indent-mode)
  (swift-mode-hook . aggressive-indent-mode))

;; Folding
(use-package origami
  :defer t)

;; Search
(setq isearch-lazy-count t)

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c C-z"))
  :init
  (persp-mode))

(use-package comby
  :defer t)

;; Editor Config
(use-package editorconfig
  :defer 1
  :diminish
  :config
  (editorconfig-mode 1))

;; Tree-sitter
(require 'treesit)

;; LSP
(use-package eglot
  ;; It's built-in, so don't load it from an external source
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
              `(toml-mode "taplo" "lsp" "stdio"))
  (add-to-list 'eglot-server-programs
              `(toml-ts-mode "taplo" "lsp" "stdio")))

;; Whitespace
(use-package whitespace
  :hook
  (before-save-hook . delete-trailing-whitespace) ; Delete trailing spaces
  :config
  (setq require-final-newline t)) ; Add new line in the end of a file on save.

;; Pretty parens
(use-package rainbow-delimiters
  :diminish
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

;; Structured editing
(use-package puni
  :defer t
  :init
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(use-package parinfer-rust-mode
  :diminish
  :hook emacs-lisp-mode)

;; Comments
;; Comment line or region
(global-set-key (kbd "s-/") 'comment-line)

;; Spellchecking and thesaurus -------------------------------------------------

(setq ispell-dictionary "australian")    ;set the default dictionary
;; Spellchecking requires an external command to be available. Install aspell on your Mac, then make it the default checker for Emacs' ispell. Note that personal dictionary is located at ~/.aspell.LANG.pws by default.
(setq ispell-program-name "aspell")

;; Popup window for spellchecking
(use-package flyspell
  :diminish)
(use-package flyspell-correct
  :after flyspell)
(use-package flyspell-correct-popup
  :after flyspell)

;; Enable spellcheck on the fly for all text modes. This includes org, latex and LaTeX.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Enable right mouse click on macOS to see the list of suggestions.
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; Spellcheck current word
(define-key flyspell-mode-map (kbd "s-\\") 'flyspell-correct-previous-word-generic) ;; Cmd+\ spellcheck word with popup
(define-key flyspell-mode-map (kbd "C-s-\\") 'ispell-word)                          ;; Ctrl+Cmd+\ spellcheck word using built UI

;; Search for synonyms
(use-package powerthesaurus
  :bind
  ("s-|" . powerthesaurus-lookup-word-dwim))

;; Word definition search
(use-package define-word
  :bind
  ("M-\\" . define-word-at-point))

;; Highlight current line
(global-hl-line-mode t)

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

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

(use-package goto-chg)

;; Vim
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (global-unset-key (kbd "C-u"))
  (global-set-key (kbd "C-u") 'evil-scroll-up))

(evil-set-undo-system 'undo-tree)

;; Programming

;; Overwrite existing function
;; (defun my/find-alternate-file ()
;;   "Find alternate FILE, if any."
;;   (interactive)
;;   (let* ((default-directory (projectile-project-root))
;;          ;; https://emacs.stackexchange.com/questions/45419/get-file-name-relative-to-projectile-root
;;          (buffile (file-relative-name buffer-file-name (projectile-project-root)))
;;          (cmd (format "alt %s" buffile))
;;          (output (shell-command-to-string cmd)))
;;     (if (string= output "")
;;         (message "No alternate file found")
;;       (if (y-or-n-p (format "Found alternate file %s. Open?" output))
;;        (find-file output (message "Not opening"))))))

;; Languages

(use-package quickrun)

;; Rust
(use-package rust-mode)
(use-package rustic)
(use-package cargo
  :hook
  (rust-mode-hook . cargo-minor-mode))

;; Web
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; Gradle
(use-package gradle-mode)

;; Elixir
(use-package elixir-mode)
(straight-use-package '(apprentice :host github
                                   :repo "Sasanidas/Apprentice"
                                   :branch "master"))
(use-package mix
  :hook
  (elixir-mode-hook . mix-minor-mode))
(use-package exunit
  :hook
  (elixir-mode-hook . exunit-mode))

;; Go
(use-package go-mode)

;; Swift
(use-package swift-mode)

;; JSON
(use-package json-mode)

;; Python
(use-package python-mode)

;; Protobuf
(use-package protobuf-mode)

;; CSV
(use-package csv-mode)

;; Markdown
(use-package markdown-mode)

;; YAML
(use-package yaml-mode)

;; Ruby
(use-package ruby-mode)

;; HCL
(use-package hcl-mode)

;; Terraform
(use-package terraform-mode
  :after hcl-mode)
(use-package terraform-doc
  :after terraform-mode)

;; GraphQL
(use-package graphql-mode)

;; Lua
(use-package lua-mode)

;; Shell
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook
  (sh-mode-hook . flymake-shellcheck-load))

;; Clojure
(use-package clojure-mode)
(use-package clojure-snippets)

(use-package cider
  :defer t
  :config
  (setq nrepl-log-messages t))

;; .env
(use-package dotenv-mode)

;; SASS
(use-package sass-mode)

;; Javascript
(use-package npm
  :commands
  (npm-update
   npm-run
   npm-menu
   npm-init
   npm-install-menu
   npm-mode
   npm))

;; Typescript
(use-package typescript-mode)

;; Docker
(use-package docker)
(use-package dockerfile-mode)
(use-package docker-compose-mode)

;; Caddy
(use-package caddyfile-mode
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

;; Emacs Lisp
(use-package eldoc
  :diminish eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
        (lisp-interaction-mode . turn-on-eldoc-mode))

;; Formatting
(use-package format-all
  :commands
  (format-all-buffer format-all-region format-all-mode))
  ; Disabled for now whilst using apheleia
  ;; :init)
  ; Auto-format code on save
  ;; (add-hook 'elixir-mode-hook 'format-all-mode)
  ;; (add-hook 'rust-mode-hook 'format-all-mode)
  ;; (add-hook 'python-mode-hook 'format-all-mode)
  ;; (add-hook 'lua-mode-hook 'format-all-mode)
  ;; (add-hook 'go-mode-hook 'format-all-mode)
  ;; (add-hook 'clojure-mode-hook 'format-all-mode))

(use-package apheleia
  :hook
  (elixir-mode . apheleia-mode)
  (swift-mode . apheleia-mode)
  (rust-mode . apheleia-mode)
  (javascript-mode . apheleia-mode)
  (sass-mode . apheleia-mode))

;; Testing
(use-package coverlay
  :defer t)

;; Snippets
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)


;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

;; Completion
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
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

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(use-package vertico
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package smex)  ;; show recent commands when invoking Alt-x (or Cmd+Shift+p)
(use-package avy)   ;; enable avy for quick navigation

;; -----------------------------------------------------------------------------
;; File system
;; -----------------------------------------------------------------------------

(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))
(setq create-lockfiles nil)

(defun my/copy-buffer-name ()
  (interactive)
  (kill-new (buffer-file-name)))

;; (defun my/copy-project-buffer-name ()
;;   (interactive)
;;   (kill-new (file-relative-name buffer-file-name (projectile-project-root))))

(use-package recentf
  :defer 1
  :config
  (recentf-mode t))

(save-place-mode 1)

(use-package treemacs
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

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-perspective ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs perspective) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

;; Don't bother with auto save and backups
(setq auto-save-default nil) ; stop creating #autosave# files
(setq make-backup-files nil) ; stop creating backup~ files

;; -----------------------------------------------------------------------------
;; CI/CD
;; -----------------------------------------------------------------------------

;; It's either the index, [0, ∞), or nil
(let ((match (string-match-p "Knapsack" system-name)))
  (when (or match (eq 0 match))
    (load "~/.config/emacs/circleci.el")))

;; TODO Treat it as a package
;; (use-package circleci
;;   :ensure nil
;;   :load-path "~/.config/emacs/circleci.el"
;;   :commands
;;   (my/circleci-get-branch-status
;;    my/circleci-get-current-branch-status
;;    my/circleci-get-branch-latest-workflow
;;    my/circleci-get-current-branch-latest-workflow
;;    my/circleci-rerun-workflow))

;; -----------------------------------------------------------------------------
;; Process
;; -----------------------------------------------------------------------------

(use-package ripgrep
  :defer 1)

(use-package org)

;; -----------------------------------------------------------------------------
;; VCS
;; -----------------------------------------------------------------------------

;; Magit
(use-package magit
  :config
  ;; Setting ‘forge-add-default-bindings’ to nil in ‘evil-collection-forge-setup’.
  ;; To suppress this message you can set this variable to nil in your init.el file.
  (setq forge-add-default-bindings nil)
  :bind
  ("s-g" . magit-status))

;; Disable due to performance on large repositories
;; (use-package magit-todos
;;   :after magit
;;   :init (magit-todos-mode))

(use-package forge
  :after magit
  :defer 1
  :config
  (setq auth-sources '("~/.authinfo")))

;; Show changes in the gutter
(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode +1))

;; Show author
(use-package blamer
  :init (global-blamer-mode 1))

;; Create URLs to files and commits in repository hosting services
(use-package git-link
  :defer 1
  :config
  (setq git-link-use-commit t))

;; Browse target page on github/bitbucket from emacs buffers
(use-package browse-at-remote)

;; Why was this line changed
(use-package git-messenger)

;; View versions of a file
;; (use-package git-timemachine)

;; View git blame
(use-package vc-msg)
