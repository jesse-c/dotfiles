;;; init.el -*- lexical-binding: t; -*-
;;; Commentary: -
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

;; -----------------------------------------------------------------------------
;; Org
;; -----------------------------------------------------------------------------

(use-package org)
(use-package org-contrib)
(use-package org-journal)

;; -----------------------------------------------------------------------------
;; Common
;; -----------------------------------------------------------------------------

(use-package dash)
(use-package loop)
(use-package s)
(use-package crux)
(use-package request)
(use-package restclient)
(use-package async
  :commands (async-start))

;; -----------------------------------------------------------------------------
;; Server
;; -----------------------------------------------------------------------------

;; Hint: emacsclient -n file1 file2 (Use -c to open in a new frame)
;; Example: emacsclient --socket-name ~/.config/emacs/server/server FILE

; Make the directory if it doesn't exist
(setq server-socket-path "~/.config/emacs/server")
(make-directory server-socket-path t)
(setq server-socket-dir server-socket-path)

(load "server")
(unless (< emacs-major-version 23)
    (defun server-ensure-safe-dir (dir) "Noop" t))

(unless (server-running-p) (server-start))

;; -----------------------------------------------------------------------------
;; OS
;; -----------------------------------------------------------------------------

;; Disabling suspend-frame binding
;; Very annoying binding, lets get rid of it.
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#disabling-suspend-frame-binding
(global-unset-key (kbd "C-z"))

(use-package chezmoi
  :after magit)

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

(use-package vterm)

;; -----------------------------------------------------------------------------
;; GUI
;; -----------------------------------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p)

(use-package browse-url
  :bind ("<s-mouse-1>" . browse-url-at-mouse))

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
  "Get the light or dark THEME based on the current time"
  (let ((hour (->> (current-time)
                   (decode-time)
                   (nth 2))))
    (if (or (> hour 18) (< hour 5))
        'spacemacs-dark
        'spacemacs-light)))

(defun my/load-theme-by-current-time ()
  "Load the right theme based on the current time"
  (load-theme (my/theme-by-current-time) t))

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

(use-package zoom-window)

;; Keymaps
(use-package hydra)

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
  :after no-littering
  :init
  (savehist-mode))

;; Indentation
(use-package indent-guide
  :config
  (indent-guide-global-mode))

(setq-default indent-tabs-mode nil)

(use-package aggressive-indent
  :hook
  (elixir-mode-hook . aggressive-indent-mode)
  (rust-mode-hook . aggressive-indent-mode)
  (swift-mode-hook . aggressive-indent-mode))

;; Search
(use-package anzu
  :init
  (global-anzu-mode +1))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c C-z"))
  :init
  (persp-mode))

;; Scratch
;; TODO https://codeberg.org/emacs-weirdware/scratch

;; Editor Config
(use-package editorconfig
  :init
  (editorconfig-mode 1))

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Delete trailing spaces
(setq require-final-newline t) ; Add new line in the end of a file on save.

;; Pretty parens
(use-package rainbow-delimiters
  :diminish
  :config
  ; Start the mode automatically in most programming modes
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Smart parens
(use-package smartparens
  :config
  (add-hook 'elixir-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'html-mode-hook #'smartparens-mode)
  (add-hook 'lua-mode-hook #'smartparens-mode)
  (add-hook 'python-mode-hook #'smartparens-mode)
  (add-hook 'rust-mode-hook #'smartparens-mode)
  (add-hook 'ruby-mode-hook #'smartparens-mode)
  (add-hook 'swift-mode-hook #'smartparens-mode)
  (add-hook 'clojure-mode-hook #'smartparens-mode))

(require 'smartparens-config)

(use-package parinfer-rust-mode
  :diminish
  :hook emacs-lisp-mode)

;; Comments
;; Comment line or region
(global-set-key (kbd "s-/") 'comment-line)

;; Spellchecking and thesaurus -------------------------------------------------

;; Spellchecking requires an external command to be available. Install aspell on your Mac, then make it the default checker for Emacs' ispell. Note that personal dictionary is located at ~/.aspell.LANG.pws by default.
(setq ispell-program-name "aspell")

;; Popup window for spellchecking
(use-package flyspell-correct)
(use-package flyspell-correct-popup)

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
  :config
  (global-set-key (kbd "s-|") 'powerthesaurus-lookup-word-dwim)) ;; Cmd+Shift+\ search thesaurus

;; Word definition search
(use-package define-word
  :config
  (global-set-key (kbd "M-\\") 'define-word-at-point))

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
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-anzu)

(evil-set-undo-system 'undo-tree)

;; Programming

;; Overwrite existing function
(defun my/find-alternate-file ()
  "Find alternate FILE, if any."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         ;; https://emacs.stackexchange.com/questions/45419/get-file-name-relative-to-projectile-root
         (buffile (file-relative-name buffer-file-name (projectile-project-root)))
         (cmd (format "alt %s" buffile))
         (output (shell-command-to-string cmd)))
    (if (string= output "")
        (message "No alternate file found")
      (if (y-or-n-p (format "Found alternate file %s. Open?" output))
       (find-file output (message "Not opening"))))))

;; Languages

(use-package quickrun)

;; Rust
(use-package rust-mode)
(use-package rustic)
(use-package cargo
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))
(use-package flycheck-rust
  :after (flycheck rust-mode)
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Web
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; Java
(use-package lsp-java
  :after lsp-mode)

;; Gradle
(use-package gradle-mode)
(use-package flycheck-gradle)

;; Elixir
(use-package elixir-mode)
(use-package alchemist)
(use-package mix
  :config (add-hook 'elixir-mode-hook 'mix-minor-mode))
(use-package flycheck-credo
  :init
  '(flycheck-credo-setup)
  :config
  (setq flycheck-elixir-credo-strict t))
(use-package flycheck-elixir
  :after (flycheck elixir-mode)
  :config (add-hook 'elixir-mode-hook 'flycheck-mode))
(use-package exunit
  :init (add-hook 'elixir-mode-hook 'exunit-mode))

;; Go
(use-package go-mode)

;; Swift
(use-package swift-mode)
(use-package lsp-sourcekit
  :after (lsp swift-mode))
(use-package flycheck-swift
  :after (swift-mode))

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

;; Lua
(use-package lua-mode)

;; Shell
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; Clojure
(use-package clojure-mode)
(use-package clojure-snippets)

(use-package cider
  :defer t
  :config
  (setq nrepl-log-messages t)
  (flycheck-clojure-setup)) ;; run setup *after* cider load

(use-package flycheck-clojure
  :defer t
  :commands (flycheck-clojure-setup) ;; autoload
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; .env
(use-package dotenv-mode)

;; SASS
(use-package sass-mode)

;; Typescript
(use-package typescript-mode)

;; Docker
(use-package docker)
(use-package dockerfile-mode)
(use-package docker-compose-mode)

;; Emacs Lisp
(use-package eldoc
  :diminish eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
        (lisp-interaction-mode . turn-on-eldoc-mode))

;; Writing
(use-package flycheck-vale
  :after flycheck
  :init (flycheck-vale-setup))

;; Syntax
(use-package flycheck
  :init (global-flycheck-mode))

;; Formatting
(use-package format-all)
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
  (rust-mode . apheleia-mode))

;; Treesitter
(use-package tree-sitter
  :diminish)
(use-package tree-sitter-langs
  :diminish)

(require 'tree-sitter)
(require 'tree-sitter-langs)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package tree-edit)
(use-package evil-tree-edit
  :after (tree-edit)
  :init
  (add-hook 'python-mode-hook 'evil-tree-edit-mode))

;; LSP
(use-package lsp-mode
  :config
  (setq lsp-elixir-server-command '("elixir-ls"))
  (setq lsp-clients-lua-language-server-command '("lua-language-server"))
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (python-mode . lsp)
  (rust-mode . lsp)
  (elixir-mode . lsp)
  (json-mode . lsp)
  (go-mode . lsp)
  (markdown-mode . lsp)
  (yaml-mode . lsp)
  (conf-toml-mode . lsp)
  (ruby-mode . lsp)
  (shell-mode . lsp)
  (lua-mode . lsp)
  (clojure-mode . lsp)
  (swift-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

;; DAP

(use-package dap-mode)

;; Snippets

(use-package yasnippet)
(use-package yasnippet-snippets)
(use-package ivy-yasnippet
  :after (yasnippet yasnippet-snippets ivy))
(yas-global-mode 1)

;; Code completion
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))
(use-package company-quickhelp
  :after (company)
  :init (company-quickhelp-mode))
(use-package company-statistics
  :after (company)
  :init (company-statistics-mode))
(use-package company-box
  :hook (company-mode . company-box-mode))
(use-package company-shell
  :after (company shell)
  :config
  (add-to-list 'company-backends 'company-shell))
(use-package company-elixir
  :disabled t
  :company elixir-mode)

;; Menu completion
;; Use minimalist Ivy for most things.
(use-package ivy
  :diminish                             ;; don't show Ivy in minor mode list
  :config
  (ivy-mode 1)                          ;; enable Ivy everywhere
  (setq ivy-use-virtual-buffers t)      ;; show bookmarks and recent files in buffer list
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))   ;; enable fuzzy searching everywhere except for Swiper

  (global-set-key (kbd "s-b") 'ivy-switch-buffer)  ;; Cmd+b show buffers and recent files
  (global-set-key (kbd "M-s-b") 'ivy-resume))      ;; Alt+Cmd+b resume whatever Ivy was doing

(use-package ivy-posframe
  :after ivy
  :init
  (ivy-posframe-mode 1))

;; Swiper is a better local finder.
(use-package swiper
  :config
  (global-set-key "\C-s" 'swiper)       ;; Default Emacs Isearch forward...
  (global-set-key "\C-r" 'swiper)       ;; ... and Isearch backward replaced with Swiper
  (global-set-key (kbd "s-f") 'swiper)) ;; Cmd+f find text

;; Better menus with Counsel (a layer on top of Ivy)
(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)            ;; Alt+x run command
  (global-set-key (kbd "s-P") 'counsel-M-x)            ;; Cmd+Shift+p run command
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)  ;; Replace built-in Emacs 'find file' (open file) with Counsel
  (global-set-key (kbd "s-o") 'counsel-find-file))     ;; Cmd+o open file

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (setq projectile-project-search-path '("~/Documents/projects/" ("~/src/" . 3)))
  (setq projectile-auto-discover nil)
  :init (counsel-projectile-mode))

;; Make Ivy a bit more friendly by adding information to ivy buffers, e.g. description of commands in Alt-x, meta info when switching buffers, etc.
(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)) ;; Abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)

(use-package smex)  ;; show recent commands when invoking Alt-x (or Cmd+Shift+p)
(use-package flx)   ;; enable fuzzy matching
(use-package avy)   ;; enable avy for quick navigation

(use-package prescient)
(use-package ivy-prescient
  :after (ivy prescient)
  :init (ivy-prescient-mode t))
(use-package company-prescient
  :after (company prescient)
  :init (company-prescient-mode t))

;; -----------------------------------------------------------------------------
;; File system
;; -----------------------------------------------------------------------------

(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))
(setq create-lockfiles nil)

(defun my/copy-buffer-name ()
  (interactive)
  (kill-new (buffer-file-name)))

(defun my/copy-project-buffer-name ()
  (interactive)
  (kill-new (file-relative-name buffer-file-name (projectile-project-root))))

(recentf-mode t)

(save-place-mode 1)

(use-package dirvish)
  ;; :config)
  ;; (dirvish-override-dired-mode))

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

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :config (lsp-treemacs-sync-mode 1))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after treemacs
  :config (treemacs-set-scope-type 'Tabs))

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

;; Don't bother with auto save and backups
(setq auto-save-default nil) ; stop creating #autosave# files
(setq make-backup-files nil) ; stop creating backup~ files

;; -----------------------------------------------------------------------------
;; Process
;; -----------------------------------------------------------------------------

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-switch-project-action #'projectile-commander)
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(use-package ripgrep)
(use-package projectile-ripgrep)

(use-package org)

;; -----------------------------------------------------------------------------
;; VCS
;; -----------------------------------------------------------------------------

;; Magit
(use-package magit
  :config
  (global-set-key (kbd "s-g") 'magit-status))

;; Disable due to performance on large repositories
;; (use-package magit-todos
;;   :after magit
;;   :init (magit-todos-mode))

(use-package forge
  :after magit
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
(use-package git-link)

;; Browse target page on github/bitbucket from emacs buffers
(use-package browse-at-remote)

;; Why was this line changed
(use-package git-messenger)

;; View versions of a file
;; (use-package git-timemachine)

;; View git blame
(use-package vc-msg)
