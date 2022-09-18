;; -----------------------------------------------------------------------------
;; Init
;; -----------------------------------------------------------------------------

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

;; -----------------------------------------------------------------------------
;; Package management
;; -----------------------------------------------------------------------------

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

;; Ensure PATH is correct when launched as GUI application
(use-package exec-path-from-shell)

;; -----------------------------------------------------------------------------
;; GUI
;; -----------------------------------------------------------------------------

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
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(setq visible-bell 1)

(setq confirm-kill-emacs 'y-or-n-p) ; y and n instead of yes and no when quitting

;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Mode-line
(use-package doom-modeline
  :after (all-the-icons)
  :init (doom-modeline-mode 1))
(doom-modeline-mode) ; TODO: This shouldn't be needed

;; -----------------------------------------------------------------------------
;; Editor
;; -----------------------------------------------------------------------------

(use-package which-key
  :diminish)
(which-key-mode)

;; Indentation
(use-package indent-guide
  :config
  (indent-guide-global-mode))

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; Delete trailing spaces
(setq require-final-newline t) ; Add new line in the end of a file on save.

;; Pretty parens
(use-package rainbow-delimiters
  :diminish
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Smart parens
(use-package parinfer-rust-mode
  :diminish
  :hook emacs-lisp-mode)

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

;; Vim
(use-package evil)
(evil-mode 1)

(evil-set-undo-system 'undo-tree)

;; Languages

;; Rust
(use-package rust-mode)

;; Web
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; Elixir
(use-package elixir-mode)
(use-package mix
  :config
  (add-hook 'elixir-mode-hook 'mix-minor-mode))
(use-package flycheck-credo
  :init
  '(flycheck-credo-setup)
  :config
  (setq flycheck-elixir-credo-strict t))

;; Go
(use-package go-mode)

;; JSON
(use-package json-mode)

;; Python
(use-package python-mode)

;; Markdown
(use-package markdown-mode)

;; YAML
(use-package yaml-mode)

;; TOML
(use-package toml-mode)

;; Ruby
(use-package ruby-mode)

;; Lua
(use-package lua-mode)

;; Shell
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; Formatting
(use-package format-all)

;; Treesitter
(use-package tree-sitter
  :diminish)
(use-package tree-sitter-langs
  :diminish)

(require 'tree-sitter)
(require 'tree-sitter-langs)

(global-tree-sitter-mode)

(use-package tree-edit)
(use-package evil-tree-edit
  :after (tree-edit)
  :init
  (add-hook 'python-mode-hook 'evil-tree-edit-mode))

;; LSP
(use-package lsp-mode
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
  (toml-mode . lsp)
  (ruby-mode . lsp)
  (shell-mode . lsp)
  (lua-mode . lsp)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

;; Snippets

(use-package yasnippet)
(use-package yasnippet-snippets)
(yas-global-mode 1)

;; Code completion
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

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
  :config (setq projectile-project-search-path '("~/Documents/projects/" ("~/src/" . 2)))
  :init (counsel-projectile-mode))

;; Make Ivy a bit more friendly by adding information to ivy buffers, e.g. description of commands in Alt-x, meta info when switching buffers, etc.
(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)) ;; Abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)

(use-package smex)  ;; show recent commands when invoking Alt-x (or Cmd+Shift+p)
(use-package flx)   ;; enable fuzzy matching
(use-package avy)   ;; enable avy for quick navigation

;; -----------------------------------------------------------------------------
;; File system
;; -----------------------------------------------------------------------------

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

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package lsp-treemacs
  :after (treemacs magit)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
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
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(use-package ripgrep)
(use-package projectile-ripgrep)

(use-package org)

;; -----------------------------------------------------------------------------
;; Version control
;; -----------------------------------------------------------------------------

;; Magit
(use-package magit
  :straight t
  :config
  (global-set-key (kbd "s-g") 'magit-status))   ;; Cmd+g for git status

;; Show changes in the gutter
(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode +1))
;; -----------------------------------------------------------------------------
;; OS
;; -----------------------------------------------------------------------------

;; macOS

(setq delete-by-moving-to-trash t)

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
