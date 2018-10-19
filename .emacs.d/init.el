;;; -*- lexical-binding: t -*-


;; ====
;; INIT


;; Package system and sources.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)


;; We will use 'use-package' to install and configure packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))


;; No need to out 'ensure' everywhere, since we don't use anything else to install packages.
(setq use-package-always-ensure t)


;; Pass system shell environment to Emacs. This is important primarily for shell inside Emacs, but also things like Org mode export to Tex PDF don't work, since it relies on running external command pdflatex, which is loaded from PATH.
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

; (if (eq system-type 'darwin)
;   (progn
;     (exec-path-from-shell-copy-env "PATH")
;     (exec-path-from-shell-copy-env "GOPATH")
;     (exec-path-from-shell-copy-env "GOROOT")
;     (exec-path-from-shell-copy-env "GOBIN")))


;; Store custom-file separately, don't freak out when it's not found
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;; Set path for private config. private.el is not part of Castlemacs and you can use it for your personal
;; additions. Do not change init.el yourself, it will make updates harder.
(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))


;; =============
;; MODIFIER KEYS


;; Both command keys are 'Super'
(setq mac-right-command-modifier 'super)
(setq mac-command-modifier 'super)


;; Option or Alt is naturally 'Meta'
(setq mac-option-modifier 'meta)


;; Right Alt (option) can be used to enter symbols like em dashes '—' and euros '€' and stuff.
(setq mac-right-option-modifier 'nil)

;; Control is control, and you also need to change Caps Lock to Control in the Keyboard
;; preferences in macOS.


;; =============
;; SANE DEFAULTS


;; Smoother and nicer scrolling
(setq scroll-margin 10
      scroll-step 1
      next-line-add-newlines nil
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      scroll-up-aggressively 0
      scroll-down-aggressively 0
      scroll-margin 3)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))


;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))


;; Don't bother with auto save and backups.
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)


;; Cleaner storage
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))


;; Warn only when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)


;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)


;; Revert (update) buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)

;; Larger initial frame.
(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist '(height . 140))


(setq
 inhibit-startup-message t         ; Don't show the startup message...
 inhibit-startup-screen t          ; ... or screen
 cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows
 show-paren-mode t

 echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffer
 initial-scratch-message nil       ; Empty scratch buffer
 ;; initial-major-mode 'org-mode      ; Org mode by default
 sentence-end-double-space nil     ; Sentences should end in one space, come on!
 confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
 help-window-select t              ; Select help window so it's easy to quit it with 'q'
 )

(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(delete-selection-mode 1)          ; Delete selected text when typing
(global-unset-key (kbd "s-p"))     ; Don't print


;; We need Emacs kill ring and system clipboard to be independent. Simpleclip is the solution to that.
(use-package simpleclip
  :config
  (simpleclip-mode 1))


;; Things you'd expect from macOS app.
(global-set-key (kbd "s-s") 'save-buffer)             ;; save
(global-set-key (kbd "s-S") 'write-file)              ;; save as
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit
(global-set-key (kbd "s-a") 'mark-whole-buffer)       ;; select all
;; (global-set-key (kbd "s-z") 'undo)


;; Delete trailing spaces and add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)


;; Linear undo and redo.
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/tmp/undo"))
          undo-tree-auto-save-history t
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t)))

(global-set-key (kbd "s-z") 'undo-tree-undo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)


;; History
(setq savehist-file "~/.emacs.d/savehist")
(setq history-length 1000)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode t)
(use-package browse-kill-ring)


;; =======
;; VISUALS


;; Enable transparent title bar on macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))


;; Font
;; (when (member "menlo" (font-family-list))
;;  (set-face-attribute 'default nil :font "Menlo-7"))
(when (member "Menlo" (font-family-list)) (set-frame-font "Menlo-11" t t))
(setq-default line-spacing 2)


;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(use-package color-theme-sanityinc-tomorrow)


;; Nice and simple default light theme.
;; (load-theme 'tsdh-light)


;; Line numbers
(global-display-line-numbers-mode)


;; Pretty icons
(use-package all-the-icons)
;; MUST DO M-x all-the-icons-install-fonts after


;; Hide toolbar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Bells
(setq ring-bell-function 'ignore)
(setq visible-bell t)


;; Always wrap lines
(global-visual-line-mode 1)


;; Highlight current line
(global-hl-line-mode 1)


;; Show parens and other pairs.
(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))


;; Hide minor modes from modeline
(use-package rich-minority
  :config
  (rich-minority-mode 1)
  (setf rm-blacklist ""))


;; Set colors to distinguish between active and inactive windows
(set-face-attribute 'mode-line nil :background "SlateGray1")
(set-face-attribute 'mode-line-inactive nil :background "grey93")


;; File tree / Sidebar
(use-package neotree
  :config
  (setq neo-window-width 32
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line t
        neo-window-fixed-size nil
        neo-vc-integration nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-show-hidden-files t
        neo-mode-line-type 'none
        neo-auto-indent-point t)
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  (setq neo-hidden-regexp-list '("venv" "\\.pyc$" "~$" "\\.git" "__pycache__" ".DS_Store"))
  (global-set-key (kbd "s-B") 'neotree-toggle))           ;; Cmd+Shift+b toggle tree


;; Show vi-like tilde in the fringe on empty lines.
(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode 1))


;; Show full path in the title bar.
(setq-default frame-title-format "%b (%f)")


;; Never use tabs, use spaces instead.
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq c-basic-offset 2)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default c-basic-indent 2)


;; Show keybindings cheatsheet
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))


;; Disable blinking cursor.
(blink-cursor-mode 0)


;;
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;;
(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))


;;
(use-package indent-guide
  :config
  (indent-guide-global-mode))


;; Statusbar
(use-package telephone-line
  :config
  (telephone-line-mode 1))


;; ================
;; BASIC NAVIGATION


;; Same keys with Shift will move you back and forward between open buffers.
(global-set-key (kbd "s-<") 'previous-buffer)
(global-set-key (kbd "s->") 'next-buffer)


;; ============
;; TEXT EDITING


;; Move-text lines around with meta-up/down.
(use-package move-text
  :config
  (move-text-default-bindings))


;; Comment line or region.
(global-set-key (kbd "s-/") 'comment-line)


;; Multiple cursors. Similar to Sublime or VS Code.
(use-package multiple-cursors
  :config
  (setq mc/always-run-for-all 1)
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this)        ;; Cmd+d select next occurrence of region
  (global-set-key (kbd "s-D") 'mc/mark-all-dwim)              ;; Cmd+Shift+d select all occurrences
  (global-set-key (kbd "M-s-d") 'mc/edit-beginnings-of-lines) ;; Alt+Cmd+d add cursor to each line in region
  (define-key mc/keymap (kbd "<return>") nil))


;; Vim
(use-package evil
  :init
  ;; (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  (setq evil-echo-state nil) ; Do not show state indicators such as '-- INSERT --'
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
(use-package evil-tabs
  :after evil
  :config
  (global-evil-tabs-mode t))
(use-package evil-nerd-commenter
  :after evil
  :config
  (evilnc-default-hotkeys)) ; The hotkey is “,cl” in evil-mode and “M-;” in all modes. “M” means ALT key.


;; =================
;; WINDOW MANAGEMENT


;; This is rather radical, but saves from a lot of pain in the ass.
;; When split is automatic, always split windows vertically
(setq split-height-threshold 0)
(setq split-width-threshold nil)


;; Move between windows with Control-Command-Arrow and with =Cmd= just like in iTerm.
(use-package windmove
  :config
  (global-set-key (kbd "<C-s-left>")  'windmove-left)  ;; Ctrl+Cmd+left go to left window
  (global-set-key (kbd "s-[")  'windmove-left)         ;; Cmd+[ go to left window

  (global-set-key (kbd "<C-s-right>") 'windmove-right) ;; Ctrl+Cmd+right go to right window
  (global-set-key (kbd "s-]")  'windmove-right)        ;; Cmd+] go to right window

  (global-set-key (kbd "<C-s-up>")    'windmove-up)    ;; Ctrl+Cmd+up go to upper window
  (global-set-key (kbd "s-{")  'windmove-up)           ;; Cmd+Shift+[ go to upper window

  (global-set-key (kbd "<C-s-down>")  'windmove-down)  ;; Ctrl+Cmd+down go to down window
  (global-set-key (kbd "s-}")  'windmove-down))        ;; Cmd+Shift+] got to down window


;; Enable winner mode to quickly restore window configurations
(winner-mode 1)
(global-set-key (kbd "M-s-[") 'winner-undo)
(global-set-key (kbd "M-s-]") 'winner-redo)


;; ==================
;; PROJECT MANAGEMENT


;; Use Projectile for project management.
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-s-p") 'projectile-command-map) ;; Ctrl+Cmd+p show projectile menu
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  )


;; ==========================================
;; MENUS AND COMPLETION (not code completion)


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

(use-package smex)  ;; show recent commands when invoking Alt-x (or Cmd+Shift+p)
(use-package flx)   ;; enable fuzzy matching
(use-package avy)   ;; enable avy for quick navigation


;; Make Ivy a bit more friendly by adding information to ivy buffers, e.g. description of commands in Alt-x, meta info when switching buffers, etc.
(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)) ;; Abbreviate paths using abbreviate-file-name (e.g. replace “/home/username” with “~”)


;; Integrate Projectile with Counsel
(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "s-p") 'counsel-projectile-find-file)         ;; Cmd+p open file in current project
  (global-set-key (kbd "s-F") 'counsel-projectile-ag))     ;; Cmd+Shift+F search in current git repository


(setq projectile-completion-system 'ivy)             ;; Use Ivy in Projectile


;; ========================
;; VERSION CONTROL WITH GIT


;; Magit
(use-package magit
  :config
  (global-set-key (kbd "s-g") 'magit-status))   ;; Cmd+g for git status
(use-package evil-magit)


;; Show changes in the gutter
(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode 't)
  (set-face-background 'git-gutter:modified 'nil)   ;; background color
  (set-face-foreground 'git-gutter:added "green4")
  (set-face-foreground 'git-gutter:deleted "red"))


;; ========
;; TERMINAL


(use-package shell-pop
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
   '(shell-pop-universal-key "s-=")))


;; ===============
;; DEVELOPMENT


;; Format
(use-package format-all
  :config
  (add-hook 'prog-mode-hook #'format-all-mode))


;; Check
(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode))

(use-package flycheck-inline
  :after flycheck
  :config
  (flycheck-inline-mode))


(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode))
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))


;; Set the company completion vocabulary to css and html when in web-mode.
(defun my-web-mode-hook ()
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))


;; Language server protocol
(use-package eglot
  :config
  (progn
    (add-hook 'rust-mode-hook 'eglot-ensure)
    (add-hook 'go 'eglot-ensure)
    (add-hook 'python-mode 'eglot-ensure)))


;; Go
(use-package go-mode
  :config
  (add-hook 'prog-mode-hook #'format-all-mode))
(use-package company-go)
;; TODO
;; (use-package company-go
;;   :hook go-mode
;;   :config
;;   (setq tab-width 2)
;;   (setq company-tooltip-limit 20)
;;   (setq company-idle-delay .3)
;;   (setq company-echo-delay 0)
;;   (setq company-begin-commands '(self-insert-command)))


;; Rust
(use-package rust-mode)
(use-package flycheck-rust
  :after rust-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;; SASS
(use-package scss-mode)


;; Python
(use-package python-mode)


;; Elm
(use-package elm-mode)
;; (add-to-list 'company-backends 'company-elm) ; Requires elm-oracle which isn't updated for 0.19


;; JSON
(use-package json-mode)


;; Terraform
(use-package terraform-mode)


;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; ===========================
;; SPELLCHECKING AND THESAURUS


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


;; ===========
;; PROGRAMMING


(use-package yaml-mode)
(use-package haml-mode)
(use-package markdown-mode)


;; Web-mode is an autonomous emacs major-mode for editing web templates.
;; HTML documents can embed parts (CSS / JavaScript) and blocks (client / server side).
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode)))


;; Emmet
(use-package emmet-mode
  :commands emmet-mode
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode)) ;; enable Emmet's css abbreviation.
;; Ctrl+j or Ctrl+Enter to expand


;; ========
;; ORG MODE


;; Some basic Org defaults
(use-package org
  :config
  (setq org-startup-indented t)         ;; Visually indent sections. This looks better for smaller files.
  (setq org-src-tab-acts-natively t)    ;; Tab in source blocks should act like in major mode
  (setq org-src-preserve-indentation t)
  (setq org-log-into-drawer t)          ;; State changes for todos and also notes should go into a Logbook drawer
  (setq org-src-fontify-natively t)     ;; Code highlighting in code blocks
  (setq org-log-done 'time)             ;; Add closed date when todo goes to DONE state
  (setq org-support-shift-select t))    ;; Allow shift selection with arrows.


;; Store all my org files in iCloud for beorg.
(setq org-path "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org")
(setq org-directory org-path)


;; And all of those files should be in included agenda.
(setq org-agenda-files '(org-path))


;; ======
;; CONFIG


;; Open config file by pressing C-x and then C
(global-set-key (kbd "C-x C") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Open private config file by pressing C-x and then c
;; Contain custom settings to private.el to ensure easy Castlemacs updates.
(global-set-key (kbd "C-x c") (lambda () (interactive) (find-file "~/.emacs.d/private.el")))


;; ========
;; TIPS & TRICKS

;; C-x C-f open file
;; M-x search for a command
;; C-g cancel
;; C-h v <search variable
;; C-x b <search buffer>
;; C-x m <search for e.g. projectile>
;; switch-to-buffer
;; --debug-init
;; apropos-variable , apropos-value

;; git-gutter
;; git-gutter:next-hunk
;; Jump to next hunk
;; git-gutter:previous-hunk
;; Jump to previous hunk
;; git-gutter:end-of-hunk
;; Move to end of current hunk

;; flycheck
;; With C-c ! n and C-c ! p you can now jump back and forth between erroneous places.
;; Press C-c ! l to pop up a list of all errors in the current buffer.

;; =======
;; THE END
