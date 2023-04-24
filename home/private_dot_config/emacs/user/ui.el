;;; ui.el --- -*- lexical-binding: t; -*-

;;; Whitespace
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq require-final-newline t) ; Add new line in the end of a file on save.
(add-hook 'before-save-hook #'delete-trailing-whitespace) ; Delete trailing spaces

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<s-mouse-1>") 'browse-url-at-mouse)

;; Soft wrapping
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(add-hook 'org-mode-hook 'visual-line-mode)

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
        'modus-vivendi
      'modus-operandi)))

(defun my/load-theme-by-current-time ()
  "Load the right theme based on the current time."
  (load-theme (my/theme-by-current-time) t))

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

(use-package
  modus-themes
  :custom
  (modus-operandi-theme-faint-syntax t)
  (modus-vivendi-theme-faint-syntax t)
  :config
  (my/load-theme-by-current-time))

(use-package doom-themes)

;; Disabled as AppearanceNotifier is used
;; (run-with-timer 0 (* 5 60) 'my/load-theme-by-current-time)

;; Disabled while I use a different distribution
;; (defun my/apply-theme (appearance)
;;   "Load theme, taking current system APPEARANCE into consideration."
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (pcase appearance
;;     ('light (load-theme 'modus-operandi t))
;;     ('dark (load-theme 'modus-vivendi t))))
;;
;; (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; Font
(set-frame-font "JetBrains Mono 12" nil t)
;; Ideally the Nerd Font variant would be used. The ligatures setup
;; would need to be changed to work with it.
;; (set-frame-font "JetBrainsMono Nerd Font" nil t)

(require 'ligatures)

(use-package
  ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode jetbrains-ligature-mode--ligatures)
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
(setq tab-bar-show t)
(setq tab-bar-new-tab-choice "*scratch*")

(global-set-key (kbd "C-<tab>") 'tab-bar-switch-to-tab)

;; Mode-line
(use-package
  doom-modeline
  :config
  (doom-modeline-mode 1))

;; (use-package moody
;;   :custom
;;   (x-underline-at-descent-line t)
;;   :config
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function))

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

;; Indentation
(use-package
  indent-guide
  :config
  (indent-guide-global-mode))

;; Highlight current line
(global-hl-line-mode t)

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq column-number-mode t)

(use-package recursion-indicator
  :demand t
  :config
  (recursion-indicator-mode))

(provide 'ui)

;;; ui.el ends here
