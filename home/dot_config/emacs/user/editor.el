;;; editor.el --- -*- lexical-binding: t; -*-

;; By default, Emacs thinks a sentence is a full-stop followed by 2 spaces. Let’s make it full-stop and 1 space.
;; http://sriramkswamy.github.io/dotemacs/
(setq sentence-end-double-space nil)

;; Bindings
(use-package
  which-key
  :custom
  (which-key-enable-god-mode-support t)
  :config
  (which-key-mode))

(use-package god-mode
  :after (evil evil-collection)
  :commands
  god-execute-with-current-bindings)

(use-package
  aggressive-indent
  :hook
  (elixir-mode-hook . aggressive-indent-mode)
  (rust-mode-hook . aggressive-indent-mode)
  (swift-mode-hook . aggressive-indent-mode))

;; Folding
(use-package origami)

;; Search
(setq isearch-lazy-count t)

(use-package comby)

;; Editor Config
(customize-set-variable 'editorconfig-mode t)

;; Structured editing
(use-package puni
  :defer t
  :config
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  :hook
  (term-mode-hook . puni-disable-puni-mode))

(use-package combobulate
  :ensure
  (:type git :host github :repo "mickeynp/combobulate" :branch "master")
  :hook
  (python-ts-mode . combobulate-mode)
  (js-ts-mode . combobulate-mode)
  (css-ts-mode . combobulate-mode)
  (yaml-ts-mode . combobulate-mode)
  (typescript-ts-mode . combobulate-mode)
  (tsx-ts-mode . combobulate-mode))

(use-package parinfer-rust-mode
  :diminish
  :hook emacs-lisp-mode)

;; Comments
;; Comment line or region
(global-set-key (kbd "s-/") 'comment-line)

(use-package embark
  :after (evil evil-collection avy)
  :commands (embark-act embark-dwim embark-bindings)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim))        ;; good alternative: M-.

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Override the Evil bindings
  (define-key evil-normal-state-map (kbd "C-.") 'embark-act)
  (define-key evil-normal-state-map (kbd "C-;") 'embark-dwim)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
 The which-key help message will show the type and value of the
 current target followed by an ellipsis if there are further
 targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

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
  (elixir-ts-mode . apheleia-mode)
  (swift-mode . apheleia-mode)
  (rust-mode . apheleia-mode)
  (rust-ts-mode . apheleia-mode)
  (python-mode . apheleia-mode)
  (python-ts-mode . apheleia-mode)
  (haskell-mode . apheleia-mode)
  (javascript-mode . apheleia-mode)
  (sass-mode . apheleia-mode)
  (typescript-mode . apheleia-mode)
  (typescript-ts-mode . apheleia-mode)
  (tsx-mode . apheleia-mode)
  (go-mode . apheleia-mode)
  (go-ts-mode . apheleia-mode)
  (ruby-mode . apheleia-mode)
  (ruby-ts-mode . apheleia-mode))

(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package re-builder
  :ensure nil
  :custom
  (reb-re-syntax 'string))

(provide 'editor)

;;; editor.el ends here
