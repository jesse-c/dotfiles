;;; modal.el --- -*- lexical-binding: t; -*-

;; Vim
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)

(use-package evil
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-mode 1)
  (global-unset-key (kbd "C-u"))
  (global-set-key (kbd "C-u") 'evil-scroll-up))

(use-package evil-visual-mark-mode
  :after evil
  :defer 1
  :config
  (evil-visual-mark-mode))

(use-package evil-collection
  :after evil
  :defer 1
  :config
  (evil-collection-init))

(provide 'modal)

;;; modal.el ends here
