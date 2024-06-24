;;; lang-kotlin.el --- -*- lexical-binding: t; -*-

(use-package kotlin-mode)

(use-package flycheck-kotlin
  :defer t
  :config
  (flycheck-kotlin-setup))

(use-package kotlin-ts-mode
  :ensure
  (:type git :host gitlab :repo "bricka/emacs-kotlin-ts-mode" :branch "main")
  :mode "\\.kt\\'")

(use-package ob-kotlin
  :after (kotlin org))

(provide 'lang-kotlin)

;;; lang-kotlin.el ends here
