;;; lang-haskell.el --- -*- lexical-binding: t; -*-

(use-package haskell-mode
  :hook (haskell-mode . flycheck-haskell-setup))

(use-package flycheck-haskell)

(provide 'lang-haskell)

;;; lang-haskell.el ends here
