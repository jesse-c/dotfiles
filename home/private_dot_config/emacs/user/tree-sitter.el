;;; tree-sitter.el --- -*- lexical-binding: t; -*-

(use-package
  treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(provide 'tree-sitter)

;;; tree-sitter.el ends here
