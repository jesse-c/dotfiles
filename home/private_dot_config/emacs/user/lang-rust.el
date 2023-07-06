;;; lang-rust.el --- -*- lexical-binding: t; -*-

(use-package rust-mode)
(use-package rustic
  :custom
  (rustic-lsp-client 'lsp-mode))
(use-package cargo
  :hook
  (rust-mode-hook . cargo-minor-mode))
(use-package flycheck-rust
  :after (flycheck rust-mode)
  :hook
  (flycheck-mode-hook . flycheck-rust-setup))

(provide 'lang-rust)

;;; lang-rust.el ends here
