;;; lang-go.el --- -*- lexical-binding: t; -*-

(use-package go-mode)

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))

(provide 'lang-go)

;;; lang-go.el ends here
