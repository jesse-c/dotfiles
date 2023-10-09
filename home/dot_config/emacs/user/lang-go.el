;;; lang-go.el --- -*- lexical-binding: t; -*-

(use-package go-mode)

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package gotest)

(use-package go-gen-test)

(use-package go-tag)

(use-package gorepl-mode
  :hook (go-mode . gorepl-mode))

(provide 'lang-go)

;;; lang-go.el ends here
