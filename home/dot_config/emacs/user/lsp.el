;;; lsp.el --- -*- lexical-binding: t; -*-

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-elixir-ls-version "v0.15.1")
  (lsp-elixir-local-server-command "/Users/jesse/src/github.com/elixir-lsp/elixir-ls/rel/language_server.sh")
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  ((lsp-completion-mode . my/lsp-mode-setup-completion)
   ;; Languages
   (elixir-mode . lsp)
   (elixir-ts-mode . lsp)
   (heex-ts-mode . lsp)
   (toml-mode . lsp)
   (xml-mode . lsp)
   (go-mode . lsp)
   (go-ts-mode . lsp)
   (bash-mode . lsp)
   (clojure-mode . lsp)
   (latex-mode . lsp)
   (lua-mode . lsp)
   (markdown-mode . lsp)
   (python-mode . lsp)
   (rust-mode . lsp)
   (ruby-mode . lsp)
   (sql-mode . lsp)
   (typescript-mode . lsp)
   (typescript-ts-mode . lsp)
   (tsx-mode . lsp)
   (tsx-ts-mode . lsp)
   (html-mode . lsp)
   (web-mode . lsp)
   (css-mode . lsp)
   (json-mode . lsp)
   (yaml-mode . lsp)
   (terraform-mode . lsp)
   (swift-mode . lsp)
   (rust-mode . lsp)
   (rust-ts-mode . lsp)
   (rustic-mode . lsp)
   (php-mode . lsp)
   ;; which-key integration
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (add-to-list 'lsp-disabled-clients 'credo-language-server))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package consult-lsp :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols))
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'lsp)

;;; lsp.el ends here