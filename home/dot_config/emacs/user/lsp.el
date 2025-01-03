;;; lsp.el --- -*- lexical-binding: t; -*-

(use-package lsp-mode
  :custom
  (lsp-elixir-local-server-command "~/.cache/elixir-ls/language_server.sh")
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  ((lsp-completion-mode . my/lsp-mode-setup-completion)
   ;; Languages
   (bash-mode . lsp-deferred)
   (clojure-mode . lsp-deferred)
   (css-mode . lsp-deferred)
   (elixir-mode . lsp-deferred)
   (elixir-ts-mode . lsp-deferred)
   (flix-mode . lsp-deferred)
   (go-mode . lsp-deferred)
   (go-ts-mode . lsp-deferred)
   (heex-ts-mode . lsp-deferred)
   (html-mode . lsp-deferred)
   (json-mode . lsp-deferred)
   (latex-mode . lsp-deferred)
   (lua-mode . lsp-deferred)
   (markdown-mode . lsp-deferred)
   (php-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (python-ts-mode . lsp-deferred)
   (ruby-mode . lsp-deferred)
   (ruby-ts-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (rust-ts-mode . lsp-deferred)
   (rust-ts-mode . lsp-deferred)
   (rustic-mode . lsp-deferred)
   (sql-mode . lsp-deferred)
   (swift-mode . lsp-deferred)
   (terraform-mode . lsp-deferred)
   (toml-mode . lsp-deferred)
   (tsx-mode . lsp-deferred)
   (tsx-ts-mode . lsp-deferred)
   (typescript-mode . lsp-deferred)
   (typescript-ts-mode . lsp-deferred)
   (web-mode . lsp-deferred)
   (xml-mode . lsp-deferred)
   (yaml-mode . lsp-deferred)
   ;; which-key integration
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (add-to-list 'lsp-disabled-clients 'credo-language-server)
  (add-to-list 'lsp-disabled-clients 'semgrep-ls)
  (add-to-list 'lsp-language-id-configuration '(protobuf-mode . "protobuf"))
  (add-to-list 'lsp-language-id-configuration '(protobuf-ts-mode . "protobuf"))
  (add-to-list 'lsp-language-id-configuration '(swift-mode . "swift"))
  (add-to-list 'lsp-language-id-configuration '(forge-post-mode-mode . "plaintext"))
  (add-to-list 'lsp-language-id-configuration '(flix-mode . "flix"))
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection '("bufls" "serve"))
                     :major-modes '(protobuf-mode protobuf-ts-mode)
                     :server-id 'protobuf-ls))
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection '("sourcekit-lsp"))
                     :major-modes '(swift-mode)
                     :server-id 'swift-ls))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("flix"
                                                            "lsp"
                                                            "6670"))
                    :major-modes '(flix-mode)
                    :server-id 'flix-ls)))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package consult-lsp :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols))
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(provide 'lsp)

;;; lsp.el ends here
