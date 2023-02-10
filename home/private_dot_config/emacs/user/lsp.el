;;; lsp.el --- -*- lexical-binding: t; -*-

(use-package eglot
  :ensure nil
  :commands eglot eglot-ensure
  :bind (:map eglot-mode-map
	            ("C-c l r" . eglot-rename)
	            ("C-c l a" . eglot-code-actions)
	            ("C-c l k" . eldoc)
	            ("C-c l f" . eglot-format-buffer)
	            ("C-c l t r" . eglot-reconnect)
	            ("C-c l t s" . eglot-shutdown)
	            ("C-c l t S" . eglot-shutdown-all))
  :config
  ;; See: https://github.com/neovim/nvim-lspconfig/tree/89a19315ef4064a144b3d7d1c9a7eefd0e91e51b/lua/lspconfig/server_configurations
  (add-to-list 'eglot-server-programs
               `(toml-mode "taplo" "lsp" "stdio"))
  (add-to-list 'eglot-server-programs
               `(elixir-mode "elixir-ls"))
  (add-to-list 'eglot-server-programs
               `(xml-mode "lemminx"))
  (add-to-list 'eglot-server-programs
               `(go-mode "gopls"))
  (add-to-list 'eglot-server-programs
               `(bash-mode "bash-language-server" "start"))
  (add-to-list 'eglot-server-programs
               `(clojure-mode "clojure-lsp"))
  (add-to-list 'eglot-server-programs
               `(latex-mode "ltex-ls"))
  (add-to-list 'eglot-server-programs
               `(lua-mode "lua-language-server"))
  (add-to-list 'eglot-server-programs
               `(markdown-mode "prosemd-lsp" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(python-mode "pylsp"))
  (add-to-list 'eglot-server-programs
               `(rust-mode "rust-analyzer"))
  ;; And/Or Sorbet for Ruby?
  (add-to-list 'eglot-server-programs
               `(ruby-mode "solargraph" "stdio"))
  (add-to-list 'eglot-server-programs
               `(sql-mode "sqls"))
  (add-to-list 'eglot-server-programs
               `(typescript-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(typescript-ts-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(tsx-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(tsx-ts-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(html-mode "vscode-html-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(web-mode "vscode-html-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(css-mode "vscode-cs-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(json-mode "vscode-json-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(yaml-mode "yaml-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(terraform-mode "terraform-ls" "serve"))
  (add-to-list 'eglot-server-programs
               `(swift-mode "sourcekit-lsp"))
  :hook
  (toml-mode-hook . eglot-ensure)
  (elixir-mode-hook . eglot-ensure)
  (xml-mode-hook . eglot-ensure)
  (go-mode-hook . eglot-ensure)
  (bash-mode-hook . eglot-ensure)
  (clojure-mode-hook . eglot-ensure)
  (latex-mode-hook . eglot-ensure)
  (lua-mode-hook . eglot-ensure)
  (markdown-mode-hook . eglot-ensure)
  (python-mode-hook . eglot-ensure)
  (rust-mode-hook . eglot-ensure)
  (ruby-mode-hook . eglot-ensure)
  (sql-mode-hook . eglot-ensure)
  (typescript-mode-hook . eglot-ensure)
  (typescript-ts-mode-hook . eglot-ensure)
  (tsx-mode-hook . eglot-ensure)
  (tsx-ts-mode-hook . eglot-ensure)
  (html-mode-hook . eglot-ensure)
  (web-mode-hook . eglot-ensure)
  (css-mode-hook . eglot-ensure)
  (json-mode-hook . eglot-ensure)
  (yaml-mode-hook . eglot-ensure)
  (terraform-mode-hook . eglot-ensure)
  (swift-mode-hook . eglot-ensure)
	(rust-mode . eglot-ensure)
	(rustic-mode . eglot-ensure))


(use-package consult-eglot)

(provide 'lsp)

;;; lsp.el ends here
