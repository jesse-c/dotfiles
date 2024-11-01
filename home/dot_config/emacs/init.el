;;; init.el --- My config -*- lexical-binding: t; -*-

;;; Commentary:

;;; My config.

;;; Code:

;;; Setup ----------------------------------------------------------------------

(defvar user-dir "user")

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file (expand-file-name (concat (file-name-as-directory user-dir) "custom.el") user-emacs-directory))
(load custom-file 'noerror)

;; Where the user config is kept
(add-to-list 'load-path (expand-file-name user-dir user-emacs-directory))

(require 'external)

;;; User -----------------------------------------------------------------------

(require 'my-server)
(require 'common)
(require 'ui)
(require 'os)
(require 'vcs)
(require 'editor)
(require 'lsp)
(require 'tree-sitter)
(require 'projects)
(require 'spelling)
(require 'modal)
(require 'notes)
(require 'completion)
(require 'file-system)
(require 'snippets)
(require 'terminal)
(require 'ai)
(require 'lang-all)
(require 'lang-http)
(require 'lang-rust)
(require 'lang-ruby)
(require 'lang-java)
(require 'lang-elixir)
(require 'lang-erlang)
(require 'lang-go)
(require 'lang-haskell)
(require 'lang-swift)
(require 'lang-just)
(require 'lang-json)
(require 'lang-jsonl)
(require 'lang-python)
(require 'lang-kotlin)
(require 'lang-protobuf)
(require 'lang-csv)
(require 'lang-markdown)
(require 'lang-yaml)
(require 'lang-hcl)
(require 'lang-terraform)
(require 'lang-graphql)
(require 'lang-lua)
(require 'lang-clojure)
(require 'lang-sass)
(require 'lang-javascript)
(require 'lang-typescript)
(require 'lang-docker)
(require 'lang-caddy)
(require 'lang-emacs-lisp)
(require 'lang-racket)
(require 'lang-nix)
(require 'lang-fish)
(require 'lang-epub)

;;; init.el ends here
