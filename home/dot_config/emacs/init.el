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
;; Languages
(require 'lang-all)
(require 'lang-caddy)
(require 'lang-clojure)
(require 'lang-csv)
(require 'lang-docker)
(require 'lang-elixir)
(require 'lang-emacs-lisp)
(require 'lang-epub)
(require 'lang-erlang)
(require 'lang-fish)
(require 'lang-flix)
(require 'lang-go)
(require 'lang-graphql)
(require 'lang-haskell)
(require 'lang-hcl)
(require 'lang-http)
(require 'lang-java)
(require 'lang-javascript)
(require 'lang-json)
(require 'lang-jsonl)
(require 'lang-just)
(require 'lang-kotlin)
(require 'lang-lua)
(require 'lang-markdown)
(require 'lang-nix)
(require 'lang-protobuf)
(require 'lang-python)
(require 'lang-racket)
(require 'lang-ruby)
(require 'lang-rust)
(require 'lang-sass)
(require 'lang-swift)
(require 'lang-terraform)
(require 'lang-typescript)
(require 'lang-yaml)

;;; init.el ends here
