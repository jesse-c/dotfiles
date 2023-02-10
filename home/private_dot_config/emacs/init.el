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

(require 'package)

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
(require 'completion)
(require 'file-system)
(require 'lang-all)
(require 'lang-rust)
(require 'lang-java)
(require 'lang-elixir)
(require 'lang-go)
(require 'lang-swift)
(require 'lang-json)
(require 'lang-python)
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

;;; init.el ends here
