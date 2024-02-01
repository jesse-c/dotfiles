;;; terminal.el --- -*- lexical-binding: t; -*-

(use-package eat
  :elpaca (:host codeberg
           :repo "akib/emacs-eat"
           :branch "master"
           :files ("*.el" ("term" "term/*.el") "*.texi"
                   "*.ti" ("terminfo/e" "terminfo/e/*")
                   ("terminfo/65" "terminfo/65/*")
                   ("integration" "integration/*")
                   (:exclude ".dir-locals.el" "*-tests.el")))
  :defer 1
  :hook
  (eshell-load-hook . eat-eshell-mode))

(use-package vterm)

(provide 'terminal)

;;; terminal.el ends here
