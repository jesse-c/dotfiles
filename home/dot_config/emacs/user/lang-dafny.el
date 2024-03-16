;;; lang-dafny.el --- -*- lexical-binding: t; -*-

(use-package boogie-friends
  :ensure
  (:type git :host github :repo "boogie-org/boogie-friends" :branch "master")
  :mode ("\\.dfy\\'" . dafny-mode)
  :custom
  (dafny-verification-backend 'server)
  (flycheck-dafny-executable "dafny")
  (flycheck-z3-executable "z3")
  (flycheck-inferior-dafny-executable "dafny"))

(provide 'lang-dafny)

;;; lang-dafny.el ends here
