;;; lang-emacs-lisp.el --- -*- lexical-binding: t; -*-

(use-package eldoc
  :diminish eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  (lisp-interaction-mode . turn-on-eldoc-mode))
(use-package elsa
  :defer 1)
(use-package flycheck-elsa
  :after (elsa flycheck)
  :defer 1
  :hook
  (emacs-lisp-mode . flycheck-elsa-setup))

(provide 'lang-emacs-lisp)

;;; lang-emacs-lisp.el ends here
