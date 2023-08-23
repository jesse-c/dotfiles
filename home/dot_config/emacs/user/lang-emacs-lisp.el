;;; lang-emacs-lisp.el --- -*- lexical-binding: t; -*-

(use-package eldoc
  :diminish eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  (lisp-interaction-mode . turn-on-eldoc-mode))

(provide 'lang-emacs-lisp)

;;; lang-emacs-lisp.el ends here
