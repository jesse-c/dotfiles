;;; lang-clojure.el --- -*- lexical-binding: t; -*-

(use-package clojure-mode)

(use-package clojure-ts-mode
  :ensure
  (:type git :host github :repo "clojure-emacs/clojure-ts-mode" :branch "main"))

(use-package clojure-snippets)

(use-package cider
  :after transient
  :defer t
  :config
  (setq nrepl-log-messages t)
  (flycheck-clojure-setup)) ;; run setup *after* cider load

(use-package flycheck-clojure
  :after transient
  :defer t
  :commands (flycheck-clojure-setup) ;; autoload
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'lang-clojure)

;;; lang-clojure.el ends here
