;;; lang-clojure.el --- -*- lexical-binding: t; -*-

(use-package clojure-mode)
(use-package clojure-snippets)

(use-package cider
  :defer t
  :config
  (setq nrepl-log-messages t)
  (flycheck-clojure-setup)) ;; run setup *after* cider load

(use-package flycheck-clojure
  :defer t
  :commands (flycheck-clojure-setup) ;; autoload
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'lang-clojure)

;;; lang-clojure.el ends here
