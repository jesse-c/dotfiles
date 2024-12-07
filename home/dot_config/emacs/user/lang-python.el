;;; lang-python.el --- -*- lexical-binding: t; -*-

(use-package python-mode)

(use-package poetry)

(use-package pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

(provide 'lang-python)

;;; lang-python.el ends here
