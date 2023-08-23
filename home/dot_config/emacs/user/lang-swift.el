;;; lang-swift.el --- -*- lexical-binding: t; -*-

(use-package swift-mode)
(use-package flycheck-swift
  :defer 1
  :after (swift-mode))

(provide 'lang-swift)

;;; lang-swift.el ends here
