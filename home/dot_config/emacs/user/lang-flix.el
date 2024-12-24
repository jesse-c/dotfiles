;;; lang-flix.el --- -*- lexical-binding: t; -*-

;; Define flix mode
(define-derived-mode flix-mode prog-mode "Flix"
  "Major mode for editing Flix source code."
  :group 'flix
  (setq-local comment-start "//")
  (setq-local comment-end ""))

;; Associate .flix files with flix-mode
(add-to-list 'auto-mode-alist '("\\.flix\\'" . flix-mode))

;; Provide the feature
(provide 'lang-flix)
