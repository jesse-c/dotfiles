;;; lang-sd.el --- -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.sd\\'" . sd-mode))
(add-hook 'sd-mode-hook
          (lambda ()
            (when (string-match "\\.sd\\'" (buffer-file-name))
              (setq indent-tabs-mode nil)
              (setq tab-width 2))))

(provide 'lang-sd)

;;; lang-sd.el ends here
