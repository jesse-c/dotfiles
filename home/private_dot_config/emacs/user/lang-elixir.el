;;; lang-elixir.el --- -*- lexical-binding: t; -*-

;; Using elixir-ts-mode over elixir-mode
;; (use-package elixir-mode)

(use-package elixir-ts-mode)

(use-package heex-ts-mode)

(use-package apprentice
  :elpaca
  (:type git :host github :repo "Sasanidas/Apprentice" :branch "master"))

(use-package mix
  :hook
  (elixir-mode-hook . mix-minor-mode))

(use-package exunit
  :hook
  (elixir-mode-hook . exunit-mode))

(use-package flycheck-credo
  :defer 1
  :init
  '(flycheck-credo-setup)
  :config
  (setq flycheck-elixir-credo-strict t))

(use-package flycheck-elixir
  :after (flycheck elixir-mode)
  :hook
  (elixir-mode-hook . flycheck-mode))

(use-package flycheck-dialyxir
  :after (flycheck elixir-mode)
  :hook
  (elixir-mode-hook . flycheck-mode))

(use-package flycheck-dialyzer
  :after (flycheck elixir-mode)
  :hook
  (elixir-mode-hook . flycheck-mode))

(defun my/strip-ecto-field ()
  "Strip Elixir Ecto field syntax."
  (interactive)
  (when (use-region-p)
    (let ((region-start (region-beginning))
          (region-end (region-end)))
      (save-restriction
        (narrow-to-region region-start region-end)
        (goto-char (point-min))
        (while (re-search-forward "field\(\\(:[a-z_?]+\\),.*\)" nil t)
          (replace-match "\\1" nil nil))))))

(provide 'lang-elixir)

;;; lang-elixir.el ends here
