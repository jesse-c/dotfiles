;;; lang-all.el --- -*- lexical-binding: t; -*-

(defun my/find-alternate-file ()
  "Find alternate FILE, if any, and open it."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         ;; https://emacs.stackexchange.com/questions/45419/get-file-name-relative-to-projectile-root
         (buffile (file-relative-name buffer-file-name (projectile-project-root)))
         (cmd (format "alt -t 1 %s" buffile))
         (output (shell-command-to-string cmd)))
    (if (string= output "")
        (message "No alternate file found")
      (if (y-or-n-p (format "Found alternate file %s. Open?" output))
          (find-file output (message "Not opening"))))))

(defun my/find-alternate-files ()
  "Find alternate files, if any, and open one."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         ;; https://emacs.stackexchange.com/questions/45419/get-file-name-relative-to-projectile-root
         (buffile (file-relative-name buffer-file-name (projectile-project-root)))
         (cmd (format "alt %s" buffile))
         (output (shell-command-to-string cmd))
         (file-list (split-string output)))
    (when file-list
      (let ((chosen-file (completing-read "Choose an alternate file: " file-list)))
        (find-file chosen-file)))))

(use-package quickrun)

;; Syntax
(use-package flycheck
  :init (global-flycheck-mode))

;; Debugging
(use-package dap-mode)

;; Testing
(use-package coverlay
  :defer t)

;; Snippets
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind
  ("M-+" . tempel-complete) ;; Alternative tempel-expand
  ("M-*" . tempel-insert)

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;; Optionally make the Tempel templates available to Abbrev,
;; either locally or globally. `expand-abbrev' is bound to C-x '.
;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;; (global-tempel-abbrev-mode)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

(provide 'lang-all)

;;; lang-all.el ends here
