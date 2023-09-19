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
         (cmd (format "alt -t 10 %s" buffile))
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

(use-package ox-pandoc)

(provide 'lang-all)

;;; lang-all.el ends here
