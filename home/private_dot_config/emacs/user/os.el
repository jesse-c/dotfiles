;;; os.el --- -*- lexical-binding: t; -*-

;; All

(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)

(setq auth-sources '("~/.authinfo"))

;; Tidy up .emacs.d mess
(use-package no-littering)
(savehist-mode)

;; Disabling suspend-frame binding
;; Very annoying binding, lets get rid of it.
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#disabling-suspend-frame-binding
(global-unset-key (kbd "C-z"))

(use-package chezmoi
  :after magit)

;; macOS

(when (memq window-system '(mac ns))
  (setq dired-use-ls-dired nil))

(setq delete-by-moving-to-trash t)

;; https://emacs.stackexchange.com/a/41767
;; Alternative to try: https://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
(setq use-dialog-box nil)

;; Ensure PATH is correct when launched as GUI application
(use-package
  exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

(when (memq window-system '(mac ns))
  (setq dired-use-ls-dired nil))

;; https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
(defun my/file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

(provide 'os)

;;; os.el ends here
