;;; my-eglot-startup-tests.el --- Deferred Eglot startup tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'subr-x)

;; Read the real definitions without loading Eglot or launching a server.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../home/dot_config/emacs/init.el"
                     (file-name-directory load-file-name)))
  (search-forward "(use-package eglot\n")
  (goto-char (match-beginning 0))
  (let ((config (cdr (memq :config (read (current-buffer))))))
    (dolist (form config)
      (when (and (memq (car-safe form) '(defun defvar-local))
                 (memq (cadr form) '(my/eglot-ensure-deferred my/eglot-ensure-timer)))
        (eval form t)))))

(ert-deftest my-startup/eglot-timer-keeps-origin-buffer ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/startup.ts")
    (let ((origin (current-buffer)) callback ensured)
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (_delay _repeat fn &rest _) (setq callback fn)))
                ((symbol-function 'eglot-ensure)
                 (lambda () (setq ensured (current-buffer)))))
        (my/eglot-ensure-deferred)
        (with-temp-buffer
          (setq buffer-file-name "/tmp/not-the-origin.org")
          (funcall callback))
        (should (eq ensured origin))))))

(ert-deftest my-startup/eglot-timer-ignores-killed-buffer ()
  (let ((buffer (generate-new-buffer " *startup-eglot*")) callback)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_delay _repeat fn &rest _) (setq callback fn)))
              ((symbol-function 'eglot-ensure)
               (lambda () (ert-fail "Must not run in another buffer"))))
      (with-current-buffer buffer
        (setq buffer-file-name "/tmp/startup.ts")
        (my/eglot-ensure-deferred))
      (kill-buffer buffer)
      (funcall callback))))

(ert-deftest my-startup/eglot-timer-ignores-changed-mode ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/startup.ts")
    (let (callback)
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (_delay _repeat fn &rest _) (setq callback fn)))
                ((symbol-function 'eglot-ensure)
                 (lambda () (ert-fail "Must not start for obsolete major mode"))))
        (my/eglot-ensure-deferred)
        (setq major-mode 'special-mode)
        (funcall callback)))))

(ert-deftest my-startup/eglot-non-file-buffer-does-not-schedule ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (ert-fail "Must not schedule for a temporary buffer"))))
      (my/eglot-ensure-deferred))))

(ert-deftest my-startup/eglot-repeated-hook-cancels-prior-timer ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/startup.ts")
    (let ((timer (timer-create)) cancelled)
      (cl-letf (((symbol-function 'run-with-idle-timer) (lambda (&rest _) timer))
                ((symbol-function 'cancel-timer) (lambda (old) (setq cancelled old))))
        (my/eglot-ensure-deferred)
        (my/eglot-ensure-deferred)
        (should (eq cancelled timer))))))

(provide 'my-eglot-startup-tests)
;;; my-eglot-startup-tests.el ends here
