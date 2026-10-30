;;; my-org-startup-tests.el --- Org-roam startup ordering tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(defconst my-org-startup-tests--init
  (expand-file-name "../home/dot_config/emacs/init.el"
                    (file-name-directory load-file-name)))

(ert-deftest my-startup/org-hook-loads-helpers-before-installing-them ()
  ;; Run the real :hook callback while the helper definitions are absent.
  ;; Simulate the deferred :config being evaluated by require, just as
  ;; use-package does; no Org-roam database or UI is needed here.
  (let* ((package
          (with-temp-buffer
            (insert-file-contents my-org-startup-tests--init)
            (search-forward "(use-package org-roam\n")
            (goto-char (match-beginning 0))
            (read (current-buffer))))
         (config (cdr (memq :config package)))
         (hooks (cdr (memq :hook package)))
         (hook (cdr (assq 'org-mode hooks)))
         (helpers '(my/org-roam-update-timestamps my/org-roam-dailies-ensure-heading-ids
                    my/org-roam-eldoc-link my/org-eldoc-footnote))
         (original-require (symbol-function 'require))
         loaded)
    (with-temp-buffer
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &rest args)
                   (if (eq feature 'org-roam)
                       (progn
                         (dolist (form config)
                           (when (and (eq (car-safe form) 'defun)
                                      (memq (cadr form) helpers))
                             (eval form t)))
                         (setq loaded t))
                     (apply original-require feature args))))
                ((symbol-function 'eldoc-box-hover-at-point-mode)
                 (lambda (&rest _)
                   (should loaded)
                   (dolist (helper helpers) (should (fboundp helper))))))
        (dolist (helper helpers) (fmakunbound helper))
        (funcall hook)
        (should (memq #'my/org-eldoc-footnote eldoc-documentation-functions))
        (should (memq #'my/org-roam-update-timestamps before-save-hook))))))

(provide 'my-org-startup-tests)
;;; my-org-startup-tests.el ends here
