;;; my-easysession-agent-shell-tests.el --- Agent restore tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'subr-x)

;; Evaluate the real integration with no agent-shell loaded.  Registration
;; must not depend on its deferred load or start any processes.
(defvar my-easysession-agent-shell-tests--managed-modes nil)
(let ((features (remq 'agent-shell features)))
  (cl-letf (((symbol-function 'easysession-add-managed-major-mode)
             (lambda (mode &rest props)
               (push (cons mode props) my-easysession-agent-shell-tests--managed-modes))))
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "../home/dot_config/emacs/init.el"
                         (file-name-directory load-file-name)))
      (search-forward ";; BEGIN my/easysession-agent-shell integration")
      (forward-line 1)
      (let ((end (save-excursion
                   (search-forward ";; END my/easysession-agent-shell integration")
                   (line-beginning-position))))
        (while (< (point) end)
          (forward-comment (point-max))
          (when (< (point) end)
            (eval (read (current-buffer)) t)))))))

(defvar agent-shell--state nil)

(ert-deftest my-startup/agent-handlers-registered-before-agent-load ()
  (dolist (mode '(agent-shell-mode my/easysession-agent-shell-unavailable-mode))
    (let ((props (cdr (assq mode my-easysession-agent-shell-tests--managed-modes))))
      (should (eq (plist-get props :save) #'my/easysession-agent-shell--save))
      (should (eq (plist-get props :restore) #'my/easysession-agent-shell--restore)))))

(defun my-easysession-agent-shell-tests--state ()
  (copy-tree
   `((buffer-name . " *startup-agent-test*")
     (major-mode . agent-shell-mode)
     (default-directory . ,temporary-file-directory)
     (data (config-id . pi) (session-id . "saved-session")))))

(ert-deftest my-startup/agent-restore-loads-package-and-resumes-explicit-id ()
  (let ((state (my-easysession-agent-shell-tests--state))
        (buffer (generate-new-buffer " *startup-restored*"))
        required started callback
        (require-original (symbol-function 'require)))
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &rest args)
                     (if (eq feature 'agent-shell) (setq required t)
                       (apply require-original feature args))))
                  ((symbol-function 'agent-shell--resolve-config-designator)
                   (lambda (id)
                     (should required)
                     (should (eq id 'pi))
                     '((:identifier . pi))))
                  ((symbol-function 'my/easysession-agent-shell--existing) #'ignore)
                  ((symbol-function 'agent-shell--start)
                   (lambda (&rest args)
                     (setq started args)
                     (with-current-buffer buffer (setq major-mode 'agent-shell-mode))
                     buffer))
                  ((symbol-function 'shell-maker-set-buffer-name)
                   (lambda (b name) (with-current-buffer b (rename-buffer name))))
                  ((symbol-function 'agent-shell-initialized-p) #'ignore)
                  ((symbol-function 'agent-shell-subscribe-to)
                   (lambda (&rest args) (setq callback (plist-get args :on-event)))))
          (should (eq buffer (my/easysession-agent-shell--restore state)))
          (should (equal (plist-get started :session-id) "saved-session"))
          (should (equal (plist-get started :cwd) temporary-file-directory))
          (should (plist-get started :no-focus))
          (should (plist-get started :new-session))
          (should (eq (plist-get started :session-strategy) 'new))
          (with-current-buffer buffer
            (should (equal (my/easysession-agent-shell--save) (alist-get 'data state))))
          (funcall callback nil)
          (with-current-buffer buffer
            (should-not my/easysession-agent-shell--saved-state)))
      (kill-buffer buffer))))

(ert-deftest my-startup/agent-missing-id-retained-without-new-conversation ()
  (let ((state (my-easysession-agent-shell-tests--state)) buffer
        (original-require (symbol-function 'require)))
    (setf (alist-get 'session-id (alist-get 'data state)) nil)
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &rest args)
                     (unless (eq feature 'agent-shell)
                       (apply original-require feature args))))
                  ((symbol-function 'agent-shell--start)
                   (lambda (&rest _) (ert-fail "Must not start a new conversation"))))
          (setq buffer (my/easysession-agent-shell--restore state))
          (with-current-buffer buffer
            (should (derived-mode-p 'my/easysession-agent-shell-unavailable-mode))
            (should (equal (my/easysession-agent-shell--save) (alist-get 'data state)))))
      (when buffer (kill-buffer buffer)))))

(ert-deftest my-startup/agent-remote-directory-does-not-start-client ()
  (let ((state (my-easysession-agent-shell-tests--state)) buffer
        (original-require (symbol-function 'require)))
    (setf (alist-get 'default-directory state) "/ssh:unavailable:/project/")
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &rest args)
                     (unless (eq feature 'agent-shell)
                       (apply original-require feature args))))
                  ((symbol-function 'agent-shell--start)
                   (lambda (&rest _) (ert-fail "Must not start remote client"))))
          (setq buffer (my/easysession-agent-shell--restore state))
          (with-current-buffer buffer
            (should (derived-mode-p 'my/easysession-agent-shell-unavailable-mode))
            (should (equal (my/easysession-agent-shell--save) (alist-get 'data state)))))
      (when buffer (kill-buffer buffer)))))

(ert-deftest my-startup/agent-unsupported-resume-retains-placeholder ()
  (with-temp-buffer
    (let ((my/easysession-agent-shell--starting-state (my-easysession-agent-shell-tests--state))
          (agent-shell--state nil))
      (my/easysession-agent-shell--initiate-session
       (lambda (&rest _) (ert-fail "Must not create an unsupported session")))
      (should (derived-mode-p 'my/easysession-agent-shell-unavailable-mode))
      (should (equal (my/easysession-agent-shell--save)
                     (alist-get 'data my/easysession-agent-shell--starting-state))))))

(ert-deftest my-startup/agent-interactive-session-unaffected ()
  (with-temp-buffer
    (let (called)
      (my/easysession-agent-shell--initiate-session
       (lambda (&rest _) (setq called t)))
      (should called))))

(ert-deftest my-startup/agent-resume-failure-retains-id ()
  (with-temp-buffer
    (setq-local my/easysession-agent-shell--saved-state (my-easysession-agent-shell-tests--state))
    (let* ((args (my/easysession-agent-shell--quiet-resume-failure
                  (list :buffer (current-buffer)
                        :request '((:method . "session/resume"))
                        :on-failure (lambda (&rest _) (ert-fail "Must not open picker")))))
           (callback (plist-get args :on-failure)))
      (funcall callback '((message . "Session not found")) nil)
      (should (derived-mode-p 'my/easysession-agent-shell-unavailable-mode))
      (should (equal (alist-get 'session-id (my/easysession-agent-shell--save))
                     "saved-session")))))

(provide 'my-easysession-agent-shell-tests)
;;; my-easysession-agent-shell-tests.el ends here
