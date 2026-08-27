;;; my-agent-shell-patches-tests.el --- Tests for my-agent-shell-patches -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test' from the repository root.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'my-agent-shell-patches)

(defun my-agent-shell-patches-tests--target () 0)

(defun my-agent-shell-patches-tests--bump (orig &rest args)
  "Call ORIG with ARGS and add one, so double-application is visible."
  (1+ (apply orig args)))

(defconst my-agent-shell-patches-tests--present
  '((my-agent-shell-patches-tests--target
     . my-agent-shell-patches-tests--bump)))

(defconst my-agent-shell-patches-tests--gone
  '((my-agent-shell-patches-tests--no-such-function
     . my-agent-shell-patches-tests--bump)))

(defmacro my-agent-shell-patches-tests--with-reports (var &rest body)
  "Run BODY with the reporter stubbed, binding VAR to the (MESSAGE LEVEL) calls."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'my/agent-shell-patches-report)
                (lambda (message level) (push (list message level) ,var))))
       ,@body
       (setq ,var (nreverse ,var)))))

(defmacro my-agent-shell-patches-tests--with-warnings (var &rest body)
  "Run BODY with `display-warning' stubbed, binding VAR to the calls made."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (type message &optional level &rest _)
                  (push (list type message level) ,var))))
       ,@body
       (setq ,var (nreverse ,var)))))

;;; The benign-stderr filter.

(ert-deftest my-agent-shell-patches/drops-benign-stderr ()
  (dolist (message '("No onPostToolUseHook found"
                     "error: consuming background task result 42"))
    (should-not (my/acp-drop-benign-internal-errors
                 (lambda (&rest args) (cons :orig args)) message))))

(ert-deftest my-agent-shell-patches/reports-real-errors ()
  (should (equal (my/acp-drop-benign-internal-errors
                  (lambda (&rest args) (cons :orig args))
                  "genuine agent failure")
                 '(:orig "genuine agent failure"))))

(ert-deftest my-agent-shell-patches/reports-non-string-messages ()
  (should (equal (my/acp-drop-benign-internal-errors
                  (lambda (&rest args) (cons :orig args)) nil)
                 '(:orig nil))))

;; Self-recovery: upstream widening the signature must not signal
;; `wrong-number-of-arguments', it must just fall through to the original.
(ert-deftest my-agent-shell-patches/tolerates-extra-arguments ()
  (should (equal (my/acp-drop-benign-internal-errors
                  (lambda (&rest args) (cons :orig args))
                  "genuine agent failure" :code 42)
                 '(:orig "genuine agent failure" :code 42)))
  (should-not (my/acp-drop-benign-internal-errors
               (lambda (&rest args) (cons :orig args))
               "No onPostToolUseHook found" :code 42)))

;;; Surfacing swallowed handler errors.

(ert-deftest my-agent-shell-patches/passes-successful-handlers-through ()
  (should (equal (my/agent-shell-report-handler-errors #'list 1 2 3)
                 '(1 2 3))))

;; This one reports immediately, not through the deferred reporter: it fires
;; mid-tool-call, when you're already waiting on the shell.
(ert-deftest my-agent-shell-patches/warns-on-handler-failure-immediately ()
  (my-agent-shell-patches-tests--with-warnings warnings
    (should-not (my/agent-shell-report-handler-errors
                 (lambda (&rest _) (error "boom"))))
    (should (equal (length warnings) 1))
    (should (equal (nth 0 (car warnings)) 'agent-shell))
    (should (string-match-p "handler failed" (nth 1 (car warnings))))
    (should (equal (nth 2 (car warnings)) :error))))

;;; Reporting.

;; agent-shell is `:defer 1', so a report emitted at install time lands in the
;; middle of startup and gets displaced.  It has to wait for idle.
(ert-deftest my-agent-shell-patches/report-waits-for-idle ()
  (let (scheduled thunk warnings messages)
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (secs repeat fn)
                 (setq scheduled (list secs repeat) thunk fn)))
              ((symbol-function 'display-warning)
               (lambda (type message &optional level &rest _)
                 (push (list type message level) warnings)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (my/agent-shell-patches-report "headline\n\ndetail" :warning)
      ;; Scheduled once, and nothing said yet.
      (should (equal scheduled '(1 nil)))
      (should-not warnings)
      (should-not messages)
      (funcall thunk)
      ;; Durable record in *Warnings*, with the full text.
      (should (equal warnings
                     '((my/agent-shell-patches "headline\n\ndetail" :warning))))
      ;; Echo area gets the headline only, so it fits.
      (should (equal messages '("headline"))))))

;; The echo area only ever gets the first line, so the names have to be on it.
;; Wrapping them onto a later line made the echo report stop at a colon.
(ert-deftest my-agent-shell-patches/missing-message-names-targets-on-first-line ()
  (let* ((message (my/agent-shell-patches--missing-message '(foo-fn bar-fn)))
         (headline (car (split-string message "\n"))))
    (should (string-match-p "foo-fn" headline))
    (should (string-match-p "bar-fn" headline))
    (should (string-match-p "agent-shell" headline))
    ;; The detail still lands in *Warnings*.
    (should (string-match-p "not running" message))))

;;; Installing.

(ert-deftest my-agent-shell-patches/advises-present-targets ()
  (unwind-protect
      (progn
        (should (equal (my/agent-shell-patches-install
                        my-agent-shell-patches-tests--present)
                       '(my-agent-shell-patches-tests--target)))
        (should (equal (my-agent-shell-patches-tests--target) 1)))
    (my/agent-shell-patches-remove my-agent-shell-patches-tests--present))
  (should (equal (my-agent-shell-patches-tests--target) 0)))

;; `with-eval-after-load' runs again on every load of the feature, so a
;; second install must not stack a second copy of the advice.
(ert-deftest my-agent-shell-patches/install-is-idempotent ()
  (unwind-protect
      (progn
        (dotimes (_ 3)
          (my/agent-shell-patches-install
           my-agent-shell-patches-tests--present))
        (should (equal (my-agent-shell-patches-tests--target) 1)))
    (my/agent-shell-patches-remove my-agent-shell-patches-tests--present)))

;; Self-recovery: `advice-add' on a void symbol neither signals nor defines
;; the symbol, so a rename upstream would drop the patch with no diagnostics
;; whatsoever.  The install has to say so.
(ert-deftest my-agent-shell-patches/reports-when-target-is-gone ()
  (my-agent-shell-patches-tests--with-reports reports
    (should-not (my/agent-shell-patches-install
                 my-agent-shell-patches-tests--gone))
    (should (equal (length reports) 1))
    (should (string-match-p "no longer apply" (nth 0 (car reports))))
    (should (string-match-p "my-agent-shell-patches-tests--no-such-function"
                            (nth 0 (car reports))))
    (should (equal (nth 1 (car reports)) :warning)))
  (should-not (fboundp 'my-agent-shell-patches-tests--no-such-function)))

(ert-deftest my-agent-shell-patches/installs-what-it-can-and-reports-the-rest ()
  (my-agent-shell-patches-tests--with-reports reports
    (unwind-protect
        (should (equal (my/agent-shell-patches-install
                        (append my-agent-shell-patches-tests--present
                                my-agent-shell-patches-tests--gone))
                       '(my-agent-shell-patches-tests--target)))
      (my/agent-shell-patches-remove my-agent-shell-patches-tests--present))
    (should (equal (length reports) 1))))

;; Self-recovery: a patch that can no longer apply must not stop the feature
;; it hangs off from loading.
(ert-deftest my-agent-shell-patches/reports-rather-than-signalling ()
  (my-agent-shell-patches-tests--with-reports reports
    (should-not (my/agent-shell-patches-install 'not-a-patch-list))
    (should (equal (length reports) 1))
    (should (string-match-p "failed to install" (nth 0 (car reports))))
    (should (equal (nth 1 (car reports)) :error))))

(ert-deftest my-agent-shell-patches/remove-tolerates-missing-targets ()
  (should (equal (my/agent-shell-patches-remove
                  my-agent-shell-patches-tests--gone)
                 '(my-agent-shell-patches-tests--no-such-function))))

(provide 'my-agent-shell-patches-tests)
;;; my-agent-shell-patches-tests.el ends here
