;;; my-agent-shell-patches.el --- Temporary agent-shell/acp workarounds -*- lexical-binding: t; -*-

;;; Commentary:

;; Local patches for upstream bugs, plus enough machinery that they fail
;; loudly and harmlessly when an update moves the ground under them.
;;
;;   1. acp surfaces benign agent stderr (orphaned tool results after a
;;      cancel, background task bookkeeping) as "Notices" error blocks.
;;      See org-roam node 20260812095756-agent-shell-cancel-notices-bug.
;;   2. acp wraps its request and notification handlers in
;;      `condition-case-unless-debug' and logs the failure to a buffer that's
;;      disabled by default, so a broken handler hangs the tool call with no
;;      diagnostics at all.
;;   3. A `session/prompt' request can vanish with no trace: the agent
;;      process stays alive, but no success, no error, and no streaming
;;      notification ever arrives for it.  `acp--fail-pending-requests'
;;      only fires on process exit, so a request the process just never
;;      answers sits in `:pending-requests' forever with nothing to see
;;      anywhere: not the transcript, not `acp-client-stderr', not
;;      `*Messages*'.  This does NOT diagnose a cause (earlier wording here
;;      guessed "ZDR policy violation" -- an explicit rejection that
;;      OpenRouter returns as an error, which doesn't match total silence
;;      with empty stderr).  It just makes sure the next occurrence has a
;;      wire-level trail: it turns on `acp-logging-enabled' and points at
;;      the traffic buffer to inspect.
;;
;; Both are `advice-add', not `fset'.  Advice is idempotent, `advice-remove'
;; undoes it, and it survives the target being redefined by a package reload.
;; The `fset' pair this replaced saved the *patched* function as the original
;; on its second run, so it called itself and any genuine error hit
;; `excessive-lisp-nesting' instead of being reported.
;;
;; These pin themselves to internal functions upstream is free to rename, so:
;;
;;   - A missing target is reported, to `*Warnings*' and the echo area, once
;;     Emacs is idle so startup can't bury it.  `advice-add' on a void symbol
;;     neither signals nor defines the symbol, so without the `fboundp' check
;;     a rename drops the patch silently and takes the workaround with it.
;;   - Every advice takes `&rest args' and inspects only what it needs, so
;;     upstream widening a signature doesn't give `wrong-number-of-arguments'.
;;   - Installing is wrapped, so a patch that can no longer apply can't stop
;;     acp or agent-shell from loading.
;;
;; The predicates are pure; installing and warning are the effects.
;;
;; Tests: tests/my-agent-shell-patches-tests.el (make test).

;;; Code:

(require 'map)
(require 'rx)
(require 'seq)

(declare-function acp--traffic-buffer-name "acp")

;; Scoped to just these two types, so it doesn't touch `*Warnings*' for
;; anything else: agent-shell errors still get logged there (durable,
;; `M-x view-buffer *Warnings*' finds them), they just don't yank the
;; window open at the bottom every time one fires.
(dolist (type '((agent-shell) (my/agent-shell-patches)))
  (add-to-list 'warning-suppress-types type))

(defconst my/acp-benign-stderr-re
  (rx (or "No onPostToolUseHook found"
          "consuming background task result"))
  "Agent stderr noise that isn't worth surfacing as an error.")

(defun my/acp-benign-message-p (message)
  "Return non-nil when MESSAGE is stderr noise rather than a real failure."
  (and (stringp message)
       (string-match-p my/acp-benign-stderr-re message)))

(defun my/acp-drop-benign-internal-errors (orig &rest args)
  "Return nil for a benign message, else call ORIG with ARGS.
ARGS rather than a lone MESSAGE so that upstream taking more arguments
degrades to ORIG instead of signalling."
  (unless (my/acp-benign-message-p (car args))
    (apply orig args)))

(defun my/agent-shell-patches-report-now (type message level)
  "Report MESSAGE at LEVEL immediately, unlike `my/agent-shell-patches-report'.
Used where the report is tied to a specific in-flight event (a handler
failing, a prompt going silent) and waiting for idle would put the echo
area message long after the moment it's about.

Still calls `display-warning' with TYPE so the report is durably logged
in `*Warnings*', but `warning-suppress-types' keeps that from popping the
window open.  The `message' call is what actually lands MESSAGE in
`*Messages*', which is where these are meant to be seen."
  (display-warning type message level)
  (message "%s" (car (split-string message "\n" t))))

(defun my/agent-shell-report-handler-errors (orig &rest args)
  "Call ORIG with ARGS, surfacing any error rather than letting acp swallow it.
nb: this makes failures visible, not harmless.  The handler still aborts,
so the call still hangs; actually responding to the request is upstream's
job.

Reports immediately rather than through `my/agent-shell-patches-report':
this fires mid-tool-call, when you're already watching the shell wondering
why nothing is happening, so waiting for idle would be backwards."
  (condition-case err
      (apply orig args)
    (error
     (my/agent-shell-patches-report-now
      'agent-shell (format "handler failed: %S" err) :error)
     nil)))

(defconst my/acp-silent-prompt-timeout-seconds 45
  "Seconds to wait for any response to a session/prompt before warning.")

(defvar my/acp-silent-prompt-alert-enabled nil
  "Non-nil to warn when a session/prompt gets no response at all.
Set to nil to mute `my/acp-warn-on-silent-prompt' without removing its
advice.  Checked when the timeout fires, not when the prompt is sent, so
toggling takes effect immediately even for a prompt already in flight.")

(when (boundp 'acp-logging-enabled)
  ;; Off by default upstream, and there's no reconstructing wire traffic
  ;; after the fact: the two silent prompts that prompted this patch left
  ;; nothing to inspect because logging wasn't on yet.  Turning it on is
  ;; what makes the warning below actionable instead of a shrug -- the next
  ;; occurrence gets an actual JSON-RPC trail in the traffic buffer instead
  ;; of a guess at the cause.
  (setq acp-logging-enabled t))

(defun my/acp-warn-on-silent-prompt (orig &rest args)
  "Call ORIG with ARGS, warning if a session/prompt gets no response at all.
ARGS is the plist `acp--request-sender' is invoked with (:client :request
:buffer :on-success :on-failure :sync), which is how the target's own
callers pass it, so reading it back with `plist-get' round-trips cleanly.

Schedules a check `my/acp-silent-prompt-timeout-seconds' out; if the
request is still in `:pending-requests' by then, neither success nor
failure has arrived, so warn.  Harmless if the turn was merely slow: by
the time the timer fires, a genuine response or `acp--fail-pending-requests'
\(process exit\) has already removed the entry.

Deliberately doesn't guess a cause.  Points at the traffic buffer instead,
which -- with `acp-logging-enabled' now on -- has the actual incoming and
outgoing JSON-RPC for this request: whether the CLI process ever wrote an
HTTP request, and what came back, if anything.  That answers what a
timestamp and a hunch can't; cross-check the same window against the
provider's own request log (e.g. https://openrouter.ai/activity), since
an explicit policy rejection (ZDR or otherwise) shows up there even when
the client sees nothing at all."
  (let ((result (apply orig args)))
    (when-let* ((client (plist-get args :client))
                (request (plist-get args :request))
                ((equal (map-elt request :method) "session/prompt"))
                (request-id (map-elt client :request-id))
                ;; Same fallback order the callback dispatch itself uses
                ;; (`acp--route-incoming-message'): a call-specific :buffer
                ;; override, else the client's :context-buffer.  May end up
                ;; nil (no context to report); that's fine, not a reason to
                ;; skip the check.
                (buffer (or (plist-get args :buffer)
                            (map-elt client :context-buffer)
                            t)))
      (run-at-time
       my/acp-silent-prompt-timeout-seconds nil
       (lambda ()
         (when (and my/acp-silent-prompt-alert-enabled
                    (map-nested-elt client `(:pending-requests ,request-id)))
           (my/agent-shell-patches-report-now
            'agent-shell
            (format "session/prompt (id %s) in %s got no response after %ss: no success, no error, no notification.  Check %s for the wire-level record."
                    request-id
                    (if (buffer-live-p buffer)
                        (format "buffer %s" (buffer-name buffer))
                      "a now-killed buffer")
                    my/acp-silent-prompt-timeout-seconds
                    (acp--traffic-buffer-name client))
            :warning)))))
    result))

(defconst my/acp-patches
  '((acp--make-internal-error . my/acp-drop-benign-internal-errors)
    (acp--request-sender . my/acp-warn-on-silent-prompt))
  "Patches to install once acp is loaded, as (TARGET . ADVICE) pairs.")

(defconst my/agent-shell-patches
  '((agent-shell--on-request . my/agent-shell-report-handler-errors)
    (agent-shell--on-notification . my/agent-shell-report-handler-errors))
  "Patches to install once agent-shell is loaded, as (TARGET . ADVICE) pairs.")

(defun my/agent-shell-patches--missing (patches)
  "Return the targets in PATCHES that no longer exist."
  (seq-remove #'fboundp (seq-map #'car patches)))

(defun my/agent-shell-patches--applicable (patches)
  "Return the entries of PATCHES whose target still exists."
  (seq-filter (lambda (patch) (fboundp (car patch))) patches))

(defun my/agent-shell-patches--missing-message (missing)
  "Return the text to report for the MISSING targets.
The first line names them, because that's the line the echo area gets."
  (format "agent-shell temp fixes no longer apply, targets gone: %s

An update either fixed the bug upstream (drop the patch from
my-agent-shell-patches.el) or renamed the function (repoint it).  Until
then the workaround is not running."
          (mapconcat #'symbol-name missing ", ")))

(defun my/agent-shell-patches-report (message level)
  "Report MESSAGE at LEVEL once Emacs is idle.
agent-shell is `:defer 1', so installing happens a second into startup,
where a report competes with startup churn: the echo area is overwritten
before anyone's looked at it.  Waiting for idle means it lands when
there's someone to read it.  MESSAGE's first line has to stand alone,
since that's all the echo area gets."
  (run-with-idle-timer
   1 nil
   (lambda () (my/agent-shell-patches-report-now 'my/agent-shell-patches message level))))

(defun my/agent-shell-patches-install (patches)
  "Advise every still-present target in PATCHES, and report the rest.
Returns the list of targets advised.  Safe to call repeatedly: `advice-add'
with a named function won't double-apply."
  (condition-case err
      (let ((missing (my/agent-shell-patches--missing patches)))
        (when missing
          (my/agent-shell-patches-report
           (my/agent-shell-patches--missing-message missing) :warning))
        (seq-map (lambda (patch)
                   (advice-add (car patch) :around (cdr patch))
                   (car patch))
                 (my/agent-shell-patches--applicable patches)))
    (error
     (my/agent-shell-patches-report
      (format "agent-shell temp fixes failed to install: %S" err) :error)
     nil)))

(defun my/agent-shell-patches-remove (patches)
  "Remove the advice PATCHES installed, whether or not the targets exist."
  (seq-map (lambda (patch)
             (advice-remove (car patch) (cdr patch))
             (car patch))
           patches))

(provide 'my-agent-shell-patches)
;;; my-agent-shell-patches.el ends here
