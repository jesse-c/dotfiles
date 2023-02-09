;;; circleci.el --- Manage CircleCI -*- lexical-binding: t; -*-
;;; Code:

(require 'auth-source)

;; https://github.com/technomancy/circleci.el/blob/master/circleci.el
(defun circleci--get-token ()
  (let* ((auth-sources '("~/.authinfo"))
         (secret-spec (car (auth-source-search :host "circleci.com/api/v2")))
         (token (funcall (plist-get secret-spec :secret)))
         (save-fn (plist-get secret-spec :save-function)))
    (when save-fn (funcall save-fn))
    token))

;; https://stackoverflow.com/a/1664272/1861724
(setq headers `(("Accept" . "application/json")
                ("Circle-Token" . ,(circleci--get-token))))

(defun build-url (url)
  ""
  (format "https://circleci.com/api/v2/%s" url))


(defun http/get (url)
  "Do a GET request to URL."
  (request
    (build-url url)
    :type "GET"
    :headers headers
    :parser 'json-read
    :sync t))

(defun http/post (url data)
  "Do a POST request to URL with DATA."
  (request
    (build-url url)
    :type "GET"
    :headers headers
    :data data
    :parser 'json-read
    :sync t))

(defun my/circleci-workflow-created-at (workflow)
  "Parse the created at field from a WORKFLOW"
  (->> workflow
       (assoc 'created_at)
       (cdr)
       (iso8601-parse)))

;; https://circleci.com/docs/api/v2/index.html#operation/getWorkflowById
(setq statuses '("success" "running" "not_run" "failed" "error" "failing" "on_hold" "canceled" "unauthorized"))
(seq-map #'make-symbol statuses)

(defun my/circleci-get-branch-status (branch)
  "Get the latest workflow status from CircleCI for BRANCH."
  (interactive)
  (let* ((pipeline-mine (http/get "project/gh/duffelhq/platform/pipeline/mine"))
         (items (assoc 'items (request-response-data pipeline-mine)))
         (pipeline-matching-branch (seq-find (lambda (pipeline) (equal (let-alist pipeline .vcs.branch) branch) ) (cdr items)))
         (pipeline-workflows (http/get (format "pipeline/%s/workflow" (cdr (assoc 'id pipeline-matching-branch)))))
         (workflow-items (cdr (assoc 'items (request-response-data pipeline-workflows))))
         (sorted-workflows (seq-sort (lambda (a b) (equal (my/circleci-workflow-created-at a) (my/circleci-workflow-created-at b))) workflow-items)))
    ;; Get the status, as a symbol, from the most recently created workflow for this branch
    (message "%S" (intern-soft (cdr (assoc 'status (elt sorted-workflows 0)))))))

(defun my/circleci-get-current-branch-status ()
  "Get the latest workflow status from CircleCI for current branch."
  (interactive)
  (my/circleci-get-branch-status (magit-get-current-branch)))

(defun my/circleci-get-branch-latest-workflow (branch)
  "Get the latest workflow for the BRANCH."
  "Get the latest workflow status from CircleCI for a BRANCH."
  (interactive)
  (let* ((pipeline-mine (http/get "project/gh/duffelhq/platform/pipeline/mine"))
         (items (assoc 'items (request-response-data pipeline-mine)))
         (pipeline-matching-branch (seq-find (lambda (pipeline) (equal (let-alist pipeline .vcs.branch) branch) ) (cdr items)))
         (pipeline-workflows (http/get (format "pipeline/%s/workflow" (cdr (assoc 'id pipeline-matching-branch)))))
         (workflow-items (cdr (assoc 'items (request-response-data pipeline-workflows))))
         (sorted-workflows (seq-sort (lambda (a b) (equal (my/circleci-workflow-created-at a) (my/circleci-workflow-created-at b))) workflow-items)))
    ;; Most recently created workflow for this branch
    (elt sorted-workflows 0)))

(defun my/circleci-get-current-branch-latest-workflow ()
  "Get the latest workflow for the current branch."
  (interactive)
  (my/circleci-get-branch-latest-workflow (magit-get-current-branch)))

(defun my/circleci-rerun-workflow (workflow)
  "Rerun the WORKFLOW from failed."
  (interactive)
  (http/post (format "workflow/%s/rerun" (cdr (assoc 'id workflow))) '(("from_failed" . "true"))))

(defun my/circleci-rerun-workflow-current-branch ()
  "Rerun the latest workflow for the current branch from failed."
  (interactive)
  (my/circleci-rerun-workflow (my/circleci-get-current-branch-latest-workflow)))
