;;; git-spice.el --- git-spice Emacs integration for Magit -*- lexical-binding: t -*-

(require 'magit)

(defun my-git-spice-insert-branch-tree (branch branches-by-name prefix is-last)
  "Insert a branch and its children in tree format."
  (let* ((name (alist-get 'name branch))
         (change (alist-get 'change branch))
         (pr-url (when change (alist-get 'url change)))
         (push-info (alist-get 'push branch))
         (needs-push (when push-info (> (or (alist-get 'ahead push-info) 0) 0)))
         (down (alist-get 'down branch))
         (commits (alist-get 'commits branch))
         (ups (alist-get 'ups branch))
         (is-current (eq (alist-get 'current branch) t))
         (branch-symbol (if is-current "■" "□"))
         (tree-char (if is-last "┗━" "┣━"))
         (status-flags ""))

    ;; Build status flags
    (when needs-push (setq status-flags (concat status-flags " (needs push)")))

    ;; Format branch line
    (let ((branch-line (format "%s%s%s %s%s%s\n"
                               prefix
                               tree-char
                               branch-symbol
                               (propertize name 'face (if is-current 'magit-branch-current 'magit-branch-local))
                               (if pr-url
                                   (propertize (format " (%s)" (alist-get 'id change))
                                               'face 'magit-dimmed
                                               'mouse-face 'highlight
                                               'keymap (let ((map (make-sparse-keymap)))
                                                         (define-key map (kbd "RET")
                                                                     `(lambda () (interactive)
                                                                        (browse-url ,pr-url)))
                                                         (define-key map [mouse-1]
                                                                     `(lambda () (interactive)
                                                                        (browse-url ,pr-url)))
                                                         map))
                                 "")
                               (if (string-empty-p status-flags)
                                   ""
                                 (propertize status-flags 'face 'magit-dimmed)))))

      ;; Insert clickable branch line
      (magit-insert-section (git-spice-branch name t)
        (insert (if is-current
                    branch-line
                  (propertize branch-line
                              'mouse-face 'highlight
                              'keymap (let ((map (make-sparse-keymap)))
                                        (define-key map (kbd "RET")
                                                    `(lambda () (interactive) (magit-checkout ,name)))
                                        (define-key map [mouse-1]
                                                    `(lambda () (interactive) (magit-checkout ,name)))
                                        map))))))

    ;; Insert commits for this branch
    (when commits
      (let ((commit-prefix (concat prefix (if is-last "   " "┃   "))))
        (dolist (commit commits)
          (let* ((sha (alist-get 'sha commit))
                 (short-hash (substring sha 0 (min 7 (length sha))))
                 (subject (alist-get 'subject commit)))
            (magit-insert-section (commit sha)
              (insert (format "%s%s %s\n"
                              commit-prefix
                              (propertize short-hash 'face 'magit-hash)
                              subject)))))))

    ;; Insert children branches
    (when ups
      (let ((child-prefix (concat prefix (if is-last "   " "┃")))
            (child-count (length ups)))
        (dotimes (i child-count)
          (let* ((child-ref (nth i ups))
                 (child-name (alist-get 'name child-ref))
                 (child-branch (gethash child-name branches-by-name))
                 (is-last-child (= i (1- child-count))))
            (when child-branch
              (my-git-spice-insert-branch-tree child-branch branches-by-name
                                               child-prefix is-last-child))))))))

(defun my-magit-insert-git-spice-section ()
  "Insert a git-spice section in Magit status buffer."
  (magit-insert-section (git-spice nil t)
    (magit-insert-heading "Stacks")
    (magit-insert-section-body
      (let* ((current-repo (magit-toplevel))
             (command (format "gs -C %s log long --all --json" current-repo))
             (output (shell-command-to-string command))
             (lines (split-string output "\n" t))
             (branches-by-name (make-hash-table :test 'equal))
             (root-branch nil))
        (condition-case err
            (progn
              ;; Parse each line as separate JSON object
              (dolist (line lines)
                (let ((branch (json-parse-string line :object-type 'alist :array-type 'list)))
                  (let ((name (alist-get 'name branch))
                        (down (alist-get 'down branch)))
                    (puthash name branch branches-by-name)
                    ;; Root branch is one without 'down'
                    (unless down
                      (setq root-branch branch)))))
              
              ;; Build and display tree starting from root
              (if root-branch
                  (my-git-spice-insert-branch-tree root-branch branches-by-name "" t)
                (insert (propertize "No git-spice branches found\n" 'face 'magit-dimmed))))
          (error
           (message "Failed to parse git-spice output: %S" err)
           (insert (propertize "Failed to parse git-spice output\n" 'face 'magit-dimmed))))))))

(defun setup-magit-git-spice-section ()
  (interactive)
  (add-hook 'magit-status-sections-hook 'my-magit-insert-git-spice-section t))

(defun teardown-magit-git-spice-section ()
  (interactive)
  (remove-hook 'magit-status-sections-hook 'my-magit-insert-git-spice-section t))

;; Helper functions for git-spice commands
(defun git-spice-run (&rest args)
  "Run gs command with ARGS and refresh magit."
  (let* ((default-directory (magit-toplevel))
         (command-string (concat "gs " (mapconcat #'shell-quote-argument args " ")))
         (buffer (get-buffer-create "*gs*")))
    (with-current-buffer buffer
      (erase-buffer))
    (let ((proc (apply #'start-process "gs" buffer "gs" args)))
      (set-process-sentinel
       proc
       (lambda (proc event)
         (let ((exit-status (process-exit-status proc)))
           (cond
            ((and (string-match-p "finished" event) (= exit-status 0))
             (message "✓ gs %s succeeded" (mapconcat #'identity args " "))
             (magit-refresh))
            ((string-match-p "finished" event)
             (message "✗ gs %s failed (exit %d)" (mapconcat #'identity args " ") exit-status)
             (with-current-buffer (get-buffer "*gs*")
               (display-buffer (current-buffer))))
            (t
             (message "gs %s: %s" (mapconcat #'identity args " ") (string-trim event))))))))))

(defun git-spice-run-display (&rest args)
  "Run gs command with ARGS and display output."
  (let ((default-directory (magit-toplevel)))
    (async-shell-command (concat "gs " (mapconcat #'identity args " ")))))

(defun git-spice-arguments (&optional prompt)
  "Get transient arguments for current command."
  (transient-args (or prompt 'git-spice-menu)))

(defun git-spice-repo-sync ()
  "Run gs repo sync with arguments."
  (interactive)
  (apply #'git-spice-run "repo" "sync" (git-spice-arguments)))

(defun git-spice-branch-create (name)
  "Create a new branch with NAME."
  (interactive "sBranch name: ")
  (git-spice-run "branch" "create" name))

(defun git-spice-branch-checkout ()
  "Checkout a branch using gs."
  (interactive)
  (let ((branch (magit-read-branch "Checkout branch")))
    (git-spice-run "branch" "checkout" branch)))

(defun git-spice-log-short ()
  "Show short log."
  (interactive)
  (git-spice-run-display "log" "short"))

(defun git-spice-log-long ()
  "Show long log with commits."
  (interactive)
  (git-spice-run-display "log" "long"))

;; Transient menus for specific commands
(transient-define-prefix git-spice-repo-sync-menu ()
  "Git Spice repo sync menu."
  ["Arguments"
   ("-r" "Restack branches" "--restack")]
  ["Actions"
   ("Y" "Sync" git-spice-repo-sync)
   ("q" "Quit" transient-quit-one)])

(transient-define-prefix git-spice-branch-restack-menu ()
  "Git Spice branch restack menu."
  ["Arguments"
   ("-c" "Continue" "--continue")
   ("-a" "Abort" "--abort")]
  ["Actions"
   ("r" "Restack" (lambda () (interactive)
                    (apply #'git-spice-run "branch" "restack" (git-spice-arguments 'git-spice-branch-restack-menu))))
   ("q" "Quit" transient-quit-one)])

(transient-define-prefix git-spice-stack-menu ()
  "Git Spice stack operations menu."
  ["Arguments"
   ("-c" "Continue" "--continue")
   ("-a" "Abort" "--abort")]
  ["Actions"
   ("s" "Submit" (lambda () (interactive)
                   (apply #'git-spice-run "stack" "submit" (git-spice-arguments 'git-spice-stack-menu))))
   ("r" "Restack" (lambda () (interactive)
                    (apply #'git-spice-run "stack" "restack" (git-spice-arguments 'git-spice-stack-menu))))
   ("q" "Quit" transient-quit-one)])

(transient-define-prefix git-spice-menu ()
  "Git Spice commands menu."
  [["Repository"
    ("Y" "Sync›" git-spice-repo-sync-menu)]
   ["Branch"
    ("b" "Create" git-spice-branch-create :transient nil)
    ("o" "Checkout" git-spice-branch-checkout :transient nil)
    ("t" "Track" (lambda () (interactive) (git-spice-run "branch" "track")) :transient nil)
    ("U" "Untrack" (lambda () (interactive) (git-spice-run "branch" "untrack")) :transient nil)
    ("x" "Delete" (lambda () (interactive) (git-spice-run "branch" "delete")) :transient nil)
    ("m" "Rename" (lambda () (interactive)
                    (let ((new-name (read-string "New branch name: ")))
                      (git-spice-run "branch" "rename" new-name))) :transient nil)
    ("r" "Restack›" git-spice-branch-restack-menu)]]
  [["Stack"
    ("s" "Stack ops›" git-spice-stack-menu)
    ("e" "Edit" (lambda () (interactive) (git-spice-run "stack" "edit")) :transient nil)
    ("d" "Delete" (lambda () (interactive) (git-spice-run "stack" "delete")) :transient nil)]
   ["Upstack"
    ("u" "Submit" (lambda () (interactive) (git-spice-run "upstack" "submit")) :transient nil)
    ("i" "Restack" (lambda () (interactive) (git-spice-run "upstack" "restack")) :transient nil)
    ("O" "Move onto" (lambda () (interactive) (git-spice-run "upstack" "onto")) :transient nil)
    ("D" "Delete" (lambda () (interactive) (git-spice-run "upstack" "delete")) :transient nil)]
   ["Downstack"
    ("w" "Submit" (lambda () (interactive) (git-spice-run "downstack" "submit")) :transient nil)
    ("E" "Edit" (lambda () (interactive) (git-spice-run "downstack" "edit")) :transient nil)]]
  [["Commit"
    ("c" "Create" (lambda () (interactive) (git-spice-run "commit" "create")) :transient nil)
    ("a" "Amend" (lambda () (interactive) (git-spice-run "commit" "amend")) :transient nil)
    ("S" "Split" (lambda () (interactive) (git-spice-run "commit" "split")) :transient nil)
    ("f" "Fixup" (lambda () (interactive) (git-spice-run "commit" "fixup")) :transient nil)]
   ["Navigate"
    ("n" "Up ↑" (lambda () (interactive) (git-spice-run "up")) :transient t)
    ("p" "Down ↓" (lambda () (interactive) (git-spice-run "down")) :transient t)
    ("N" "Top ⬆" (lambda () (interactive) (git-spice-run "top")) :transient nil)
    ("P" "Bottom ⬇" (lambda () (interactive) (git-spice-run "bottom")) :transient nil)
    ("T" "Trunk" (lambda () (interactive) (git-spice-run "trunk")) :transient nil)]
   ["Log"
    ("l" "Short" git-spice-log-short :transient nil)
    ("L" "Long" git-spice-log-long :transient nil)]]
  [:class transient-row
          ("g" "Refresh" magit-refresh :transient t)
          ("q" "Quit" transient-quit-one)])

(provide 'git-spice)
;;; git-spice.el ends here
