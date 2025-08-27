;;; git-spice.el --- git-spice Emacs integration for Magit -*- lexical-binding: t -*-

(defun my-git-spice-insert-branch-tree (branch branches-by-index current-branch prefix is-last)
  "Insert a branch and its children in tree format."
  (let* ((name (alist-get 'Name branch))
         (change-id (alist-get 'ChangeID branch))
         (pr-number (when (and change-id (not (eq change-id :null)))
                      (alist-get 'number change-id)))
         (needs-push (eq (alist-get 'NeedsPush branch) :true))
         (needs-restack (eq (alist-get 'NeedsRestack branch) :true))
         (commits (let ((c (alist-get 'Commits branch)))
                    (if (eq c :null) nil c)))
         (aboves (let ((a (alist-get 'Aboves branch)))
                   (if (eq a :null) nil a)))
         (is-current (string= name current-branch))
         (branch-symbol (if is-current "■" "□"))
         (tree-char (if is-last "┗━" "┣━"))
         (status-flags ""))

    ;; Build status flags
    (when needs-restack (setq status-flags (concat status-flags " (needs restack)")))
    (when needs-push (setq status-flags (concat status-flags " (needs push)")))

    ;; Format branch line
    (let ((branch-line (format "%s%s%s %s%s%s\n"
                               prefix
                               tree-char
                               branch-symbol
                               (propertize name 'face (if is-current 'magit-branch-current 'magit-branch-local))
                               (if pr-number
                                   (propertize (format " (#%d)" pr-number)
                                               'face 'magit-dimmed
                                               'mouse-face 'highlight
                                               'keymap (let ((map (make-sparse-keymap)))
                                                         (define-key map (kbd "RET")
                                                                     `(lambda () (interactive)
                                                                        (let ((repo-url (magit-get "remote" "origin" "url")))
                                                                          (when repo-url
                                                                            (let ((repo-name (replace-regexp-in-string)
                                                                                             ".*[:/]\\([^/]+/[^/]+\\)\\.git$" "\\1" repo-url))
                                                                              (browse-url (format "https://github.com/%s/pull/%d" repo-name ,pr-number)))))))
                                                         (define-key map [mouse-1]
                                                                     `(lambda () (interactive)
                                                                        (let ((repo-url (magit-get "remote" "origin" "url")))
                                                                          (when repo-url
                                                                            (let ((repo-name (replace-regexp-in-string)
                                                                                             ".*[:/]\\([^/]+/[^/]+\\)\\.git$" "\\1" repo-url))
                                                                              (browse-url (format "https://github.com/%s/pull/%d" repo-name ,pr-number)))))))
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
          (let ((hash (alist-get 'Hash commit))
                (short-hash (alist-get 'ShortHash commit))
                (subject (alist-get 'Subject commit)))
            (magit-insert-section (commit hash)
              (insert (format "%s%s %s\n"
                              commit-prefix
                              (propertize short-hash 'face 'magit-hash)
                              subject)))))))

    ;; Insert children branches
    (when aboves
      (let ((child-prefix (concat prefix (if is-last "   " "┃")))
            (child-count (length aboves)))
        (dotimes (i child-count)
          (let* ((child-index (nth i aboves))
                 (child-branch (gethash child-index branches-by-index))
                 (is-last-child (= i (1- child-count))))
            (when child-branch
              (my-git-spice-insert-branch-tree child-branch branches-by-index current-branch
                                               child-prefix is-last-child))))))))

(defun my-magit-insert-git-spice-section ()
  "Insert a git-spice section in Magit status buffer."
  (magit-insert-section (git-spice nil t)
    (magit-insert-heading "Stacks")
    (magit-insert-section-body
      (let* ((current-repo (magit-toplevel))
             (current-branch (magit-get-current-branch))
             (command (format "/Users/jesse/src/github.com/jesse-c/git-spice/gsx -C %s log long --all --json"
                              current-repo))
             (output (shell-command-to-string command))
             (json-data (condition-case nil
                            (json-parse-string output :object-type 'alist :array-type 'list)
                          (error nil))))
        (cond
         ;; Check if git-spice is not initialized
         ((and (stringp output)
               (string-match-p "Repository not initialized" output))
          (insert (propertize "Repository not initialized with git-spice\n" 'face 'magit-dimmed)))
         ;; Parse successful JSON data
         (json-data
          (let ((branches-by-index (make-hash-table :test 'equal))
                (main-branch nil))
            ;; Index branches by their index for quick lookup
            (dolist (branch json-data)
              (let ((index (alist-get 'Index branch))
                    (name (alist-get 'Name branch)))
                (puthash index branch branches-by-index)
                (when (string= name "main")
                  (setq main-branch branch))))

            ;; Build and display tree starting from main
            (when main-branch
              (my-git-spice-insert-branch-tree main-branch branches-by-index current-branch "" t))))
         ;; Fallback for other errors
         (t
          (insert "Failed to parse git-spice JSON output\n")))))))

(defun setup-magit-git-spice-section ()
  (interactive)
  (add-hook 'magit-status-sections-hook 'my-magit-insert-git-spice-section t))

(defun teardown-magit-git-spice-section ()
  (interactive)
  (remove-hook 'magit-status-sections-hook 'my-magit-insert-git-spice-section t))

(provide 'git-spice)
;;; git-spice.el ends here
