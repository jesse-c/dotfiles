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

(provide 'git-spice)
;;; git-spice.el ends here
