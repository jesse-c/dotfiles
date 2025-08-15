;;; -*- lexical-binding: t; -*-

(remove-hook 'magit-status-sections-hook 'my-magit-git-spice-working)
(remove-hook 'magit-status-sections-hook 'my-magit-insert-dummy-section)

(defun my-magit-insert-dummy-section ()
  "Insert a dummy section in Magit status buffer."
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
        (if json-data
            (progn
              (insert "Branches:\n")
              (dolist (branch json-data)
                (let ((name (alist-get 'Name branch)))
                  (when name
                    (if (string= name current-branch)
                        (insert (format "* %s\n" (propertize name 'face 'magit-branch-current)))
                      (insert (format "  %s\n" 
                                      (propertize name
                                                  'face 'magit-branch-local
                                                  'keymap (let ((map (make-sparse-keymap)))
                                                            (define-key map [mouse-1] 
                                                              `(lambda () (interactive) 
                                                                 (magit-checkout ,name)))
                                                            (define-key map (kbd "RET")
                                                              `(lambda () (interactive)
                                                                 (magit-checkout ,name)))
                                                            map)
                                                  'mouse-face 'highlight))))))))
          (insert (format "Failed to parse JSON:\n%s\n" output)))))))

(add-hook 'magit-status-sections-hook 'my-magit-insert-dummy-section t)
