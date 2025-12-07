(require 'cl-lib)
(require 'project)

(defgroup srgn nil
  "Emacs interface for srgn."
  :group 'tools
  :prefix "srgn-")

(defcustom srgn-executable "srgn"
  "Path to the srgn executable."
  :type 'string)

(defcustom srgn-preview-window-height 0.3
  "Height of the preview window as a fraction of the frame height."
  :type 'float
  :group 'srgn)

(defcustom srgn-enable-live-preview nil
  "Enable live preview while typing in minibuffer."
  :type 'boolean
  :group 'srgn)

(defcustom srgn-debug-messages nil
  "Enable debug messages to *Messages* buffer."
  :type 'boolean
  :group 'srgn)

(defvar srgn--current-buffer nil
  "Buffer currently being searched.")

(defvar srgn--current-language nil
  "Language currently being used for search.")

(defface srgn-match-face
  '((t :background "yellow" :foreground "black"))
  "Face for highlighting srgn matches."
  :group 'srgn)

(defun srgn--ensure-executable ()
  "Ensure srgn executable is available."
  (unless (executable-find srgn-executable)
    (user-error "srgn executable not found. Please install srgn or set `srgn-executable`")))

(defun srgn--run-command-on-project (args)
  "Run srgn with ARGS on project files."
  (let* ((project (project-current t))
         (project-root (project-root project))
         (default-directory project-root))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process
                              srgn-executable
                              nil t nil args)))
        (when srgn-debug-messages
          (message "srgn command: %s %s in %s" srgn-executable (mapconcat 'identity args " ") default-directory)
          (message "srgn exit code: %d" exit-code)
          (message "srgn output: %s" (buffer-string)))
        (if (zerop exit-code)
            (buffer-string)
          (error "srgn failed: %s" (buffer-string)))))))

(defun srgn--run-command (args &optional input)
  "Run srgn with ARGS and optional INPUT."
  (let ((default-directory (or (and (buffer-file-name)
                                    (file-name-directory (buffer-file-name)))
                               default-directory)))
    (with-temp-buffer
      (when input
        (insert input))
      (let ((exit-code (apply #'call-process-region
                              (point-min) (point-max)
                              srgn-executable
                              t t nil args)))
        (if (zerop exit-code)
            (buffer-string)
          (error "srgn failed: %s" (buffer-string)))))))

(defun srgn--parse-shell-args (arg-string)
  "Parse ARG-STRING like a shell would, handling quotes."
  (let ((args '())
        (current-arg "")
        (in-quotes nil)
        (quote-char nil)
        (i 0))
    (while (< i (length arg-string))
      (let ((char (aref arg-string i)))
        (cond
         ;; Starting a quoted section
         ((and (not in-quotes) (or (eq char ?\') (eq char ?\")))
          (setq in-quotes t quote-char char))
         ;; Ending a quoted section
         ((and in-quotes (eq char quote-char))
          (setq in-quotes nil quote-char nil))
         ;; Space outside quotes - end current arg
         ((and (not in-quotes) (eq char ?\s))
          (when (not (string-empty-p current-arg))
            (push current-arg args)
            (setq current-arg "")))
         ;; Regular character
         (t
          (setq current-arg (concat current-arg (char-to-string char))))))
      (setq i (1+ i)))
    ;; Add final arg if any
    (when (not (string-empty-p current-arg))
      (push current-arg args))
    (nreverse args)))

(defun srgn--search-in-project (pattern-and-args)
  "Search using PATTERN-AND-ARGS string passed directly to srgn."
  (when (and pattern-and-args (not (string-empty-p pattern-and-args)))
    (let* ((user-args (srgn--parse-shell-args pattern-and-args))
           (result (condition-case-unless-debug err
                      (srgn--run-command-on-project user-args)
                    (error nil))))
      (when result
        (srgn--parse-search-results result)))))

(defun srgn--search-in-buffer (pattern &optional language)
  "Search for PATTERN in current buffer using optional LANGUAGE."
  (when (and pattern (not (string-empty-p pattern)))
    (let* ((buffer-content (buffer-string))
           (args (append (when language (list "--language" language))
                        (list pattern)))
           (result (condition-case-unless-debug err
                      (srgn--run-command args buffer-content)
                    (error nil))))
      (when result
        (srgn--parse-search-results result)))))

(defun srgn--parse-search-results (output)
  "Parse srgn OUTPUT into structured results."
  (let ((lines (split-string output "\n" t))
        results)
    (cl-loop for line in lines
             do (cond
                 ;; Format: file:line (no content shown)
                 ((string-match "^\\([^:]+\\):\\([0-9]+\\)$" line)
                  (push (list :file (match-string 1 line)
                             :line (string-to-number (match-string 2 line))
                             :column 1
                             :content "")
                        results))
                 ;; Format: file:line:col-col:content
                 ((string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\(.*\\)$" line)
                  (push (list :file (match-string 1 line)
                             :line (string-to-number (match-string 2 line))
                             :column (string-to-number (match-string 3 line))
                             :content (match-string 5 line))
                        results))
                 ;; Format: file:line:content  
                 ((string-match "^\\([^:]+\\):\\([0-9]+\\):\\(.*\\)$" line)
                  (push (list :file (match-string 1 line)
                             :line (string-to-number (match-string 2 line))
                             :column 1
                             :content (match-string 3 line))
                        results))
                 ;; Just filename
                 ((string-match "^\\([^:]+\\)$" line)
                  (push (list :file line :line 1 :column 1 :content "")
                        results))))
    (nreverse results)))

(defun srgn--show-preview (match)
  "Show preview for MATCH using preview window."
  (when match
    (let* ((line-num (plist-get match :line))
           (col-num (plist-get match :column))
           (content (plist-get match :content)))
      
      (with-current-buffer-window
       "*srgn-preview*"
       `(display-buffer-in-side-window
         (side . bottom)
         (window-height . ,srgn-preview-window-height)
         (window-parameters . ((no-other-window . t))))
       nil
       
       (insert (propertize (format "Line %d, Column %d:" line-num col-num)
                          'face 'font-lock-comment-face))
       (insert "\n\n")
       (insert content)
       (goto-char (point-min))
       (when (derived-mode-p 'prog-mode)
         (font-lock-mode 1))))))

(defun srgn--format-result (match)
  "Format MATCH for display in completion."
  (let* ((file (plist-get match :file))
         (line (plist-get match :line))
         (content (plist-get match :content))
         ;; Use actual file content if srgn content is just fragments
         (display-content (if (srgn--is-meaningful-content content)
                             (string-trim content)
                           (srgn--get-file-line file line)))
         (line-str (format ":%d" line))
         ;; Truncate filename intelligently, keeping meaningful parts
         (max-file-width (- 38 (length line-str))) ; Reserve space for :line
         (truncated-file (if (> (length file) max-file-width)
                            (let* ((filename (file-name-nondirectory file))
                                   (directory (file-name-directory file))
                                   (available-space (- max-file-width (length filename) 3))) ; 3 for "..."
                              (if (> available-space 10) ; If we have space, show some directory
                                  (concat "..." (substring directory (max 0 (- (length directory) available-space))) filename)
                                (concat "..." filename))) ; Just filename if no space
                          file))
         (location (concat (propertize truncated-file 'face 'font-lock-constant-face)
                          (propertize line-str 'face 'font-lock-comment-face))))
    (if display-content
        (format "%-40s %s" 
                location
                (propertize display-content 'face 'default))
      location)))

(defun srgn--marginalia-annotate (candidate)
  "Provide marginalia annotation for CANDIDATE."
  nil)

(defun srgn--completion-table (pattern-and-args)
  "Create completion table for PATTERN-AND-ARGS."
  (when-let* ((results (srgn--search-in-project pattern-and-args)))
    (cl-mapcar (lambda (match)
                 (let ((formatted (srgn--format-result match)))
                   (propertize formatted 'srgn-match match)))
               results)))

(defun srgn--completing-read (prompt &optional initial-input)
  "Enhanced completing-read for srgn with PROMPT and INITIAL-INPUT."
  (read-string prompt initial-input))

(defun srgn--minibuffer-update (&rest _)
  "Update preview while typing in minibuffer."
  (when (minibufferp)
    (let* ((input (minibuffer-contents))
           (candidates (when (> (length input) 1)
                        (srgn--completion-table input srgn--current-language))))
      (when (and candidates (car candidates))
        (let ((match (get-text-property 0 'srgn-match (car candidates))))
          (srgn--show-preview match))))

(defun srgn--get-file-line (file line-num)
  "Get the actual content of LINE-NUM from FILE."
  (when (and file (file-exists-p file) (> line-num 0))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (= (forward-line (1- line-num)) 0)
        (string-trim (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))))))

(defun srgn--is-meaningful-content (content)
  "Return t if CONTENT is meaningful, nil if it's just fragments."
  (and content 
       (not (string-empty-p content))
       (not (string-match-p "^[[:punct:][:space:]]*$" content))
       (> (length (string-trim content)) 2)))

(defun srgn--select-result (results)
  "Let user select from RESULTS with filtering and preview."
  (let* ((candidates (cl-mapcar (lambda (match)
                                  (let ((formatted (srgn--format-result match)))
                                    (propertize formatted 'srgn-match match)))
                                results))
         (completion-extra-properties
          '(:annotation-function srgn--marginalia-annotate
            :category srgn-match))
         (completion-styles (if (featurep 'orderless)
                               '(orderless basic)
                             '(substring basic)))
         (selected (completing-read (format "Select result (%d matches): " (length results)) candidates nil t)))
    
    (when selected
      (when-let* ((match (get-text-property 0 'srgn-match selected))
                  (file (plist-get match :file))
                  (line-num (plist-get match :line))
                  (col-num (plist-get match :column)))
        ;; Open file and jump to location
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- line-num))
        (when (> col-num 1)
          (forward-char (1- col-num)))
        (recenter)
        (message "Jumped to %s:%d:%d" file line-num col-num)))))

(defun srgn-search (&optional pattern-and-args)
  "Interactive srgn search. 
Pass PATTERN-AND-ARGS directly to srgn (e.g., '--py class TextBlockLength')."
  (interactive)
  (srgn--ensure-executable)
  
  (let* ((initial-pattern (or pattern-and-args
                              (when (use-region-p)
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end)))
                              ""))
         (user-input (read-string "srgn args: " initial-pattern)))
    
    (when (and user-input (not (string-empty-p user-input)))
      (let ((results (srgn--search-in-project user-input)))
        (when results
          (srgn--select-result results))))))

(defun srgn-replace (pattern replacement &optional language)
  "Replace PATTERN with REPLACEMENT using optional LANGUAGE."
  (interactive "sSearch pattern: \nsReplace with: ")
  (srgn--ensure-executable)
  
  (let* ((buffer-content (buffer-string))
         (args (append (when language (list "--language" language))
                      (list pattern replacement)))
         (result (srgn--run-command args buffer-content)))
    
    (when result
      (erase-buffer)
      (insert result)
      (message "Replaced with srgn"))))

