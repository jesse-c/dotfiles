;;; slow.el -*- lexical-binding: t; -*-

;;; Enhanced slow command monitoring - file saving only, no popup

(require 'profiler)
(require 'cl-lib)

(defvar slow-command-threshold 0.500
  "Warn about commands slower than this many seconds.")

(defvar slow-command-log-file
  (expand-file-name "slow-commands.log" user-emacs-directory)
  "File to log slow commands to.")

(defvar slow-command-profiles-directory
  (expand-file-name "profiles/" user-emacs-directory)
  "Directory to save individual profile files.")

(defvar slow-command-save-profiles-to-files t
  "Whether to save full profiler reports to individual files.")

(defvar slow-command-profile-file-format "txt"
  "Format for saved profile files: 'txt', 'json', or 'elisp'.")

(defvar slow-command-auto-profile-all-commands nil
  "Whether to profile all commands to capture slow ones.")

(defvar slow-command-capture-detailed-profile nil
  "Whether to capture detailed profiling data for slow commands.")

(defvar slow-command-show-profiler-buffer nil
  "Whether to show the profiler buffer popup. Set to nil to disable.")

(defvar slow-command-auto-profile-threshold 1.0
  "Save detailed profiles for commands slower than this.")

(defvar slow-command-last-command nil
  "Stores the last command and its start time.")

(defvar slow-command-show-notifications t
  "Whether to show message notifications for slow commands.")

(defvar slow-command-profiling-active nil
  "Whether we're currently profiling a command.")

;; Enhanced structure with profile file path
(cl-defstruct slow-command-event
  command
  duration
  timestamp
  buffer
  major-mode
  file-path
  keys
  profile-data
  profile-summary
  profile-file-path)

;; Ensure profiles directory exists
(unless (file-directory-p slow-command-profiles-directory)
  (make-directory slow-command-profiles-directory t))

(defun slow-command-generate-profile-filename (command duration timestamp)
  "Generate a filename for saving the profile."
  (let ((time-str (format-time-string "%Y%m%d_%H%M%S" timestamp))
        (cmd-str (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_"
                                           (symbol-name command)))
        (duration-str (format "%.2fs" duration)))
    (format "%s_%s_%s.%s"
            time-str cmd-str duration-str slow-command-profile-file-format)))

(defun slow-command-save-profile-to-file (command duration timestamp)
  "Save the current profiler report to a file without showing buffer."
  (when (and slow-command-save-profiles-to-files
             (get-buffer "*Profiler-Report*"))
    (let* ((filename (slow-command-generate-profile-filename command duration timestamp))
           (filepath (expand-file-name filename slow-command-profiles-directory))
           (profile-content (with-current-buffer "*Profiler-Report*"
                              (buffer-string))))

      ;; Save based on format
      (cond
       ((string= slow-command-profile-file-format "txt")
        (slow-command-save-profile-txt filepath command duration timestamp profile-content))
       ((string= slow-command-profile-file-format "json")
        (slow-command-save-profile-json filepath command duration timestamp profile-content))
       ((string= slow-command-profile-file-format "elisp")
        (slow-command-save-profile-elisp filepath command duration timestamp)))

      ;; Clean up the profiler buffer silently
      (when (get-buffer "*Profiler-Report*")
        (kill-buffer "*Profiler-Report*"))

      filepath)))

(defun slow-command-save-profile-txt (filepath command duration timestamp profile-content)
  "Save profile as formatted text file."
  (with-temp-buffer
    (insert (format "Slow Command Profile Report\n"))
    (insert (format "==========================\n\n"))
    (insert (format "Command: %s\n" command))
    (insert (format "Duration: %.3f seconds\n" duration))
    (insert (format "Timestamp: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)))
    (insert (format "Generated: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert "Profiler Report:\n")
    (insert "================\n\n")
    (insert profile-content)
    (write-region (point-min) (point-max) filepath)))

(defun slow-command-save-profile-json (filepath command duration timestamp profile-content)
  "Save profile as JSON-like format."
  (with-temp-buffer
    (insert "{\n")
    (insert (format "  \"command\": \"%s\",\n" command))
    (insert (format "  \"duration\": %.3f,\n" duration))
    (insert (format "  \"timestamp\": \"%s\",\n" (format-time-string "%Y-%m-%dT%H:%M:%SZ" timestamp)))
    (insert (format "  \"generated\": \"%s\",\n" (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
    (insert "  \"profile_data\": [\n")

    ;; Parse profile content into JSON-like format
    (let ((lines (split-string profile-content "\n"))
          (first-line t))
      (dolist (line lines)
        (when (string-match "^\\s-*\\([0-9]+\\)%\\s-+\\([0-9.]+\\)\\(ms\\|s\\)\\s-+\\(.+\\)$" line)
          (unless first-line (insert ",\n"))
          (setq first-line nil)
          (let ((percentage (match-string 1 line))
                (time (match-string 2 line))
                (unit (match-string 3 line))
                (function-name (match-string 4 line)))
            (insert (format "    {\"percentage\": %s, \"time\": \"%s%s\", \"function\": \"%s\"}"
                            percentage time unit function-name))))))

    (insert "\n  ],\n")
    (insert "  \"raw_report\": ")
    (insert (json-encode profile-content))
    (insert "\n}\n")
    (write-region (point-min) (point-max) filepath)))

(defun slow-command-save-profile-elisp (filepath command duration timestamp)
  "Save profile as Elisp data structure."
  (when (profiler-running-p)
    (let ((profile-data (profiler-cpu-log)))
      (with-temp-buffer
        (insert ";; Slow Command Profile Data\n")
        (insert (format ";; Command: %s\n" command))
        (insert (format ";; Duration: %.3f seconds\n" duration))
        (insert (format ";; Timestamp: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)))
        (insert "(setq slow-command-profile-data\n")
        (insert "  '((command . ")
        (prin1 command (current-buffer))
        (insert ")\n    (duration . ")
        (prin1 duration (current-buffer))
        (insert ")\n    (timestamp . ")
        (prin1 timestamp (current-buffer))
        (insert ")\n    (profile-log . ")
        (prin1 profile-data (current-buffer))
        (insert ")))\n")
        (write-region (point-min) (point-max) filepath)))))

(defun slow-command-extract-profile-data ()
  "Extract and format profiler data silently."
  (when (and slow-command-capture-detailed-profile
             (profiler-running-p))
    (let ((report-buffer "*Profiler-Report*"))
      ;; Clean up any existing report buffer
      (when (get-buffer report-buffer)
        (kill-buffer report-buffer)))

    ;; Stop profiler and generate report silently
    (profiler-stop)
    (let ((inhibit-message t)
          (message-log-max nil)
          (pop-up-windows nil)
          (display-buffer-alist '(("\\*Profiler-Report\\*" display-buffer-no-window))))
      (profiler-report-cpu))

    ;; Extract data from the report buffer
    (when (get-buffer "*Profiler-Report*")
      (with-current-buffer "*Profiler-Report*"
        (save-excursion
          (goto-char (point-min))
          ;; Skip header lines
          (forward-line 5)
          (let ((lines-collected 0)
                (profile-lines '()))
            (while (and (< lines-collected 15)
                        (not (eobp)))
              (let ((line (buffer-substring-no-properties)
                          (line-beginning-position)
                          (line-end-position)))
                (when (and (> (length line) 10)
                           (string-match "^\\s-*[0-9]" line))
                  (push line profile-lines)
                  (setq lines-collected (1+ lines-collected))))
              (forward-line 1))
            (when profile-lines
              (string-join (reverse profile-lines) "\n"))))))))

(defun slow-command-extract-profile-summary ()
  "Extract a brief summary of the profiling data."
  (when (get-buffer "*Profiler-Report*")
    (with-current-buffer "*Profiler-Report*"
      (save-excursion
        (goto-char (point-min))
        (forward-line 5)
        (let ((top-functions '())
              (lines-collected 0))
          (while (and (< lines-collected 5)
                      (not (eobp)))
            (let ((line (buffer-substring-no-properties)
                        (line-beginning-position)
                        (line-end-position)))
              (when (and (> (length line) 10)
                         (string-match "^\\s-*\\([0-9]+\\)%.*?\\([^ \t]+\\)\\s-*$" line))
                (let ((percentage (match-string 1 line))
                      (function-name (match-string 2 line)))
                  (push (format "%s%% %s" percentage function-name) top-functions)
                  (setq lines-collected (1+ lines-collected)))))
            (forward-line 1))
          (when top-functions
            (string-join (reverse top-functions) ", ")))))))

(defun slow-command-create-event (command duration)
  "Create a structured slow command event with profiling data."
  (let* ((timestamp (current-time))
         (profile-data (slow-command-extract-profile-data))
         (profile-summary (slow-command-extract-profile-summary))
         (profile-file-path (when slow-command-save-profiles-to-files
                              (slow-command-save-profile-to-file command duration timestamp))))
    (make-slow-command-event
     :command command
     :duration duration
     :timestamp timestamp
     :buffer (buffer-name)
     :major-mode major-mode
     :file-path (buffer-file-name)
     :keys (key-description (this-command-keys))
     :profile-data profile-data
     :profile-summary profile-summary
     :profile-file-path profile-file-path)))

(defun slow-command-format-event (event &optional include-full-profile)
  "Format a slow command event for display."
  (let ((basic-info (format "[%s] %s (%s) took %.3fs in %s (%s) - %s"
                            (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                (slow-command-event-timestamp event))
                            (slow-command-event-command event)
                            (slow-command-event-keys event)
                            (slow-command-event-duration event)
                            (slow-command-event-buffer event)
                            (slow-command-event-major-mode event)
                            (or (slow-command-event-file-path event) "no file")))
        (summary (slow-command-event-profile-summary event))
        (profile-file (slow-command-event-profile-file-path event)))

    (if summary
        (format "%s\nTop functions: %s\nProfile file: %s"
                basic-info
                summary
                (if profile-file
                    (file-name-nondirectory profile-file)
                  "not saved"))
      basic-info)))

;; [Rest of the monitoring code...]
(defvar slow-command-patterns (make-hash-table :test 'equal))
(defvar slow-command-history nil)
(defvar slow-command-ring-size 50)

(defun slow-command-log (event)
  "Log a slow command event with profiling data."
  (let ((formatted (slow-command-format-event event t)))

    ;; Show notification
    (when slow-command-show-notifications
      (message "⚠ Slow: %s (%.2fs) → %s"
               (slow-command-event-command event)
               (slow-command-event-duration event)
               (if (slow-command-event-profile-file-path event)
                   (file-name-nondirectory (slow-command-event-profile-file-path event))
                 "no profile")))

    ;; Log to main log file
    (when slow-command-log-file
      (with-temp-buffer
        (insert formatted "\n\n")
        (append-to-file (point-min) (point-max) slow-command-log-file)))

    ;; Add to history ring
    (push event slow-command-history)
    (when (> (length slow-command-history) slow-command-ring-size)
      (setq slow-command-history
            (cl-subseq slow-command-history 0 slow-command-ring-size)))))

(defun slow-command-before ()
  "Record command start time and start profiling."
  (setq slow-command-last-command
        (list this-command (float-time) (current-time)))

  ;; Start profiling if enabled
  (when (and slow-command-auto-profile-all-commands
             (not slow-command-profiling-active)
             (not (profiler-running-p)))
    (condition-case nil
        (progn
          (let ((inhibit-message t))
            (profiler-start 'cpu))
          (setq slow-command-profiling-active t))
      (error nil))))

(defun slow-command-after-with-profiling ()
  "Enhanced post-command hook - saves files but no popup."
  (when slow-command-last-command
    (let* ((command (nth 0 slow-command-last-command))
           (start-time (nth 1 slow-command-last-command))
           (duration (- (float-time) start-time)))

      ;; Check if this was a slow command
      (if (> duration slow-command-threshold)
          ;; Slow command - create event and save profile file
          (let ((event (slow-command-create-event command duration)))
            (slow-command-log event))

        ;; Fast command - just clean up profiler silently
        (when (and slow-command-profiling-active (profiler-running-p))
          (let ((inhibit-message t))
            (profiler-stop))))

      ;; Clean up
      (setq slow-command-profiling-active nil)
      (setq slow-command-last-command nil))))

;; Utility functions remain the same...
(defun slow-command-open-profiles-directory ()
  "Open the profiles directory in dired."
  (interactive)
  (dired slow-command-profiles-directory))

(defun slow-command-list-profile-files ()
  "List all saved profile files."
  (interactive)
  (with-current-buffer (get-buffer-create "*Profile Files*")
    (erase-buffer)
    (insert "Saved Profile Files:\n")
    (insert "====================\n\n")
    (let ((files (directory-files slow-command-profiles-directory t "^[0-9]")))
      (dolist (file files)
        (let ((attrs (file-attributes file)))
          (insert (format "%s - %s (%.1fKB)\n"
                          (file-name-nondirectory file)
                          (format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 attrs))
                          (/ (nth 7 attrs) 1024.0))))))
    (pop-to-buffer (current-buffer))))

;; Enable/disable functions
(defun enable-slow-command-monitoring ()
  "Enable slow command monitoring with silent profile file saving."
  (interactive)
  (add-hook 'pre-command-hook #'slow-command-before)
  (add-hook 'post-command-hook #'slow-command-after-with-profiling)
  (message "Slow command monitoring enabled (silent mode)\nProfiles: %s\nFormat: %s"
           slow-command-profiles-directory
           slow-command-profile-file-format))

(defun disable-slow-command-monitoring ()
  "Disable slow command monitoring."
  (interactive)
  (remove-hook 'pre-command-hook #'slow-command-before)
  (remove-hook 'post-command-hook #'slow-command-after-with-profiling)
  (when (profiler-running-p)
    (profiler-stop))
  (setq slow-command-profiling-active nil)
  (message "Slow command monitoring disabled"))

;; Set defaults for silent operation
(setq slow-command-show-profiler-buffer nil)  ; No popup
(setq slow-command-save-profiles-to-files t)  ; Save to files

(provide 'slow)
;;; slow.el ends here
