;;; -*- lexical-binding: t; -*-

(require 'profiler)
(require 'cl-lib)

(defvar slow-command-threshold 1.0
  "Warn about commands slower than this many seconds.")

(defvar slow-command-log-file
  (expand-file-name "slow-commands.log" user-emacs-directory)
  "File to log slow commands to.")

(defvar slow-command-auto-profile-all-commands t
  "Whether to profile all commands to capture slow ones.")

(defvar slow-command-capture-detailed-profile t
  "Whether to capture detailed profiling data for slow commands.")

(defvar slow-command-profile-threshold 0.5
  "Start profiling commands expected to take longer than this.")

(defvar slow-command-auto-profile-threshold 2.0
  "Show profiler report for commands slower than this.")

(defvar slow-command-last-command nil
  "Stores the last command and its start time.")

(defvar slow-command-show-notifications t
  "Whether to show message notifications for slow commands.")

(defvar slow-command-profiling-active nil
  "Whether we're currently profiling a command.")

;; Enhanced structure with profiling data
(cl-defstruct slow-command-event
  command
  duration
  timestamp
  buffer
  major-mode
  file-path
  keys
  profile-data
  profile-summary)

(defun slow-command-extract-profile-data ()
  "Extract and format profiler data."
  (when (and slow-command-capture-detailed-profile
             (profiler-running-p))
    (let ((report-buffer "*Profiler-Report*"))
      (when (get-buffer report-buffer)
        (kill-buffer report-buffer)))

    ;; Stop profiler and generate report
    (profiler-stop)
    (profiler-report)

    ;; Extract data from the report buffer
    (when (get-buffer "*Profiler-Report*")
      (with-current-buffer "*Profiler-Report*"
        (let ((content (buffer-string)))
          ;; Extract top functions (first 20 lines of meaningful content)
          (save-excursion
            (goto-char (point-min))
            ;; Skip header lines
            (forward-line 5)
            (let ((start (point))
                  (lines-collected 0)
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
                (string-join (reverse profile-lines) "\n")))))))))

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
  (let ((profile-data (slow-command-extract-profile-data))
        (profile-summary (slow-command-extract-profile-summary)))
    (make-slow-command-event
     :command command
     :duration duration
     :timestamp (current-time)
     :buffer (buffer-name)
     :major-mode major-mode
     :file-path (buffer-file-name)
     :keys (key-description (this-command-keys))
     :profile-data profile-data
     :profile-summary profile-summary)))

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
        (full-profile (slow-command-event-profile-data event)))

    (cond
     ;; Full profile requested and available
     ((and include-full-profile full-profile)
      (format "%s\nTop functions: %s\nDetailed profile:\n%s\n---"
              basic-info
              (or summary "no summary")
              full-profile))
     ;; Summary available
     (summary
      (format "%s\nTop functions: %s"
              basic-info
              summary))
     ;; Basic info only
     (t basic-info))))

(defvar slow-command-patterns (make-hash-table :test 'equal)
  "Hash table tracking slow command patterns.")

(defvar slow-command-history nil
  "Ring buffer of recent slow commands.")

(defvar slow-command-ring-size 50
  "Maximum number of slow commands to remember.")

(defun slow-command-log (event)
  "Log a slow command event with profiling data."
  (let ((formatted-basic (slow-command-format-event event))
        (formatted-full (slow-command-format-event event t)))

    ;; Show notification
    (when slow-command-show-notifications
      (message "⚠ Slow command: %s took %.2fs | Top: %s"
               (slow-command-event-command event)
               (slow-command-event-duration event)
               (or (slow-command-event-profile-summary event) "no profile")))

    ;; Log to file (full format with profiling data)
    (when slow-command-log-file
      (with-temp-buffer
        (insert formatted-full "\n\n")
        (append-to-file (point-min) (point-max) slow-command-log-file)))

    ;; Add to history ring
    (push event slow-command-history)
    (when (> (length slow-command-history) slow-command-ring-size)
      (setq slow-command-history
            (cl-subseq slow-command-history 0 slow-command-ring-size)))

    ;; Track patterns
    (slow-command-track-patterns (slow-command-event-command event)
                                 (slow-command-event-duration event))))

(defun slow-command-track-patterns (command duration)
  "Track patterns of slow commands."
  (let* ((key (symbol-name command))
         (current-count (gethash key slow-command-patterns 0))
         (new-count (1+ current-count)))
    (puthash key new-count slow-command-patterns)

    (when (and (> new-count 2) (= (mod new-count 3) 0))
      (message "📊 %s has been slow %d times - consider investigating"
               command new-count))))

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
          (profiler-start 'cpu)
          (setq slow-command-profiling-active t))
      (error nil))))

(defun slow-command-after-with-profiling ()
  "Enhanced post-command hook with automatic profiling."
  (when slow-command-last-command
    (let* ((command (nth 0 slow-command-last-command))
           (start-time (nth 1 slow-command-last-command))
           (start-time-precise (nth 2 slow-command-last-command))
           (duration (- (float-time) start-time)))

      ;; Check if this was a slow command
      (if (> duration slow-command-threshold)
          (progn
            ;; Create event with profiling data
            (let ((event (slow-command-create-event command duration)))
              (slow-command-log event)

              ;; Show detailed profiler report for very slow commands
              (when (> duration slow-command-auto-profile-threshold)
                (when (get-buffer "*Profiler-Report*")
                  (pop-to-buffer "*Profiler-Report*")))))

        ;; Command was fast - just clean up profiler
        (when (and slow-command-profiling-active (profiler-running-p))
          (profiler-stop)))

      ;; Clean up
      (setq slow-command-profiling-active nil)
      (setq slow-command-last-command nil))))

;; Utility functions
(defun slow-command-show-log ()
  "Show the slow command log in a buffer."
  (interactive)
  (find-file slow-command-log-file))

(defun slow-command-show-recent-with-profiles ()
  "Show recent slow commands with profiling data."
  (interactive)
  (with-current-buffer (get-buffer-create "*Slow Commands with Profiles*")
    (erase-buffer)
    (insert "Recent Slow Commands with Profiling Data:\n")
    (insert "===============================================\n\n")
    (dolist (event (reverse (cl-subseq slow-command-history 0 (min 10 (length slow-command-history)))))
      (insert (slow-command-format-event event t) "\n\n"))
    (goto-char (point-min))
    (view-mode 1)
    (pop-to-buffer (current-buffer))))

(defun slow-command-show-patterns ()
  "Show slow command patterns with profiling summaries."
  (interactive)
  (with-current-buffer (get-buffer-create "*Slow Command Patterns*")
    (erase-buffer)
    (insert "Slow Command Patterns:\n")
    (insert "======================\n\n")
    (let ((sorted-patterns '()))
      (maphash (lambda (cmd count)
                 (push (cons cmd count) sorted-patterns))
               slow-command-patterns)
      (setq sorted-patterns (sort sorted-patterns (lambda (a b) (> (cdr a) (cdr b)))))
      (dolist (pattern sorted-patterns)
        (insert (format "%s: %d times\n" (car pattern) (cdr pattern)))
        ;; Show profile summary for recent occurrences
        (let ((recent-events (cl-remove-if-not)
                             (lambda (event)
                               (string= (car pattern)
                                        (symbol-name (slow-command-event-command event))))
                             (cl-subseq slow-command-history 0 (min 5 (length slow-command-history)))))
          (when recent-events
            (let ((summary (slow-command-event-profile-summary (car recent-events))))
              (when summary
                (insert (format "  Recent profile: %s\n" summary))))))
        (insert "\n")))
    (pop-to-buffer (current-buffer))))

(defun slow-command-analyze-command (command-symbol)
  "Manually profile a specific command."
  (interactive
   (list (intern (completing-read "Command to analyze: "
                                  obarray 'commandp t))))
  (message "Profiling %s - execute the command now..." command-symbol)
  (profiler-start 'cpu)
  (run-with-timer 30 nil
                  (lambda ()
                    (when (profiler-running-p)
                      (profiler-stop)
                      (profiler-report)
                      (message "Manual profiling completed for %s" command-symbol)))))

;; Enable/disable functions
(defun enable-slow-command-monitoring ()
  "Enable slow command monitoring with automatic profiling."
  (interactive)
  (add-hook 'pre-command-hook #'slow-command-before)
  (add-hook 'post-command-hook #'slow-command-after-with-profiling)
  (message "Slow command monitoring with profiling enabled\nLog file: %s\nThreshold: %.1fs\nAuto-profile all: %s"
           slow-command-log-file
           slow-command-threshold
           (if slow-command-auto-profile-all-commands "yes" "no")))

(defun disable-slow-command-monitoring ()
  "Disable slow command monitoring."
  (interactive)
  (remove-hook 'pre-command-hook #'slow-command-before)
  (remove-hook 'post-command-hook #'slow-command-after-with-profiling)
  (when (profiler-running-p)
    (profiler-stop))
  (setq slow-command-profiling-active nil)
  (message "Slow command monitoring disabled"))

(defun slow-command-configure ()
  "Configure slow command monitoring settings."
  (interactive)
  (setq slow-command-threshold
        (read-number "Threshold (seconds): " slow-command-threshold))
  (setq slow-command-auto-profile-all-commands
        (y-or-n-p "Auto-profile all commands? "))
  (setq slow-command-auto-profile-threshold
        (read-number "Show profiler report threshold (seconds): "
                     slow-command-auto-profile-threshold))
  (message "Configuration updated"))

;; Set up the log file location
(setq slow-command-log-file
      (expand-file-name "slow-commands.log" user-emacs-directory))
