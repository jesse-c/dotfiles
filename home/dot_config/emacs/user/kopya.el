;;; kopya.el --- Kopya Emacs integration -*- lexical-binding: t -*-

;; Load required libraries
(require 'cl-lib)
(require 'pdd)

;; Define the kopya group for customization
(defgroup kopya nil
  "Kopya clipboard API integration."
  :group 'external)

;; Define the base URL for the Kopya API
(defcustom kopya-api-url "http://localhost:9090"
  "Base URL for the Kopya API."
  :type 'string)

;; Define a struct to represent a clipboard entry from the Kopya API
;; This struct contains the following fields:
;; - id: unique identifier for the entry
;; - content: the actual clipboard content
;; - type: type of the clipboard content (e.g. text, image)
;; - human-readable-type: human-readable description of the type
;; - timestamp: timestamp when the entry was created
;; - is-textual: whether the content is textual or not
(cl-defstruct clipboard-entry
  id
  content
  type
  human-readable-type
  timestamp
  is-textual)

;; Convert a single alist (from API) to a clipboard-entry struct
(defun kopya--alist-to-clipboard-entry (alist)
  "Convert ALIST to a `clipboard-entry' struct."
  (make-clipboard-entry
   :id (alist-get 'id alist)
   :content (alist-get 'content alist)
   :type (alist-get 'type alist)
   :human-readable-type (alist-get 'humanReadableType alist)
   :timestamp (alist-get 'timestamp alist)
   :is-textual (alist-get 'isTextual alist)))

;; Convert the full API response to (:entries (list of clipboard-entry) :total N)
(defun kopya--parse-history-response (json)
  "Convert pdd.el response to (:entries (list of clipboard-entry) :total N)."
  (let* ((entries-vec (alist-get 'entries json))
         ;; Convert vector to list if needed
         (entries-list (if (vectorp entries-vec)
                           (append entries-vec nil)
                         entries-vec))
         (entries (mapcar #'kopya--alist-to-clipboard-entry entries-list))
         (total (alist-get 'total json)))
    (list :entries entries :total total)))

;; Get clipboard history from Kopya API
;; This function takes a plist of arguments:
;; - :query: optional search string
;; - :type: optional filter by type
;; - :limit: optional result limit
;; - :callback: function to call with result
;; The callback function is called with a plist containing:
;; - :entries: list of clipboard-entry structs
;; - :total: total number of entries
(defun kopya-get-history (&rest args)
  "Get clipboard history from Kopya API.
ARGS is a plist: :query, :type, :limit, :callback.
CALLBACK is called with (:entries (list of clipboard-entry) :total N).

Example usage:
  (kopya-get-history :limit 1 :callback #'pp)
  (kopya-get-history :query \"foo\" :callback (lambda (r) (message \"%S\" r)))"
  (let* ((query (plist-get args :query))   ;; Optional search string
         (type (plist-get args :type))     ;; Optional filter by type
         (limit (plist-get args :limit))   ;; Optional result limit
         (callback (plist-get args :callback)) ;; Function to call with result
         (endpoint (if query "search" "history")) ;; Use /search if query, else /history
         (params (delq nil
                       (list (when query (cons 'query query))
                             (when type (cons 'type type))
                             (when limit (cons 'limit (number-to-string limit))))))
         (url (concat (replace-regexp-in-string "/$" "" kopya-api-url)
                      "/" endpoint)))
    ;; Make HTTP request via pdd.el
    (pdd url
      :params params
      :done (lambda (json)
              ;; Parse and return result as structs
              (when callback
                (funcall callback (kopya--parse-history-response json)))))))

;; Function to extract the text content of the last clipboard entry from history
(defun kopya-get-last-text-content (callback)
  "Fetch the last clipboard entry and pass its text content to CALLBACK.
If the entry is not textual, passes nil. CALLBACK is called with one argument."
  (let ((cb callback))
    (kopya-get-history
     :limit 1
     :callback (lambda (result)
                 (let* ((entry (car (plist-get result :entries)))
                        (is-textual (and entry (clipboard-entry-is-textual entry)))
                        (content (and is-textual (clipboard-entry-content entry))))
                   (funcall cb content))))))

(defun kopya-get-last-text-content-sync-error (&optional timeout)
  "Synchronously fetch the last clipboard text content from history.
Returns the string, or signals an error if not available within TIMEOUT seconds (default 3).
This version uses `accept-process-output' to better integrate with the Emacs event loop."
  (let ((result :kopya--pending)
        (timeout (or timeout 3))
        (start-time (float-time)))
    (kopya-get-last-text-content (lambda (text) (setq result text)))
    (while (and (eq result :kopya--pending)
                (< (- (float-time) start-time) timeout))
      ;; Process events and timers while waiting
      (accept-process-output nil 0.05))
    (if (eq result :kopya--pending)
        (error "kopya-get-last-text-content-sync-error: Timed out after %s seconds" timeout)
      result)))

;; Example:
;; (kopya-get-last-text-content #'message)
;; (kopya-get-last-text-content #'identity)
;; (kopya-get-last-text-content-sync)
;; (kopya-get-last-text-content-sync-error) ; Waits up to 3s, returns string or signals error

;; Example: pretty-print the result for a single entry
;; (kopya-get-history :limit 1 :callback #'pp)

;; Example: show first entry in minibuffer
;; (kopya-get-history
;;  :limit 1
;;  :callback (lambda (result)
;;              (let ((entry (car (plist-get result :entries))))
;;                (message "First entry: %S" entry))))

(defun kopya-get-last-text-to-killring ()
  "Get the last saved text and put it on the killring."
  (interactive)
  (kill-new (kopya-get-last-text-content-sync-error)))

(provide 'kopya)
;;; kopya.el ends here
