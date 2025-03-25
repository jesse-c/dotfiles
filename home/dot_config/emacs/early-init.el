;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Code:

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Make Emacs Native-compile .elc files asynchronously
(setq native-comp-jit-compilation t
      native-comp-deferred-compilation t)  ; Deprecated in Emacs > 29.1

;; Suppress native compilation warnings and errors
(setq native-comp-async-report-warnings-errors nil)

;; Ensure that quitting only occurs once Emacs finishes native compiling,
;; preventing incomplete or leftover compilation files in `/tmp`.
(setq native-comp-async-query-on-exit t)
(setq confirm-kill-processes t)

;; Non-nil means to native compile packages as part of their installation.
(setq package-native-compile t)

;; Disable warning [1]
;;
;; [1] https://memopixel.com/fix-package-cl-is-deprecated-warning-for-emacs
(setq byte-compile-warnings '(cl-functions))

;; Don't resize the frame to preserve the number of columns or lines
;; being displayed when setting font, menu bar, tool bar, tab bar,
;; internal borders, fringes, or scroll bars.  Since I use XMonad, this
;; option is i) useless anyways and ii) _terribly_ expensive.
(setq frame-inhibit-implied-resize t)

(defvar my/gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
 decrease this. If you experience stuttering, increase this.")

(defvar my/gc-cons-upper-limit 536870912 ; 512mb
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun my/restore-startup-optimizations ()
  "Resets garbage collection settings to reasonable defaults (a large `gc-cons-threshold' can cause random freezes otherwise)."

  ;; Do this on idle timer to defer a possible GC pause that could result; also
  ;; allows deferred packages to take advantage of these optimizations.
  (run-with-idle-timer
   3 nil
   (lambda ()
     (setq file-name-handler-alist my/file-name-handler-alist)
     (setq-default gc-cons-threshold my/gc-cons-threshold)
     ;; To speed up minibuffer commands (like helm and ivy), we defer garbage
     ;; collection while the minibuffer is active.
     (defun my/defer-garbage-collection ()
       (setq gc-cons-threshold my/gc-cons-upper-limit))
     (defun my/restore-garbage-collection ()
       ;; Defer it so that commands launched from the minibuffer can enjoy the
       ;; benefits.
       (run-at-time 1 nil (lambda () (setq gc-cons-threshold my/gc-cons-threshold))))
     (add-hook 'minibuffer-setup-hook #'my/defer-garbage-collection)
     (add-hook 'minibuffer-exit-hook  #'my/restore-garbage-collection)
     ;; GC all sneaky breeky like
     (add-hook 'focus-out-hook #'garbage-collect))))


;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later in
;; `doom|restore-startup-optimizations'.
(setq gc-cons-threshold my/gc-cons-upper-limit)
;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(setq file-name-handler-alist nil)
;; Not restoring these to their defaults will cause stuttering/freezes.
(add-hook 'after-init-hook #'my/restore-startup-optimizations)

(unless (or (daemonp) noninteractive)

  ;; Improves startup speed by not looking a bunch of file handlers
  ;; for every require statement.
  ;; NOTE: Breaks systems that use *.el.gz rather than byte-compiling.
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (defun doom-reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h 101))

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  ;; Report how long it took to load.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done))))

(setq package-quickstart t)

(provide 'early-init)

;;; early-init.el ends here
