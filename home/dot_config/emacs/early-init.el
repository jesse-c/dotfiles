;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Code:

;; Using Elpaca instead
(setq package-enable-at-startup nil)

;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Make Emacs Native-compile .elc files asynchronously
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation native-comp-jit-compilation)  ; Deprecated

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

(defvar my/gc-cons-threshold (* 256 1024 1024) ; 256mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
 decrease this. If you experience stuttering, increase this.")

(defvar my/gc-cons-upper-limit most-positive-fixnum
  "The temporary value for `gc-cons-threshold' to defer it.")

(defun my/defer-garbage-collection ()
  (setq gc-cons-threshold my/gc-cons-upper-limit))
(defun my/restore-garbage-collection ()
  ;; Defer it briefly so that commands launched from the minibuffer can enjoy the
  ;; benefits.
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold my/gc-cons-threshold))))

(defun my/restore-startup-optimizations ()
  "Resets garbage collection settings to reasonable defaults (a large `gc-cons-threshold' can cause random freezes otherwise)."

  ;; To speed up minibuffer commands (like Vertico), we defer garbage
  ;; collection while the minibuffer is active.
  (add-hook 'minibuffer-setup-hook #'my/defer-garbage-collection)
  (add-hook 'minibuffer-exit-hook  #'my/restore-garbage-collection)
  ;; Normal hook run when all frames lost input focus.
  (add-hook 'focus-out-hook #'garbage-collect)

  (setq-default gc-cons-threshold my/gc-cons-threshold)

  ;; Do this on idle timer to defer a possible GC pause that could result; also
  ;; allows deferred packages to take advantage of these optimisations.
  (run-with-idle-timer 3 t 'garbage-collect))

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later in
;; `doom|restore-startup-optimizations'.
(setq gc-cons-threshold my/gc-cons-upper-limit)
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

(setq server-socket-dir "~/.config/emacs/server")
(make-directory server-socket-dir t)

(provide 'early-init)

;;; early-init.el ends here
