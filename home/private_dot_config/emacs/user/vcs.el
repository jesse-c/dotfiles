;;; vcs.el --- -*- lexical-binding: t; -*-

;; Magit
(use-package magit
  :config
  ;; Setting ‘forge-add-default-bindings’ to nil in ‘evil-collection-forge-setup’.
  ;; To suppress this message you can set this variable to nil in your init.el file.
  (setq forge-add-default-bindings nil)
  :bind
  ("s-g" . magit-status))

(use-package magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode))

(use-package forge
  :after magit
  :defer 1
  :config
  (setq auth-sources '("~/.authinfo")))

;; Show changes in the gutter
(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode +1))

(use-package git-modes)

;; Show author
(use-package blamer
  :init (global-blamer-mode 1))

;; Create URLs to files and commits in repository hosting services
(use-package git-link
  :defer 1
  :config
  (setq git-link-use-commit t))

;; Browse target page on github/bitbucket from emacs buffers
(use-package browse-at-remote)

;; Why was this line changed
(use-package git-messenger)

;; View versions of a file
;; (use-package git-timemachine)

;; View git blame
(use-package vc-msg)

(provide 'vcs)

;;; vcs.el ends here
