;;; projects.el --- -*- lexical-binding: t; -*-

(use-package
  perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c C-z"))
  (persp-state-default-file (concat user-emacs-directory "persp-state"))
  (persp-sort 'access)
  :config
  (persp-mode))

(use-package perspective-tabs
  :elpaca
  (:type git :host sourcehut :repo "woozong/perspective-tabs")
  :after perspective
  :config
  (perspective-tabs-mode +1))

;; CI/CD -----------------------------------------------------------------------

;; It's either the index, [0, ∞), or nil
(let ((match (string-match-p "Knapsack" system-name)))
  (when (or match (eq 0 match))
    (require 'circleci)))

;; TODO Treat it as a package
;; (use-package circleci
;;   :ensure nil
;;   :load-path "~/.config/emacs/circleci.el"
;;   :commands
;;   (my/circleci-get-branch-status
;;    my/circleci-get-current-branch-status
;;    my/circleci-get-branch-latest-workflow
;;    my/circleci-get-current-branch-latest-workflow
;;    my/circleci-rerun-workflow))

;; Process ---------------------------------------------------------------------

(use-package projectile
  :custom
  (projectile-switch-project-action 'projectile-commander)
  (projectile-sort-order 'recently-active)
  (projectile-project-search-path '("~/Documents/projects/" ("~/src/" . 3)))
  (projectile-auto-discover nil)
  :config
  (projectile-mode +1)
  ;; Recommended keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Custom project types
  (projectile-register-project-type
   'zola
   '("config.toml" "content" "static" "templates" "themes")
   :project-file "config.toml"
   :compile "zola build"
   :test "zola check"
   :run "zola server")
  (projectile-register-project-type
   'zig
   '("build.zig")
   :project-file "build.zig"
   :compile "zig build"
   :run "zig build run"))

(use-package ibuffer-projectile
  :after projectile)

(use-package consult-projectile
  :after projectile
  :elpaca
  (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :custom
  (consult-projectile-source-projectile-project-action 'projectile-commander))

(use-package flycheck-projectile
  :defer t
  :after (flycheck projectile))

(use-package ripgrep
  :defer 1)

(provide 'projects)

;;; projects.el ends here
