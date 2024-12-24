;;; ai.el --- -*- lexical-binding: t; -*-

(require 'auth-source)

(defun my/get-password (hostname username)
  "Return the password for the given HOSTNAME and USERNAME from .authinfo file."
  (interactive "sHostname: \nsUsername: ")
  (require 'auth-source)
  (let ((match (auth-source-search :host hostname
                                   :user username
                                   :require '(:secret)
                                   :max 1)))
    (if match
        (funcall (plist-get (car match) :secret))
      (error "No password found for %s@%s in .authinfo" username hostname))))

(use-package gptel
  :after transient
  :commands (gptel
             gptel-menu
             gptel-mode
             gptel-send
             gptel-backend
             gptel-make-anthropic)
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model "claude-3-5-sonnet-20241022")
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude" :stream t :key (my/get-password "anthropic.com" "me"))))

(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :hook
  (org-mode . org-ai-mode)
  :init
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :custom
  (org-ai-openai-api-token (my/get-password "api.openai.com" "me"))
  (org-ai-default-chat-model "gpt-4-turbo"))

(use-package aider
  :after transient
  :ensure
  (:type git :host github :repo "tninja/aider.el" :branch "main" :files ("aider.el")))
  ;; :config)
  ;; (global-set-key (kbd "C-c a") 'aider-transient-menu))

(provide 'ai)

;;; ai.el ends here
