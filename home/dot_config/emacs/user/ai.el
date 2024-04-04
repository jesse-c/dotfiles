;;; ai.el --- -*- lexical-binding: t; -*-

(use-package copilot
  :disabled
  :after jsonrpc
  :ensure
  (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook
  (prog-mode-hook . copilot-mode)
  (text-mode-hook . copilot-mode)
  :custom
  (copilot-enable-predicates '(evil-insert-state-p)))

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

(use-package chatgpt-arcana
  :disabled
  :after all-the-icons
  :ensure
  (:host github :repo "CarlQLange/ChatGPT-Arcana.el" :files ("*.el"))
  :custom
  (chatgpt-arcana-api-key (my/get-password "api.openai.com" "me"))
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
               '(chatgpt-arcana-chat-mode all-the-icons-octicon "comment-discussion" :height 1.0 :v-adjust -0.1 :face all-the-icons-purple)))

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
  (gptel-model "claude-3-opus-20240229") ;  "claude-3-sonnet-20240229" also available
  :config
  (setq gptel-backend (gptel-make-anthropic "Claude" :stream t :key (my/get-password "anthropic.com" "me"))))

(use-package ellama
  :disabled
  :ensure
  (:host github :repo "s-kostyaev/ellama"))

(use-package org-ai
  :disabled)

(use-package llm
  :disabled)

(provide 'ai)

;;; ai.el ends here
