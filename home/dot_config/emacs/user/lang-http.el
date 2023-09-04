;;; lang-http.el --- -*- lexical-binding: t; -*-

(use-package request)

(use-package verb
  :after (org org-babel)
  :commands (verb-command-map)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (org-babel-do-load-languages 'org-babel-load-languages '((verb . t))))

(use-package restclient
  :after (org org-babel))

(use-package ob-restclient
  :after (org org-babel restclient)
  (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))

(provide 'lang-http)

;;; lang-http.el ends here
