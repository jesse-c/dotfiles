;;; lang-javascript.el --- -*- lexical-binding: t; -*-

(use-package npm
  :disabled
  :after transient
  :commands
  (npm-update
   npm-run
   npm-menu
   npm-init
   npm-install-menu
   npm-mode
   npm))

(provide 'lang-javascript)

;;; lang-javascript.el ends here
