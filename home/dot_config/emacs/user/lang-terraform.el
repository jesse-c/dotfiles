;;; lang-terraform.el --- -*- lexical-binding: t; -*-

(use-package terraform-mode
  :after hcl-mode)
(use-package terraform-doc
  :after (terraform-mode org))

(provide 'lang-terraform)

;;; lang-terraform.el ends here
