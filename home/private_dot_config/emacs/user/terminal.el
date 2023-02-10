(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :branch "master")
  :defer 1
  :hook
  (eshell-load-hook . eat-eshell-mode))
