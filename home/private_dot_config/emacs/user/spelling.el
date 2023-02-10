;;; spelling.el --- -*- lexical-binding: t; -*-

(require 'flyspell)

(set-language-environment "UTF-8")

(setq ispell-dictionary "australian")    ;; set the default dictionary
;; Spellchecking requires an external command to be available. Install aspell on your Mac, then make it the default checker for Emacs' ispell. Note that personal dictionary is located at ~/.aspell.LANG.pws by default.
(setq ispell-program-name "aspell")

(use-package languagetool
  :defer t
  :custom
  (languagetool-java-arguments nil)
  (languagetool-console-command "languagetool")
  (languagetool-server-command "languagetool-server")
  (languagetool-server-host "localhost")
  (languagetool-server-port 8081)
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop))

(use-package flycheck-vale
  :after flycheck
  :init (flycheck-vale-setup))

(use-package flycheck-aspell
  :defer 1
  :custom
  ;; If you want to check TeX/LaTeX/ConTeXt buffers
  (add-to-list 'flycheck-checkers 'tex-aspell-dynamic)
  ;; If you want to check Markdown/GFM buffers
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  ;; If you want to check HTML buffers
  (add-to-list 'flycheck-checkers 'html-aspell-dynamic)
  ;; If you want to check XML/SGML buffers
  (add-to-list 'flycheck-checkers 'xml-aspell-dynamic)
  ;; If you want to check Nroff/Troff/Groff buffers
  (add-to-list 'flycheck-checkers 'nroff-aspell-dynamic)
  ;; If you want to check Texinfo buffers
  (add-to-list 'flycheck-checkers 'texinfo-aspell-dynamic)
  ;; If you want to check comments and strings for C-like languages
  (add-to-list 'flycheck-checkers 'c-aspell-dynamic)
  ;; If you want to check message buffers
  (add-to-list 'flycheck-checkers 'mail-aspell-dynamic))

;; Popup window for spellchecking
(use-package flyspell-correct)
(use-package flyspell-correct-popup)

;; Enable spellcheck on the fly for all text modes. This includes org, latex and LaTeX.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Enable right mouse click on macOS to see the list of suggestions.
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; Spellcheck current word
(define-key flyspell-mode-map (kbd "s-\\") 'flyspell-correct-previous-word-generic) ;; Cmd+\ spellcheck word with popup
(define-key flyspell-mode-map (kbd "C-s-\\") 'ispell-word)                          ;; Ctrl+Cmd+\ spellcheck word using built UI

;; Search for synonyms
(use-package powerthesaurus
  :bind
  ("s-|" . powerthesaurus-lookup-word-dwim))

;; Word definition search
(use-package define-word
  :bind
  ("M-\\" . define-word-at-point))

(provide 'spelling)

;;; spelling.el ends here
