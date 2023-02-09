;;; init.el --- My config -*- lexical-binding: t; -*-

;;; Commentary:

;;; My config.

;;; Code:

;; -----------------------------------------------------------------------------
;; Init
;; -----------------------------------------------------------------------------

;; Store custom-file separately, don't freak out when it's not found
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))
(setq read-process-output-max (* 1024 1024))

;; -----------------------------------------------------------------------------
;; Package management
;; -----------------------------------------------------------------------------

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "repos/elpaca/" elpaca-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--")))))
          (progn
            (byte-recompile-directory repo 0 'force)
            (require 'elpaca)
            (and (fboundp 'elpaca-generate-autoloads)
                 (elpaca-generate-autoloads "elpaca" repo))
            (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error)
     (warn "%s" err)
     (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Block until current queue processed.
(elpaca-wait)

;; -----------------------------------------------------------------------------
;; Common
;; -----------------------------------------------------------------------------

(elpaca ht)
(elpaca dash (require 'dash))
(elpaca loop)
(elpaca s)
(elpaca crux)
(elpaca request)
(elpaca restclient)
(elpaca async)
(elpaca load-relative)
(elpaca f)

;; -----------------------------------------------------------------------------
;; Server
;; -----------------------------------------------------------------------------

;; Hint: emacsclient -n file1 file2 (Use -c to open in a new frame)
;; Example: emacsclient --socket-name ~/.config/emacs/server/server FILE

(setq server-socket-path "~/.config/emacs/server") ; Make the directory if it doesn't exist
(make-directory server-socket-path t)
(setq server-socket-dir server-socket-path)

(load "server")
(unless (< emacs-major-version 23)
  (defun server-ensure-safe-dir (_dir) "Noop" t))

(unless (server-running-p) (server-start))

;; -----------------------------------------------------------------------------
;; OS
;; -----------------------------------------------------------------------------

;; Disabling suspend-frame binding
;; Very annoying binding, lets get rid of it.
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#disabling-suspend-frame-binding
(global-unset-key (kbd "C-z"))

(elpaca chezmoi)

;; macOS

(when (memq window-system '(mac ns))
  (setq dired-use-ls-dired nil))

(setq delete-by-moving-to-trash t)

;; https://emacs.stackexchange.com/a/41767
;; Alternative to try: https://superuser.com/questions/125569/how-to-fix-emacs-popup-dialogs-on-mac-os-x
(setq use-dialog-box nil)

;; Ensure PATH is correct when launched as GUI application
(elpaca
  exec-path-from-shell
    (when (memq window-system '(mac ns))
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize)))

(when (memq window-system '(mac ns))
  (setq dired-use-ls-dired nil))

;; https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
(defun my/file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; All

(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)

;; Tidy up .emacs.d mess
(elpaca no-littering)
(savehist-mode)

;; -----------------------------------------------------------------------------
;; GUI
;; -----------------------------------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<s-mouse-1>") 'browse-url-at-mouse)

;; Setting default coding system
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#setting-default-coding-system
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Focus new frame
;; https://github.com/shfx/emacs.d/blob/8715ced2c49ba2f693ad965f2c0b4c1b44c829c8/README.org#focus-new-frame
(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
  (when (display-graphic-p)
    (ns-raise-emacs)))

;; Themes
(defun my/theme-by-current-time ()
  "Get the light or dark THEME based on the current time."
  (let ((hour (->> (current-time)
                   (decode-time)
                   (nth 2))))
    (if (or (> hour 18) (< hour 5))
        'modus-vivendi
      'modus-operandi)))

(defun my/load-theme-by-current-time ()
  "Load the right theme based on the current time."
  (load-theme (my/theme-by-current-time) t))

(defun my/disable-all-themes ()
  "Disable all enabled themes."
  (interactive)
  ;; List of enabled Custom Themes, highest precedence
  ;; first.
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my/disable-and-load-theme (theme)
  "Disable all themes and then load theme THEME."
  (interactive)
  (my/disable-all-themes)
  (load-theme theme))

(elpaca spacemacs-theme)
  ;; (my/load-theme-by-current-time))

(elpaca
  modus-themes
    (setq modus-operandi-theme-faint-syntax t)
    (setq modus-vivendi-theme-faint-syntax t)
    (my/load-theme-by-current-time))

(elpaca doom-themes)

;; Disabled as AppearanceNotifier is used
;; (run-with-timer 0 (* 5 60) 'my/load-theme-by-current-time)

;; Disabled while I use a different distribution
;; (defun my/apply-theme (appearance)
;;   "Load theme, taking current system APPEARANCE into consideration."
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (pcase appearance
;;     ('light (load-theme 'modus-operandi t))
;;     ('dark (load-theme 'modus-vivendi t))))
;;
;; (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

;; Font
(set-frame-font "JetBrains Mono 12" nil t)

(load "~/.config/emacs/ligatures.el")

(elpaca
  (ligature
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    ;; Enable all Cascadia Code ligatures in programming modes
    (ligature-set-ligatures 'prog-mode jetbrains-ligature-mode--ligatures)
    ;; Enables ligature checks globally in all buffers. You can also do it
    ;; per mode with `ligature-mode'.
    (global-ligature-mode t)))

;; Default frame size
(add-to-list 'default-frame-alist '(height . 100))
(add-to-list 'default-frame-alist '(width . 120))

;; Initial frame content
(setq inhibit-splash-screen t) ; No startup screen
(setq inhibit-startup-message t) ; No startup message
(setq initial-scratch-message nil) ; Empty scratch buffer

;; Keyboard-centric user interface
(scroll-bar-mode  -1)
(tool-bar-mode    -1)
(tooltip-mode     -1)
(menu-bar-mode    -1)
(setq visible-bell 1)

(setq confirm-kill-emacs 'y-or-n-p) ; y and n instead of yes and no when quitting

;; Use ESC as universal get me out of here command
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Tab-bar
(setq tab-bar-tab-hints t) ; Show tab numbers
(setq tab-bar-mode t)
(setq tab-bar-show nil)
(setq tab-bar-new-tab-choice "*scratch*")

(global-set-key (kbd "C-<tab>") 'tab-bar-switch-to-tab)

;; Mode-line
(elpaca
  (doom-modeline
    (doom-modeline-mode 1)))

;; Windows
(global-set-key (kbd "C-h") 'windmove-left) ; Use F1 instead of C-h for help-command
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

(global-set-key (kbd "C-q") 'delete-window)

(defun my/split-window-below-and-move ()
  "Make the new split focused."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun my/split-window-right-and-move ()
  "Make the new split focused."
  (interactive)
  (split-window-right)
  (other-window 1))

(global-set-key (kbd "C-S-x") 'my/split-window-below-and-move)
(global-set-key (kbd "C-S-v") 'my/split-window-right-and-move)

(elpaca zoom-window)

;; Keymaps
(elpaca hydra)

;; Buffers
(defun my/clean-up-buffers ()
  "Clean-up buffers that have built up."
  (interactive)
  (kill-matching-buffers ".*\.ex" nil t)
  (kill-matching-buffers ".*\.exs" nil t)
  (kill-matching-buffers ".*\.yaml" nil t))

;; -----------------------------------------------------------------------------
;; Editor
;; -----------------------------------------------------------------------------

;; By default, Emacs thinks a sentence is a full-stop followed by 2 spaces. Let’s make it full-stop and 1 space.
;; http://sriramkswamy.github.io/dotemacs/
(setq sentence-end-double-space nil)

;; Bindings
(elpaca
  which-key
    (which-key-mode))

;; Indentation
(elpaca
  indent-guide
    (indent-guide-global-mode))

(setq-default indent-tabs-mode nil)

(elpaca
  aggressive-indent
    (add-hook 'elixir-mode-hook #'aggressive-indent-mode)
    (add-hook 'rust-mode-hook #'aggressive-indent-mode)
    (add-hook 'swift-mode-hook #'aggressive-indent-mode))

;; Folding
(elpaca origami)

;; Search
(setq isearch-lazy-count t)

(elpaca
  perspective
    (setq persp-mode-prefix-key (kbd "C-c C-z"))
    (setq persp-state-default-file (concat user-emacs-directory "persp-state"))
    (setq persp-sort 'access)
    (persp-mode))

(elpaca comby)

;; Tree-sitter
(elpaca
  treesit-auto
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode))

;; LSP
(elpaca
  nil
  (require 'eglot)
  ;; See: https://github.com/neovim/nvim-lspconfig/tree/89a19315ef4064a144b3d7d1c9a7eefd0e91e51b/lua/lspconfig/server_configurations
  (add-to-list 'eglot-server-programs
               `(toml-mode "taplo" "lsp" "stdio"))
  (add-to-list 'eglot-server-programs
               `(elixir-mode "elixir-ls"))
  (add-to-list 'eglot-server-programs
               `(xml-mode "lemminx"))
  (add-to-list 'eglot-server-programs
               `(go-mode "gopls"))
  (add-to-list 'eglot-server-programs
               `(bash-mode "bash-language-server" "start"))
  (add-to-list 'eglot-server-programs
               `(clojure-mode "clojure-lsp"))
  (add-to-list 'eglot-server-programs
               `(latex-mode "ltex-ls"))
  (add-to-list 'eglot-server-programs
               `(lua-mode "lua-language-server"))
  (add-to-list 'eglot-server-programs
               `(markdown-mode "prosemd-lsp" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(python-mode "pylsp"))
  (add-to-list 'eglot-server-programs
               `(rust-mode "rust-analyzer"))
  ;; And/Or Sorbet for Ruby?
  (add-to-list 'eglot-server-programs
               `(ruby-mode "solargraph" "stdio"))
  (add-to-list 'eglot-server-programs
               `(sql-mode "sqls"))
  (add-to-list 'eglot-server-programs
               `(typescript-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(typescript-ts-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(tsx-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(tsx-ts-mode "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(html-mode "vscode-html-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(web-mode "vscode-html-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(css-mode "vscode-cs-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(json-mode "vscode-json-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(yaml-mode "yaml-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
               `(terraform-mode "terraform-ls" "serve"))
  (add-to-list 'eglot-server-programs
               `(swift-mode "sourcekit-lsp"))
  (add-hook 'toml-mode-hook #'eglot-ensure)
  (add-hook 'elixir-mode-hook #'eglot-ensure)
  (add-hook 'xml-mode-hook #'eglot-ensure)
  (add-hook 'go-mode-hook #'eglot-ensure)
  (add-hook 'bash-mode-hook #'eglot-ensure)
  (add-hook 'clojure-mode-hook #'eglot-ensure)
  (add-hook 'latex-mode-hook #'eglot-ensure)
  (add-hook 'lua-mode-hook #'eglot-ensure)
  (add-hook 'markdown-mode-hook #'eglot-ensure)
  (add-hook 'python-mode-hook #'eglot-ensure)
  (add-hook 'rust-mode-hook #'eglot-ensure)
  (add-hook 'ruby-mode-hook #'eglot-ensure)
  (add-hook 'sql-mode-hook #'eglot-ensure)
  (add-hook 'typescript-mode-hook #'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  (add-hook 'tsx-mode-hook #'eglot-ensure)
  (add-hook 'tsx-ts-mode-hook #'eglot-ensure)
  (add-hook 'html-mode-hook #'eglot-ensure)
  (add-hook 'web-mode-hook #'eglot-ensure)
  (add-hook 'css-mode-hook #'eglot-ensure)
  (add-hook 'json-mode-hook #'eglot-ensure)
  (add-hook 'yaml-mode-hook #'eglot-ensure)
  (add-hook 'terraform-mode-hook #'eglot-ensure)
  (add-hook 'swift-mode-hook #'eglot-ensure))

;; Whitespace
(setq-default require-final-newline t) ; Add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Pretty parens
(elpaca
  rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Structured editing
(elpaca
  puni
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

(elpaca
  (combobulate
     :host github
     :repo "mickeynp/combobulate"
     :branch "master")

  (add-hook 'python-ts-mode #'combobulate-mode)
  (add-hook 'js-ts-mode #'combobulate-mode)
  (add-hook 'css-ts-mode #'combobulate-mode)
  (add-hook 'yaml-ts-mode #'combobulate-mode)
  (add-hook 'typescript-ts-mode #'combobulate-mode)
  (add-hook 'tsx-ts-mode #'combobulate-mode))

(elpaca
  parinfer-rust-mode
  (add-hook 'emacs-lisp-mode #'parinfer-rust-mode))

;; Comments
;; Comment line or region
(global-set-key (kbd "s-/") 'comment-line)

(elpaca
  embark
  (global-set-key (kbd "C-.") 'embark-act)         ;; pick some comfortable binding
  ;; ("C-;" . embark-dwim))        ;; good alternative: M-.
  ;; Disabled for now whilst I use C-h for window switching
  ;; ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(elpaca
  embark-consult
  (add-hook 'embark-collect-mode #'consult-preview-at-point-mode))

;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
 The which-key help message will show the type and value of the
 current target followed by an ellipsis if there are further
 targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
      '(embark-which-key-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

;; Spellchecking and thesaurus -------------------------------------------------

(setq ispell-dictionary "australian")    ;; set the default dictionary
;; Spellchecking requires an external command to be available. Install aspell on your Mac, then make it the default checker for Emacs' ispell. Note that personal dictionary is located at ~/.aspell.LANG.pws by default.
(setq ispell-program-name "aspell")

(elpaca
  languagetool
  (setq languagetool-java-arguments nil)
  (setq languagetool-console-command "languagetool")
  (setq languagetool-server-command "languagetool-server")
  (setq languagetool-server-host "localhost")
  (setq languagetool-server-port 8081))

(elpaca
  flycheck-vale
  (flycheck-vale-setup))

(elpaca
  flycheck-aspell
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
(require 'flyspell)
(elpaca flyspell-correct)
(elpaca flyspell-correct-popup)

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
(elpaca
  powerthesaurus
  (global-set-key (kbd "s-|") 'powerthesaurus-lookup-word-dwim))

;; Word definition search
(elpaca
  define-word
  :bind
  (global-set-key (kbd "M-\\") 'define-word-at-point))

;; Highlight current line
(global-hl-line-mode t)

;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(setq column-number-mode t)

;; Undo
;; Linear undo and redo.
(elpaca
  undo-tree
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-history-directory-alist '(("." . "~/.config/emacs/tmp/undo"))
          undo-tree-auto-save-history t
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t)))

(elpaca goto-chg)

;; Vim
(elpaca
  evil
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  (global-unset-key (kbd "C-u"))
  (global-set-key (kbd "C-u") 'evil-scroll-up)
  (evil-set-undo-system 'undo-tree))

(elpaca
  evil-visual-mark-mode
  (evil-visual-mark-mode))

(elpaca
  evil-collection
  (evil-collection-init))

;; Programming

(defun my/find-alternate-file ()
  "Find alternate FILE, if any."
  (interactive)
  (let* ((default-directory (projectile-project-root))
         ;; https://emacs.stackexchange.com/questions/45419/get-file-name-relative-to-projectile-root
         (buffile (file-relative-name buffer-file-name (projectile-project-root)))
         (cmd (format "alt %s" buffile))
         (output (shell-command-to-string cmd)))
    (if (string= output "")
        (message "No alternate file found")
      (if (y-or-n-p (format "Found alternate file %s. Open?" output))
          (find-file output (message "Not opening"))))))

;; Languages

(elpaca quickrun)

;; Rust
(elpaca rust-mode)
(elpaca rustic
  (setq rustic-lsp-client 'eglot)
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1))))
(elpaca cargo
  (add-hook 'rust-mode-hook #'cargo-minor-mode))
(elpaca
  flycheck-rust
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Web
(elpaca
  web-mode
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; Gradle
(elpaca gradle-mode)
(elpaca flycheck-gradle)

;; Elixir
(elpaca elixir-mode)
(elpaca
    (apprentice
     :host github
     :repo "Sasanidas/Apprentice"
     :branch "master"))
(elpaca
  mix
  (add-hook 'elixir-mode-hook #'mix-minor-mode))
(elpaca
  exunit
  (add-hook 'elixir-mode-hook #'exunit-mode))
(elpaca
  flycheck-credo
  (flycheck-credo-setup)
  (setq flycheck-elixir-credo-strict t))
(elpaca
  flycheck-elixir
  (add-hook 'elixir-mode-hook #'flycheck-mode))
(elpaca
  flycheck-dialyxir
  (add-hook 'elixir-mode-hook #'flycheck-mode))
(elpaca
  flycheck-dialyzer
  (add-hook 'elixir-mode-hook #'flycheck-mode))

;; Go
(elpaca go-mode)

;; Swift
(elpaca swift-mode)
(elpaca flycheck-swift)

;; JSON
(elpaca json-mode)

;; Python
(elpaca python-mode)

;; Protobuf
(elpaca protobuf-mode)

;; CSV
(elpaca csv-mode)

;; Markdown
(elpaca markdown-mode)

;; YAML
(elpaca yaml-mode)

;; HCL
(elpaca hcl-mode)

;; Terraform
(elpaca terraform-mode)
(elpaca terraform-doc)

;; GraphQL
(elpaca graphql-mode)

;; Lua
(elpaca lua-mode)

;; Shell

;; Clojure
(elpaca clojure-mode)
(elpaca clojure-snippets)

(elpaca
  cider
  (setq nrepl-log-messages t)
  (eval-after-load 'cider
    (flycheck-clojure-setup))) ;; run setup *after* cider load

(elpaca
  flycheck-clojure
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; .env
(elpaca dotenv-mode)

;; SASS
(elpaca sass-mode)

;; Javascript
(elpaca npm)

;; Typescript
(elpaca typescript-mode)
;; (elpaca
;;     (tsx-mode
;;      :host github
;;      :repo "orzechowskid/tsx-mode.el"
;;      :branch "emacs29"))

;; Docker
(elpaca docker)
(elpaca dockerfile-mode)
(elpaca docker-compose-mode)

;; Caddy
(elpaca
  caddyfile-mode)
  ;; (("Caddyfile\\'" . caddyfile-mode))
  ;; ("caddy\\.conf\\'" . caddyfile-mode))

;; Emacs Lisp
(elpaca
  eldoc
  (add-hook 'emacs-lisp-mode #'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode #'turn-on-eldoc-mode))

;; Syntax
(elpaca
  flycheck
  (global-flycheck-mode))

;; Formatting
;; Preferring apheleia
(elpaca format-all)

(elpaca
  apheleia
  (require 'apheleia)
  (add-to-list 'apheleia-mode-alist
               `(tsx-mode . prettier-typescript))
  (add-to-list 'apheleia-mode-alist
               `(tsx-ts-mode . prettier-typescript))
  (add-hook 'elixir-mode #'apheleia-mode)
  (add-hook 'swift-mode #'apheleia-mode)
  (add-hook 'rust-mode #'apheleia-mode)
  (add-hook 'javascript-mode #'apheleia-mode)
  (add-hook 'sass-mode #'apheleia-mode)
  (add-hook 'typescript-mode #'apheleia-mode)
  (add-hook 'tsx-mode #'apheleia-mode))

;; Testing
(elpaca coverlay)

;; Snippets
(elpaca tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  (global-set-key (kbd "M-+") 'tempel-complete) ;; Alternative tempel-expand
  (global-set-key (kbd "M-*") 'tempel-insert)

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  (global-tempel-abbrev-mode))

;; The package is young and doesn't have comprehensive coverage.
(elpaca tempel-collection)

;; Completion
(elpaca
  consult
  ;; C-c bindings (mode-specific-map)
  (global-set-key (kbd "C-c h") 'consult-history)
  (global-set-key (kbd "C-c m") 'consult-mode-command)
  (global-set-key (kbd "C-c k") 'consult-kmacro)
  ;; C-x bindings (ctl-x-map)
  (global-set-key (kbd "C-x M-:") 'consult-complex-command)     ;; orig. repeat-complex-command
  (global-set-key (kbd "C-x b") 'consult-buffer)                ;; orig. switch-to-buffer
  (global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  (global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  (global-set-key (kbd "C-x r b") 'consult-bookmark)            ;; orig. bookmark-jump
  (global-set-key (kbd "C-x p b") 'consult-project-buffer)      ;; orig. project-switch-to-buffer
  ;; Custom M-# bindings for fast register access
  (global-set-key (kbd "M-#") 'consult-register-load)
  (global-set-key (kbd "M-'") 'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  (global-set-key (kbd "C-M-#") 'consult-register)
  ;; Other custom bindings
  (global-set-key (kbd "M-y") 'consult-yank-pop)                ;; orig. yank-pop
  ;; M-g bindings (goto-map)
  (global-set-key (kbd "M-g e") 'consult-compile-error)
  (global-set-key (kbd "M-g f") 'consult-flycheck)
  (global-set-key (kbd "M-g g") 'consult-goto-line)             ;; orig. goto-line
  (global-set-key (kbd "M-g M-g") 'consult-goto-line)           ;; orig. goto-line
  (global-set-key (kbd "M-g o") 'consult-outline)               ;; Alternative: consult-org-heading
  (global-set-key (kbd "M-g m") 'consult-mark)
  (global-set-key (kbd "M-g k") 'consult-global-mark)
  (global-set-key (kbd "M-g i") 'consult-imenu)
  (global-set-key (kbd "M-g I") 'consult-imenu-multi)
  ;; M-s bindings (search-map)
  (global-set-key (kbd "M-s d") 'consult-find)
  (global-set-key (kbd "M-s D") 'consult-locate)
  (global-set-key (kbd "M-s g") 'consult-grep)
  (global-set-key (kbd "M-s G") 'consult-git-grep)
  (global-set-key (kbd "M-s r") 'consult-ripgrep)
  (global-set-key (kbd "M-s l") 'consult-line)
  (global-set-key (kbd "M-s L") 'consult-line-multi)
  (global-set-key (kbd "M-s k") 'consult-keep-lines)
  (global-set-key (kbd "M-s u") 'consult-focus-lines)
  ;; Isearch integration
  (global-set-key (kbd "M-s e") 'consult-isearch-history)
  (global-set-key (kbd "M-e") 'consult-isearch-history)         ;; orig. isearch-edit-string
  (global-set-key (kbd "M-s e") 'consult-isearch-history)       ;; orig. isearch-edit-string
  (global-set-key (kbd "M-s l") 'consult-line)                  ;; needed by consult-line to detect isearch
  (global-set-key (kbd "M-s L") 'consult-line-multi)            ;; needed by consult-line to detect isearch
  ;; Minibuffer history
  (global-set-key (kbd "M-s") 'consult-history)                 ;; orig. next-matching-history-element
  (global-set-key (kbd "M-r") 'consult-history)                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  (add-hook 'completion-list-mode #'consult-preview-at-point-mode)

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (require 'consult) 
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))

(elpaca consult-flycheck)

(elpaca
  vertico
  (vertico-mode))

;; Enable rich annotations using the Marginalia package
(elpaca
  marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  (global-set-key (kbd "M-A") 'marginalia-cycle)
  ;; :map minibuffer-local-map)
  ;; ("M-A" . marginalia-cycle))
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(elpaca
  orderless
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion))))
  (setq completion-category-overrides '((eglot (styles orderless)))))

(elpaca
  corfu
     ;; https://github.com/purplg/dotfiles/blob/04c5217247a738adef11d9bf569a329c7eebae4a/.config/emacs/modules/pg-completion.el
     ;; :files (:defaults "extensions/corfu-popupinfo.el")
     ;; Optional customizations
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  (setq corfu-auto-delay 0.2)
  (setq corfu-popupinfo-delay 0.4)
  (setq corfu-separator ?\s)          ;; Orderless field separator
  (setq corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (setq corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
  (setq corfu-scroll-margin 5)        ;; Use scroll margin
  ;; :hook
  ;; (corfu-mode . corfu-popupinfo-mode)
  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  (global-corfu-mode))
  ;; :bind
  ;; (:map corfu-map
  ;;       ("M-n" . corfu-popupinfo-scroll-up)
  ;;       ("M-p" . corfu-popupinfo-scroll-down)
  ;;       ("M-a" . corfu-popupinfo-beginning)
  ;;       ("M-e" . corfu-popupinfo-end)
  ;;       ("M-l" . corfu-popupinfo-location)
  ;;       ("M-d" . corfu-popupinfo-documentation)
  ;;       ("M-t" . corfu-popupinfo-toggle)))

(elpaca
  cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  (global-set-key (kbd "C-c p p") 'completion-at-point) ;; capf
  (global-set-key (kbd "C-c p t") 'complete-tag)        ;; etags
  (global-set-key (kbd "C-c p d") 'cape-dabbrev)        ;; or dabbrev-completion
  (global-set-key (kbd "C-c p h") 'cape-history)
  (global-set-key (kbd "C-c p f") 'cape-file)
  (global-set-key (kbd "C-c p k") 'cape-keyword)
  (global-set-key (kbd "C-c p s") 'cape-symbol)
  (global-set-key (kbd "C-c p a") 'cape-abbrev)
  (global-set-key (kbd "C-c p i") 'cape-ispell)
  (global-set-key (kbd "C-c p l") 'cape-line)
  (global-set-key (kbd "C-c p w") 'cape-dict)
  (global-set-key (kbd "C-c p \\") 'cape-tex)
  (global-set-key (kbd "C-c p _") 'cape-tex)
  (global-set-key (kbd "C-c p ^") 'cape-tex)
  (global-set-key (kbd "C-c p &") 'cape-sgml)
  (global-set-key (kbd "C-c p r") 'cape-rfc1345)
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(elpaca
  kind-icon
  (setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(elpaca all-the-icons)

(elpaca
  all-the-icons-completion
  (add-hook 'marginalia-mode #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode))

(elpaca smex)  ;; show recent commands when invoking Alt-x (or Cmd+Shift+p)
(elpaca avy)   ;; enable avy for quick navigation

;; -----------------------------------------------------------------------------
;; File system
;; -----------------------------------------------------------------------------

(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))
(setq create-lockfiles nil)

(defun my/copy-buffer-name ()
  "Copy the buffer name."
  (interactive)
  (kill-new (buffer-file-name)))

(defun my/copy-project-buffer-name ()
  "Copy the buffer name relative to the project."
  (interactive)
  (kill-new (file-relative-name buffer-file-name (project-root))))

(recentf-mode 1)

(save-place-mode 1)

(setq dired-kill-when-opening-new-dired-buffer t)

(elpaca
  treemacs
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  (with-eval-after-load 'treemacs
    (progn
      (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
            treemacs-deferred-git-apply-delay        0.5
            treemacs-directory-name-transformer      #'identity
            treemacs-display-in-side-window          t
            treemacs-eldoc-display                   'simple
            treemacs-file-event-delay                2000
            treemacs-file-extension-regex            treemacs-last-period-regex-value
            treemacs-file-follow-delay               0.2
            treemacs-file-name-transformer           #'identity
            treemacs-follow-after-init               t
            treemacs-expand-after-init               t
            treemacs-find-workspace-method           'find-for-file-or-pick-first
            treemacs-git-command-pipe                ""
            treemacs-goto-tag-strategy               'refetch-index
            treemacs-header-scroll-indicators        '(nil . "^^^^^^")
            treemacs-hide-dot-git-directory          t
            treemacs-indentation                     2
            treemacs-indentation-string              " "
            treemacs-is-never-other-window           nil
            treemacs-max-git-entries                 5000
            treemacs-missing-project-action          'ask
            treemacs-move-forward-on-expand          nil
            treemacs-no-png-images                   nil
            treemacs-no-delete-other-windows         t
            treemacs-project-follow-cleanup          nil
            treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
            treemacs-position                        'left
            treemacs-read-string-input               'from-child-frame
            treemacs-recenter-distance               0.1
            treemacs-recenter-after-file-follow      nil
            treemacs-recenter-after-tag-follow       nil
            treemacs-recenter-after-project-jump     'always
            treemacs-recenter-after-project-expand   'on-distance
            treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
            treemacs-show-cursor                     nil
            treemacs-show-hidden-files               t
            treemacs-silent-filewatch                nil
            treemacs-silent-refresh                  nil
            treemacs-sorting                         'alphabetic-asc
            treemacs-select-when-already-in-treemacs 'move-back
            treemacs-space-between-root-nodes        t
            treemacs-tag-follow-cleanup              t
            treemacs-tag-follow-delay                1.5
            treemacs-text-scale                      nil
            treemacs-user-mode-line-format           nil
            treemacs-user-header-line-format         nil
            treemacs-wide-toggle-width               70
            treemacs-width                           35
            treemacs-width-increment                 1
            treemacs-width-is-initially-locked       t
            treemacs-workspace-switch-cleanup        nil)

      ;; The default width and height of the icons is 22 pixels. If you are
      ;; using a Hi-DPI display, uncomment this to double the icon size.
      ;;(treemacs-resize-icons 44)

      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-fringe-indicator-mode 'always)
      (when treemacs-python-executable
        (treemacs-git-commit-diff-mode t))

      (pcase (cons (not (null (executable-find "git")))
                   (not (null treemacs-python-executable)))
        (`(t . t)
         (treemacs-git-mode 'deferred))
        (`(t . _)
         (treemacs-git-mode 'simple)))

      (treemacs-hide-gitignored-files-mode nil)
      (global-set-key (kbd "M-0") 'treemacs-select-window)
      (global-set-key (kbd "C-x t 1") 'treemacs-delete-other-windows)
      (global-set-key (kbd "C-x t t") 'treemacs)
      (global-set-key (kbd "C-x t d") 'treemacs-select-directory)
      (global-set-key (kbd "C-x t B") 'treemacs-bookmark)
      (global-set-key (kbd "C-x t C-t") 'treemacs-find-file)
      (global-set-key (kbd "C-x t M-t") 'treemacs-find-tag))))

(elpaca treemacs-evil)

(elpaca
  treemacs-perspective ;;treemacs-perspective if you use perspective.el vs. persp-mode
  (setq treemacs-set-scope-type 'Perspectives))

(elpaca
  treemacs-icons-dired
  (add-hook 'dired-mode #'treemacs-icons-dired-enable-once))

(elpaca treemacs-magit)

(elpaca treemacs-projectile)

(elpaca
  treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  (setq treemacs-set-scope-type 'Tabs))

;; Move file to trash instead of removing.
(setq-default delete-by-moving-to-trash t)

;; Don't bother with auto save and backups
(setq auto-save-default nil) ; stop creating #autosave# files
(setq make-backup-files nil) ; stop creating backup~ files

;; -----------------------------------------------------------------------------
;; CI/CD
;; -----------------------------------------------------------------------------

;; It's either the index, [0, ∞), or nil
(let ((match (string-match-p "Knapsack" system-name)))
  (when (or match (eq 0 match))
    (load "~/.config/emacs/circleci.el")))

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

;; -----------------------------------------------------------------------------
;; Process
;; -----------------------------------------------------------------------------

(elpaca
  projectile
  (setq projectile-switch-project-action 'projectile-commander)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-project-search-path '("~/Documents/projects/" ("~/src/" . 3)))
  (setq projectile-auto-discover nil)
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

(elpaca
  (consult-projectile
   :host gitlab
   :repo "OlMon/consult-projectile"
   :branch "master")
  (setq consult-projectile-source-projectile-project-action 'projectile-commander))

(elpaca flycheck-projectile)

(elpaca ripgrep)

;; -----------------------------------------------------------------------------
;; VCS
;; -----------------------------------------------------------------------------

;; Magit
(elpaca
  magit
  ;; Setting ‘forge-add-default-bindings’ to nil in ‘evil-collection-forge-setup’.
  ;; To suppress this message you can set this variable to nil in your init.el file.
  (setq forge-add-default-bindings nil)
  (global-set-key (kbd "s-g") 'magit-status))

;; Disable due to performance on large repositories
;; (use-package magit-todos
;;   :after magit
;;   :init (magit-todos-mode))

(elpaca
  forge
  (setq auth-sources '("~/.authinfo")))

;; Show changes in the gutter
(elpaca
  git-gutter
  (global-git-gutter-mode +1))

;; Show author
(elpaca
  blamer
  (global-blamer-mode 1))

;; Create URLs to files and commits in repository hosting services
(elpaca git-link
  (setq git-link-use-commit t))

;; Browse target page on github/bitbucket from emacs buffers
(elpaca browse-at-remote)

;; Why was this line changed
(elpaca git-messenger)

;; View versions of a file
;; (use-package git-timemachine)

;; View git blame
(elpaca vc-msg)

;; -----------------------------------------------------------------------------
;; Org
;; -----------------------------------------------------------------------------

(elpaca org)
(elpaca org-contrib)
(elpaca org-journal)

(provide 'init)
;;; init.el ends here
