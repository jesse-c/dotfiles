((ace-window :source "elpaca-menu-lock-file" :recipe
             (:package "ace-window" :repo "abo-abo/ace-window"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id ace-window :type git
                       :protocol https :inherit t :depth treeless :ref
                       "77115afc1b0b9f633084cf7479c767988106c196"))
 (acp :source "elpaca-menu-lock-file" :recipe
      (:package "acp" :fetcher github :repo "xenodium/acp.el" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id acp :type git :protocol https
                :inherit t :depth treeless :ref
                "c32fbf8df34ed0095853a8cf55dc783e68b67d90"))
 (affe :source "elpaca-menu-lock-file" :recipe
       (:package "affe" :repo "minad/affe" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id affe :type git :protocol https
                 :inherit t :depth treeless :ref
                 "6e06b8efcd5b57160ba267e42cbf3b982a4b89a1"))
 (age :source "elpaca-menu-lock-file" :recipe
      (:package "age" :fetcher github :repo "anticomputer/age.el"
                :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id age :type git :protocol https
                :inherit t :depth treeless :ref
                "e99165ef5274bc4512b8d77ba2ac208c59b5d456"))
 (agent-shell :source "elpaca-menu-lock-file" :recipe
              (:package "agent-shell" :fetcher github :repo
                        "xenodium/agent-shell" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id agent-shell :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "7f106a355295d6c0fde5cd589cf566df7850463f"))
 (aio :source "elpaca-menu-lock-file" :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
                ("aio.el" "README.md" "UNLICENSE") :source "MELPA" :id
                aio :type git :protocol https :inherit t :depth
                treeless :ref
                "0e94a06bb035953cbbb4242568b38ca15443ad4c"))
 (all-the-icons :source "elpaca-menu-lock-file" :recipe
                (:package "all-the-icons" :repo
                          "domtronn/all-the-icons.el" :fetcher github
                          :files (:defaults "data") :source "MELPA"
                          :id all-the-icons :type git :protocol https
                          :inherit t :depth treeless :ref
                          "4778632b29c8c8d2b7cd9ce69535d0be01d846f9"))
 (all-the-icons-completion :source "elpaca-menu-lock-file" :recipe
                           (:package "all-the-icons-completion" :repo
                                     "iyefrat/all-the-icons-completion"
                                     :fetcher github :files
                                     ("*.el" "*.el.in" "dir" "*.info"
                                      "*.texi" "*.texinfo" "doc/dir"
                                      "doc/*.info" "doc/*.texi"
                                      "doc/*.texinfo" "lisp/*.el"
                                      "docs/dir" "docs/*.info"
                                      "docs/*.texi" "docs/*.texinfo"
                                      (:exclude ".dir-locals.el"
                                                "test.el" "tests.el"
                                                "*-test.el"
                                                "*-tests.el" "LICENSE"
                                                "README*" "*-pkg.el"))
                                     :source "MELPA" :id
                                     all-the-icons-completion :type
                                     git :protocol https :inherit t
                                     :depth treeless :ref
                                     "4c8bcad8033f5d0868ce82ea3807c6cd46c4a198"))
 (annalist :source "elpaca-menu-lock-file" :recipe
           (:package "annalist" :fetcher github :repo
                     "noctuid/annalist.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id annalist :type git :protocol
                     https :inherit t :depth treeless :ref
                     "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d"))
 (ansi :source "elpaca-menu-lock-file" :recipe
       (:package "ansi" :repo "rejeep/ansi.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id ansi :type git :protocol https
                 :inherit t :depth treeless :ref
                 "584be727097c9e34638d8c6161560de60202fb18"))
 (apheleia :source "elpaca-menu-lock-file" :recipe
           (:package "apheleia" :fetcher github :repo
                     "radian-software/apheleia" :files
                     (:defaults ("scripts" "scripts/formatters"))
                     :source "MELPA" :id apheleia :type git :protocol
                     https :inherit t :depth treeless :ref
                     "e6e5d5523d229735ab5f8ec83e10beefcfd00d76"))
 (ast-grep :source "elpaca-menu-lock-file" :recipe
           (:package "ast-grep" :fetcher github :repo
                     "SunskyXH/ast-grep.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id ast-grep :host github :type
                     git :protocol https :inherit t :depth treeless
                     :ref "beb7f34aaf94df559ae61665a26156453fd72a07"))
 (async :source "elpaca-menu-lock-file" :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id async :type git :protocol https
                  :inherit t :depth treeless :ref
                  "5faab28916603bb324d9faba057021ce028ca847"))
 (avy :source "elpaca-menu-lock-file" :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id avy :type git :protocol https
                :inherit t :depth treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (browse-at-remote :source "elpaca-menu-lock-file" :recipe
                   (:package "browse-at-remote" :repo
                             "rmuslimov/browse-at-remote" :fetcher
                             github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id browse-at-remote
                             :type git :protocol https :inherit t
                             :depth treeless :ref
                             "38e5ffd77493c17c821fd88f938dbf42705a5158"))
 (cape :source "elpaca-menu-lock-file" :recipe
       (:package "cape" :repo "minad/cape" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id cape :type git :protocol https
                 :inherit t :depth treeless :ref
                 "7a6a752bc694e81853d915281a73a9c3acc69757"))
 (cargo-mode :source "elpaca-menu-lock-file" :recipe
             (:package "cargo-mode" :fetcher github :repo
                       "ayrat555/cargo-mode" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id cargo-mode :type git
                       :protocol https :inherit t :depth treeless :ref
                       "33528954218f8957a26f3fef506c3537823d569d"))
 (cargo-transient :source "elpaca-menu-lock-file" :recipe
                  (:package "cargo-transient" :repo
                            "peterstuart/cargo-transient" :fetcher
                            github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :id cargo-transient :type
                            git :protocol https :inherit t :depth
                            treeless :ref
                            "6110489e8ea22f2f85976260f8ded201c00c01e3"))
 (casual :source "elpaca-menu-lock-file" :recipe
         (:package "casual" :fetcher github :repo
                   "kickingvegas/casual" :old-names
                   (casual-agenda casual-bookmarks casual-calc
                                  casual-dired casual-editkit
                                  casual-ibuffer casual-info
                                  casual-isearch cc-isearch-menu
                                  casual-lib casual-re-builder)
                   :files (:defaults "docs/images") :source "MELPA"
                   :id casual :type git :protocol https :inherit t
                   :depth treeless :ref
                   "d646190d565cb678db762fcc3663703616327bb7"))
 (casual-avy :source "elpaca-menu-lock-file" :recipe
             (:package "casual-avy" :fetcher github :repo
                       "kickingvegas/casual-avy" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id casual-avy :type git
                       :protocol https :inherit t :depth treeless :ref
                       "c5bc8e9d57a843f75e6125f097550414af3d5ec7"))
 (catppuccin-theme :source "elpaca-menu-lock-file" :recipe
                   (:package "catppuccin-theme" :fetcher github :repo
                             "catppuccin/emacs" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id catppuccin-theme
                             :type git :protocol https :inherit t
                             :depth treeless :ref
                             "4544014985689ee812165b99414d057f5728d8a1"))
 (chezmoi :source "elpaca-menu-lock-file" :recipe
          (:package "chezmoi" :fetcher github :repo
                    "tuh8888/chezmoi.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id chezmoi :type git :protocol
                    https :inherit t :depth treeless :ref
                    "1389782f8c0780c7e66f8e77b10345ba1f4eabae"))
 (cider :source "elpaca-menu-lock-file" :recipe
        (:package "cider" :fetcher github :repo "clojure-emacs/cider"
                  :files
                  ("lisp/*.el" "bin/*.sh" "*.el" "clojure.sh"
                   "lein.sh" (:exclude ".dir-locals.el"))
                  :old-names (nrepl) :source "MELPA" :id cider :type
                  git :protocol https :inherit t :depth treeless :ref
                  "5d003b579c7a7b55321c6e4e672f0f57b4021930"))
 (clojure-mode :source "elpaca-menu-lock-file" :recipe
               (:package "clojure-mode" :repo
                         "clojure-emacs/clojure-mode" :fetcher github
                         :files ("clojure-mode.el") :source "MELPA"
                         :id clojure-mode :type git :protocol https
                         :inherit t :depth treeless :ref
                         "c3b039ecf85e343edbc67c5856322654381dbc3e"))
 (clojure-ts-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "clojure-ts-mode" :repo
                            "clojure-emacs/clojure-ts-mode" :fetcher
                            github :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :id clojure-ts-mode :type
                            git :protocol https :inherit t :depth
                            treeless :ref
                            "ba6de87b0acb5aa5483f6012611b30f6bf0414f3"))
 (closql :source "elpaca-menu-lock-file" :recipe
         (:package "closql" :fetcher github :repo "magit/closql"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id closql :type git :protocol
                   https :inherit t :depth treeless :ref
                   "947426d0c93e5ad5374c464b2f121c36cdaf2132"))
 (codeium :source "elpaca-menu-lock-file" :recipe
          (:source nil :package "codeium" :id codeium :host github
                   :repo "Exafunction/codeium.el" :type git :protocol
                   https :inherit t :depth treeless :ref
                   "d815912878becffda76c1c6b41294604629e0e10"))
 (combobulate :source "elpaca-menu-lock-file" :recipe
              (:source nil :package "combobulate" :id combobulate
                       :host github :repo "mickeynp/combobulate" :type
                       git :protocol https :inherit t :depth treeless
                       :ref "74fc764c58904f6f7a9ab3417887f0eaa44af30b"))
 (compile-angel :source "elpaca-menu-lock-file" :recipe
                (:package "compile-angel" :fetcher github :repo
                          "jamescherti/compile-angel.el" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id compile-angel :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "67727ad1be21d5a6f1ab273854b6081e8f569efa"))
 (cond-let
   :source "elpaca-menu-lock-file" :recipe
   (:package "cond-let" :fetcher github :repo "tarsius/cond-let"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "MELPA" :id cond-let :type git :protocol https
             :inherit t :depth treeless :ref
             "8bf87d45e169ebc091103b2aae325aece3aa804d"))
 (consult :source "elpaca-menu-lock-file" :recipe
          (:package "consult" :repo "minad/consult" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id consult :type git :protocol
                    https :inherit t :depth treeless :ref
                    "20476c690ce3ecd45460011ce6b03fd58a642181"))
 (consult-eglot :source "elpaca-menu-lock-file" :recipe
                (:package "consult-eglot" :fetcher github :repo
                          "mohkale/consult-eglot" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id consult-eglot :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "d8b444aac39edfc6473ffbd228df3e9119451b51"))
 (consult-eglot-embark :source "elpaca-menu-lock-file" :recipe
                       (:package "consult-eglot-embark" :fetcher
                                 github :repo "mohkale/consult-eglot"
                                 :files
                                 ("extensions/consult-eglot-embark/consult-eglot-embark*.el")
                                 :source "MELPA" :id
                                 consult-eglot-embark :type git
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "d8b444aac39edfc6473ffbd228df3e9119451b51"))
 (consult-flycheck :source "elpaca-menu-lock-file" :recipe
                   (:package "consult-flycheck" :fetcher github :repo
                             "minad/consult-flycheck" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id consult-flycheck
                             :type git :protocol https :inherit t
                             :depth treeless :ref
                             "16fa53d2cc31a2689dfb5d012575c81399f6669d"))
 (consult-gh :source "elpaca-menu-lock-file" :recipe
             (:package "consult-gh" :fetcher github :repo
                       "armindarvish/consult-gh" :files
                       ("consult-gh.el" "consult-gh-transient.el")
                       :source "MELPA" :id consult-gh :type git
                       :protocol https :inherit t :depth treeless :ref
                       "f078379a50ebace30252447ad4b4b7c4514b7f95"))
 (consult-gh-embark :source "elpaca-menu-lock-file" :recipe
                    (:package "consult-gh-embark" :fetcher github
                              :repo "armindarvish/consult-gh" :files
                              ("consult-gh-embark.el") :source "MELPA"
                              :id consult-gh-embark :type git
                              :protocol https :inherit t :depth
                              treeless :ref
                              "f078379a50ebace30252447ad4b4b7c4514b7f95"))
 (consult-ghq :source "elpaca-menu-lock-file" :recipe
              (:package "consult-ghq" :repo "tomoya/consult-ghq"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id consult-ghq :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "65a99980fb313d473376542cb87464a8a44ff25e"))
 (consult-org-roam :source "elpaca-menu-lock-file" :recipe
                   (:package "consult-org-roam" :repo
                             "jgru/consult-org-roam" :fetcher github
                             :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id consult-org-roam
                             :type git :protocol https :inherit t
                             :depth treeless :ref
                             "781d9c1cfee8631bc125fa45bab92de320d3941e"))
 (consult-project-extra :source "elpaca-menu-lock-file" :recipe
                        (:package "consult-project-extra" :fetcher
                                  github :repo
                                  "Qkessler/consult-project-extra"
                                  :files
                                  ("*.el" "*.el.in" "dir" "*.info"
                                   "*.texi" "*.texinfo" "doc/dir"
                                   "doc/*.info" "doc/*.texi"
                                   "doc/*.texinfo" "lisp/*.el"
                                   "docs/dir" "docs/*.info"
                                   "docs/*.texi" "docs/*.texinfo"
                                   (:exclude ".dir-locals.el"
                                             "test.el" "tests.el"
                                             "*-test.el" "*-tests.el"
                                             "LICENSE" "README*"
                                             "*-pkg.el"))
                                  :source "MELPA" :id
                                  consult-project-extra :type git
                                  :protocol https :inherit t :depth
                                  treeless :ref
                                  "2b3fa36fd3a14deacf594f4acd54d220d6890c55"))
 (consult-todo :source "elpaca-menu-lock-file" :recipe
               (:package "consult-todo" :fetcher github :repo
                         "eki3z/consult-todo" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id consult-todo :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "f9ba063a6714cb95ddbd886786ada93771f3c140"))
 (consult-xref-stack :source "elpaca-menu-lock-file" :recipe
                     (:source nil :package "consult-xref-stack" :id
                              consult-xref-stack :host github :repo
                              "brett-lempereur/consult-xref-stack"
                              :type git :protocol https :inherit t
                              :depth treeless :ref
                              "1dcbf2e15a2279365940de8e7c2d29d2586dfa2c"))
 (consult-yasnippet :source "elpaca-menu-lock-file" :recipe
                    (:package "consult-yasnippet" :fetcher github
                              :repo "mohkale/consult-yasnippet" :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :id consult-yasnippet
                              :type git :protocol https :inherit t
                              :depth treeless :ref
                              "a3482dfbdcbe487ba5ff934a1bb6047066ff2194"))
 (corfu :source "elpaca-menu-lock-file" :recipe
        (:package "corfu" :repo "minad/corfu" :files
                  (:defaults "extensions/corfu-*.el") :fetcher github
                  :source "MELPA" :id corfu :type git :protocol https
                  :inherit t :depth treeless :ref
                  "20009d4fcc31770200b63a1440f15320ee009def"))
 (crux :source "elpaca-menu-lock-file" :recipe
       (:package "crux" :fetcher github :repo "bbatsov/crux" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id crux :type git :protocol https
                 :inherit t :depth treeless :ref
                 "69e03917f6fd35e25b9a9dfd02df8ff3643f9227"))
 (csv-mode :source "elpaca-menu-lock-file" :recipe
           (:package "csv-mode" :repo
                     ("https://github.com/emacsmirror/gnu_elpa"
                      . "csv-mode")
                     :tar "1.27" :host gnu :branch
                     "externals/csv-mode" :files
                     ("*" (:exclude ".git")) :source "GNU ELPA" :id
                     csv-mode :type git :protocol https :inherit t
                     :depth treeless :ref
                     "ba5dc934b9dbdc2b57ab1917a669cdfd7d1838d3"))
 (dape :source "elpaca-menu-lock-file" :recipe
       (:package "dape" :repo
                 ("https://github.com/svaante/dape" . "dape") :tar
                 "0.26.0" :host gnu :files ("*" (:exclude ".git"))
                 :source "GNU ELPA" :id dape :type git :protocol https
                 :inherit t :depth treeless :ref
                 "6fab87c15e5b68d9f3c7d0764978f196c53a578e"))
 (dash :source "elpaca-menu-lock-file" :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi") :source "MELPA" :id dash
                 :type git :protocol https :inherit t :depth treeless
                 :ref "d3a84021dbe48dba63b52ef7665651e0cf02e915"))
 (devdocs :source "elpaca-menu-lock-file" :recipe
          (:package "devdocs" :fetcher github :repo
                    "astoff/devdocs.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id devdocs :type git :protocol
                    https :inherit t :depth treeless :ref
                    "25c746024ddf73570195bf42b841f761a2fee10c"))
 (diff-hl :source "elpaca-menu-lock-file" :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id diff-hl :type git :protocol
                    https :inherit t :depth treeless :ref
                    "b965e19e6e7f9933199e421849a49229207c1c9f"))
 (difftastic :source "elpaca-menu-lock-file" :recipe
             (:package "difftastic" :fetcher github :repo
                       "pkryger/difftastic.el" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id difftastic :type git
                       :protocol https :inherit t :depth treeless :ref
                       "7db20929cac31687a529943c3d8d5b44fd8d69e2"))
 (diminish :source "elpaca-menu-lock-file" :recipe
           (:package "diminish" :fetcher github :repo
                     "myrjola/diminish.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id diminish :type git :protocol
                     https :inherit t :depth treeless :ref
                     "fbd5d846611bad828e336b25d2e131d1bc06b83d"))
 (docker :source "elpaca-menu-lock-file" :recipe
         (:package "docker" :fetcher github :repo "Silex/docker.el"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id docker :type git :protocol
                   https :inherit t :depth treeless :ref
                   "916686b86e83a3bd2281fbc5e6f98962aa747429"))
 (docker-compose-mode :source "elpaca-menu-lock-file" :recipe
                      (:package "docker-compose-mode" :repo
                                "meqif/docker-compose-mode" :fetcher
                                github :files
                                (:defaults
                                 (:exclude
                                  "docker-compose-mode-helpers.el"))
                                :source "MELPA" :id
                                docker-compose-mode :type git
                                :protocol https :inherit t :depth
                                treeless :ref
                                "abaa4f3aeb5c62d7d16e186dd7d77f4e846e126a"))
 (dockerfile-mode :source "elpaca-menu-lock-file" :recipe
                  (:package "dockerfile-mode" :fetcher github :repo
                            "spotify/dockerfile-mode" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :id dockerfile-mode :type
                            git :protocol https :inherit t :depth
                            treeless :ref
                            "97733ce074b1252c1270fd5e8a53d178b66668ed"))
 (doom-modeline :source "elpaca-menu-lock-file" :recipe
                (:package "doom-modeline" :repo
                          "seagle0128/doom-modeline" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id doom-modeline :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "2e70c8411be374ce6e29d06da7103e51982191e8"))
 (dtrt-indent :source "elpaca-menu-lock-file" :recipe
              (:package "dtrt-indent" :fetcher github :repo
                        "jscheid/dtrt-indent" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id dtrt-indent :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "7c372bec8d84c247e4bd0d5599024d66ee300429"))
 (easysession :source "elpaca-menu-lock-file" :recipe
              (:package "easysession" :fetcher github :repo
                        "jamescherti/easysession.el" :files
                        (:defaults "extensions/easysession*.el")
                        :source "MELPA" :id easysession :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "cb6ec28e952da7a19f32a07e8729c37af6d88d20"))
 (eglot-booster :source "elpaca-menu-lock-file" :recipe
                (:source nil :package "eglot-booster" :id
                         eglot-booster :host github :repo
                         "jesse-c/eglot-booster" :type git :protocol
                         https :inherit t :depth treeless :ref
                         "cab7803c4f0adc7fff9da6680f90110674bb7a22"))
 (eglot-tempel :source "elpaca-menu-lock-file" :recipe
               (:package "eglot-tempel" :fetcher github :repo
                         "fejfighter/eglot-tempel" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id eglot-tempel :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "c6c9a18eba61f6bae7167fa62bab9b637592d20d"))
 (eldoc-box :source "elpaca-menu-lock-file" :recipe
            (:package "eldoc-box" :repo "casouri/eldoc-box" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id eldoc-box :type git
                      :protocol https :inherit t :depth treeless :ref
                      "8847b1aac2133e8a8211e728807fdd20d332298c"))
 (elisp-demos :source "elpaca-menu-lock-file" :recipe
              (:package "elisp-demos" :fetcher github :repo
                        "xuchunyang/elisp-demos" :files
                        (:defaults "*.org") :source "MELPA" :id
                        elisp-demos :type git :protocol https :inherit
                        t :depth treeless :ref
                        "1a108d1c5011f9ced58be2ca98bea1fbd4130a2f"))
 (elisp-refs :source "elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs"
                       :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el"))
                       :source "MELPA" :id elisp-refs :type git
                       :protocol https :inherit t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (elpaca :source
   "elpaca-menu-lock-file" :recipe
   (:source nil :package "elpaca" :id elpaca :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "e9cb7eef2d8539e362d87f0489ab9eed8e8732c4" :depth 1
            :inherit ignore :files
            (:defaults "elpaca-test.el" (:exclude "extensions"))
            :build (:not elpaca-activate) :type git :protocol https))
 (elpaca-use-package :source "elpaca-menu-lock-file" :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git"
                               :files
                               ("extensions/elpaca-use-package.el")
                               :main
                               "extensions/elpaca-use-package.el"
                               :build (:not elpaca-build-docs) :source
                               "Elpaca extensions" :id
                               elpaca-use-package :type git :protocol
                               https :inherit t :depth treeless :ref
                               "e9cb7eef2d8539e362d87f0489ab9eed8e8732c4"))
 (elsa :source "elpaca-menu-lock-file" :recipe
       (:package "elsa" :fetcher github :repo "emacs-elsa/Elsa" :files
                 (:defaults "bin") :source "MELPA" :id elsa :type git
                 :protocol https :inherit t :depth treeless :ref
                 "4a4a180a7e6837ac359c0094e40da339e1300765"))
 (emacsql :source "elpaca-menu-lock-file" :recipe
          (:package "emacsql" :fetcher github :repo "magit/emacsql"
                    :files (:defaults "README.md" "sqlite") :source
                    "MELPA" :id emacsql :type git :protocol https
                    :inherit t :depth treeless :ref
                    "2fe6d4562b32a170a750d5e80514fbb6b6694803"))
 (embark :source "elpaca-menu-lock-file" :recipe
         (:package "embark" :repo "oantolin/embark" :fetcher github
                   :files ("embark.el" "embark-org.el" "embark.texi")
                   :source "MELPA" :id embark :type git :protocol
                   https :inherit t :depth treeless :ref
                   "27de48004242e98586b9c9661fdb6912f26fe70f"))
 (embark-consult :source "elpaca-menu-lock-file" :recipe
                 (:package "embark-consult" :repo "oantolin/embark"
                           :fetcher github :files
                           ("embark-consult.el") :source "MELPA" :id
                           embark-consult :type git :protocol https
                           :inherit t :depth treeless :ref
                           "27de48004242e98586b9c9661fdb6912f26fe70f"))
 (eros :source "elpaca-menu-lock-file" :recipe
       (:package "eros" :fetcher github :repo "xiongtx/eros" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id eros :type git :protocol https
                 :inherit t :depth treeless :ref
                 "66ee90baa3162fea028f5101ddcc370f7d1d4fcf"))
 (eros-inspector :source "elpaca-menu-lock-file" :recipe
                 (:package "eros-inspector" :repo
                           "port19x/eros-inspector" :fetcher github
                           :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id eros-inspector :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "6b9eb625b1b3ece683c6c2ac8dcc9a14530f2c89"))
 (esxml :source "elpaca-menu-lock-file" :recipe
        (:package "esxml" :fetcher github :repo "tali713/esxml" :files
                  ("esxml.el" "esxml-query.el") :source "MELPA" :id
                  esxml :type git :protocol https :inherit t :depth
                  treeless :ref
                  "6a375888a74b7563eb53176aad81faee8b858189"))
 (eval-in-repl :source "elpaca-menu-lock-file" :recipe
               (:package "eval-in-repl" :fetcher github :repo
                         "kaz-yos/eval-in-repl" :commit
                         "origin/master" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id eval-in-repl :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "a57c6a790c0ca72b0d1218b837d3114ef874dd1f"))
 (evil :source "elpaca-menu-lock-file" :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi"
                            (:exclude "evil-test-helpers.el"))
                 :source "MELPA" :id evil :type git :protocol https
                 :inherit t :depth treeless :ref
                 "729d9a58b387704011a115c9200614e32da3cefc"))
 (evil-collection :source "elpaca-menu-lock-file" :recipe
                  (:package "evil-collection" :fetcher github :repo
                            "emacs-evil/evil-collection" :files
                            (:defaults "modes") :source "MELPA" :id
                            evil-collection :type git :protocol https
                            :inherit t :depth treeless :ref
                            "4ad1646964638322302dfb167aec40a2455bfb78"))
 (evil-org :source "elpaca-menu-lock-file" :recipe
           (:package "evil-org" :fetcher github :repo
                     "Somelauw/evil-org-mode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id evil-org :type git :protocol
                     https :inherit t :depth treeless :ref
                     "b1f309726b1326e1a103742524ec331789f2bf94"))
 (evil-surround :source "elpaca-menu-lock-file" :recipe
                (:package "evil-surround" :repo
                          "emacs-evil/evil-surround" :fetcher github
                          :old-names (surround) :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id evil-surround :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "da05c60b0621cf33161bb4335153f75ff5c29d91"))
 (exec-path-from-shell :source "elpaca-menu-lock-file" :recipe
                       (:package "exec-path-from-shell" :fetcher
                                 github :repo
                                 "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info"
                                  "*.texi" "*.texinfo" "doc/dir"
                                  "doc/*.info" "doc/*.texi"
                                  "doc/*.texinfo" "lisp/*.el"
                                  "docs/dir" "docs/*.info"
                                  "docs/*.texi" "docs/*.texinfo"
                                  (:exclude ".dir-locals.el" "test.el"
                                            "tests.el" "*-test.el"
                                            "*-tests.el" "LICENSE"
                                            "README*" "*-pkg.el"))
                                 :source "MELPA" :id
                                 exec-path-from-shell :type git
                                 :protocol https :inherit t :depth
                                 treeless :ref
                                 "7552abf032a383ff761e7d90e6b5cbb4658a728a"))
 (f :source "elpaca-menu-lock-file" :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "MELPA" :id f :type git :protocol https :inherit
              t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (fish-mode :source "elpaca-menu-lock-file" :recipe
            (:package "fish-mode" :fetcher github :repo
                      "wwwjfy/emacs-fish" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id fish-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "2526b1803b58cf145bc70ff6ce2adb3f6c246f89"))
 (flycheck :source "elpaca-menu-lock-file" :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id flycheck :type git :protocol
                     https :inherit t :depth treeless :ref
                     "0e5eb8300d32fd562724216c19eaf199ee1451ab"))
 (flycheck-clojure :source "elpaca-menu-lock-file" :recipe
                   (:package "flycheck-clojure" :fetcher github :repo
                             "clojure-emacs/squiggly-clojure" :files
                             ("elisp/flycheck-clojure/*.el") :source
                             "MELPA" :id flycheck-clojure :type git
                             :protocol https :inherit t :depth
                             treeless :ref
                             "592c4f89efb5112784cbf94c9ea6fdd045771b62"))
 (flycheck-credo :source "elpaca-menu-lock-file" :recipe
                 (:package "flycheck-credo" :fetcher github :repo
                           "aaronjensen/flycheck-credo" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id flycheck-credo :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "e285bd042a535d0f13e0b4c5226df404cdda4033"))
 (flycheck-eglot :source "elpaca-menu-lock-file" :recipe
                 (:package "flycheck-eglot" :fetcher github :repo
                           "flycheck/flycheck-eglot" :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id flycheck-eglot :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "cd1dd78cec0ae1f566c765d98bbff322cc7b67ef"))
 (flycheck-elsa :source "elpaca-menu-lock-file" :recipe
                (:package "flycheck-elsa" :fetcher github :repo
                          "emacs-elsa/flycheck-elsa" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id flycheck-elsa :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "d60db9544d0c4213f2478bcea0fd0e668e31cf34"))
 (flycheck-package :source "elpaca-menu-lock-file" :recipe
                   (:package "flycheck-package" :fetcher github :repo
                             "purcell/flycheck-package" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id flycheck-package
                             :type git :protocol https :inherit t
                             :depth treeless :ref
                             "a52e4e95f3151898b36739dfdb4a98b368626fc0"))
 (flycheck-rust :source "elpaca-menu-lock-file" :recipe
                (:package "flycheck-rust" :repo
                          "flycheck/flycheck-rust" :fetcher github
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id flycheck-rust :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "b9db73a7a5980ca884d5dd0cbe79b3291a185972"))
 (flycheck-swift :source "elpaca-menu-lock-file" :recipe
                 (:package "flycheck-swift" :repo
                           "swift-emacs/flycheck-swift" :fetcher
                           github :files
                           ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                            "*.texinfo" "doc/dir" "doc/*.info"
                            "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                            "docs/dir" "docs/*.info" "docs/*.texi"
                            "docs/*.texinfo"
                            (:exclude ".dir-locals.el" "test.el"
                                      "tests.el" "*-test.el"
                                      "*-tests.el" "LICENSE" "README*"
                                      "*-pkg.el"))
                           :source "MELPA" :id flycheck-swift :type
                           git :protocol https :inherit t :depth
                           treeless :ref
                           "4c5ad401252400a78da395fd56a71e67ff8c2761"))
 (flyover :source "elpaca-menu-lock-file" :recipe
          (:package "flyover" :fetcher github :repo
                    "konrad1977/flyover" :files ("flyover.el") :source
                    "MELPA" :id flyover :type git :protocol https
                    :inherit t :depth treeless :ref
                    "8815cb067ca0d3d32607c9ae5093673cb29663a0"))
 (forge :source "elpaca-menu-lock-file" :recipe
        (:package "forge" :fetcher github :repo "magit/forge" :files
                  ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source
                  "MELPA" :id forge :type git :protocol https :inherit
                  t :depth treeless :ref
                  "69801d0da19d62b4b68b1f1756900e47ce7e8769"))
 (ghub :source "elpaca-menu-lock-file" :recipe
       (:package "ghub" :fetcher github :repo "magit/ghub" :files
                 ("lisp/*.el" "docs/*.texi" ".dir-locals.el") :source
                 "MELPA" :id ghub :type git :protocol https :inherit t
                 :depth treeless :ref
                 "1fb0fba075cb8b80f9819c874be584dffce50b51"))
 (git-commit-ts-mode :source "elpaca-menu-lock-file" :recipe
                     (:package "git-commit-ts-mode" :repo
                               "danilshvalov/git-commit-ts-mode"
                               :fetcher github :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :id git-commit-ts-mode
                               :host github :type git :protocol https
                               :inherit t :depth treeless :ref
                               "6eb42a3c08c5c6a1a610d433b93590b88a71f63e"))
 (git-link :source "elpaca-menu-lock-file" :recipe
           (:package "git-link" :fetcher github :repo "sshaw/git-link"
                     :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id git-link :type git :protocol
                     https :inherit t :depth treeless :ref
                     "d9b375f79e6071a9926bf73bba64111adfc93bf5"))
 (git-messenger :source "elpaca-menu-lock-file" :recipe
                (:package "git-messenger" :repo
                          "emacsorphanage/git-messenger" :fetcher
                          github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id git-messenger :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "eade986ef529aa2dac6944ad61b18de55cee0b76"))
 (git-modes :source "elpaca-menu-lock-file" :recipe
            (:package "git-modes" :fetcher github :repo
                      "magit/git-modes" :old-names
                      (gitattributes-mode gitconfig-mode
                                          gitignore-mode)
                      :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id git-modes :type git
                      :protocol https :inherit t :depth treeless :ref
                      "c3faeeea1982786f78d8c38397dec0f078eaec84"))
 (git-timemachine :source "elpaca-menu-lock-file" :recipe
                  (:package "git-timemachine" :fetcher codeberg :repo
                            "pidu/git-timemachine" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                             "*.texinfo" "doc/dir" "doc/*.info"
                             "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             "docs/dir" "docs/*.info" "docs/*.texi"
                             "docs/*.texinfo"
                             (:exclude ".dir-locals.el" "test.el"
                                       "tests.el" "*-test.el"
                                       "*-tests.el" "LICENSE"
                                       "README*" "*-pkg.el"))
                            :source "MELPA" :id git-timemachine :type
                            git :protocol https :inherit t :depth
                            treeless :ref
                            "d1346a76122595aeeb7ebb292765841c6cfd417b"))
 (goto-chg :source "elpaca-menu-lock-file" :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id goto-chg :type git :protocol
                     https :inherit t :depth treeless :ref
                     "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (gptel :source "elpaca-menu-lock-file" :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github
                  :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id gptel :type git :protocol https
                  :inherit t :depth treeless :ref
                  "8d6411b5f89d796c817ff79324973b8910e164fe"))
 (gptel-quick :source "elpaca-menu-lock-file" :recipe
              (:source nil :package "gptel-quick" :id gptel-quick
                       :host github :repo "karthink/gptel-quick" :type
                       git :protocol https :inherit t :depth treeless
                       :ref "018ff2be8f860a1e8fe3966eec418ad635620c38"))
 (helpful :source "elpaca-menu-lock-file" :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id helpful :type git :protocol
                    https :inherit t :depth treeless :ref
                    "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39"))
 (hl-todo :source "elpaca-menu-lock-file" :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id hl-todo :type git :protocol
                    https :inherit t :depth treeless :ref
                    "9540fc414014822dde00f0188b74e17ac99e916d"))
 (ht :source "elpaca-menu-lock-file" :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "MELPA" :id ht :type git :protocol https
               :inherit t :depth treeless :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (hydra :source "elpaca-menu-lock-file" :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source "MELPA" :id
                  hydra :type git :protocol https :inherit t :depth
                  treeless :ref
                  "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (indent-bars :source "elpaca-menu-lock-file" :recipe
              (:package "indent-bars" :repo
                        ("https://github.com/jdtsmith/indent-bars"
                         . "indent-bars")
                        :tar "1.0.0" :host gnu :files
                        ("*" (:exclude ".git" "LICENSE")) :source
                        "GNU ELPA" :id indent-bars :type git :protocol
                        https :inherit t :depth treeless :ref
                        "6e6bb5484edebf22654a960073f1ae23b4fe9a1e"))
 (inspector :source "elpaca-menu-lock-file" :recipe
            (:package "inspector" :repo
                      ("https://github.com/mmontone/emacs-inspector"
                       . "inspector")
                      :tar "0.39" :host gnu :files
                      ("*" (:exclude ".git" "LICENSE")) :source
                      "GNU ELPA" :id inspector :type git :protocol
                      https :inherit t :depth treeless :ref
                      "52a64993ac36ed3ed0be51b6a0d54d190edc9c74"))
 (jinx :source "elpaca-menu-lock-file" :recipe
       (:package "jinx" :repo "minad/jinx" :files
                 (:defaults "jinx-mod.c" "emacs-module.h") :fetcher
                 github :source "MELPA" :id jinx :type git :protocol
                 https :inherit t :depth treeless :ref
                 "5aed0911971b866d75e326a9258a20a66df0cff2"))
 (json-mode :source "elpaca-menu-lock-file" :recipe
            (:package "json-mode" :fetcher github :repo
                      "json-emacs/json-mode" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id json-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "466d5b563721bbeffac3f610aefaac15a39d90a9"))
 (json-snatcher :source "elpaca-menu-lock-file" :recipe
                (:package "json-snatcher" :fetcher github :repo
                          "Sterlingg/json-snatcher" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id json-snatcher :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "b28d1c0670636da6db508d03872d96ffddbc10f2"))
 (just-ts-mode :source "elpaca-menu-lock-file" :recipe
               (:package "just-ts-mode" :fetcher github :repo
                         "leon-barrett/just-ts-mode.el" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id just-ts-mode :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "94f788eccb13cd3ade827af5612ffe3cad5fddf0"))
 (kind-icon :source "elpaca-menu-lock-file" :recipe
            (:package "kind-icon" :repo
                      ("https://github.com/jdtsmith/kind-icon"
                       . "kind-icon")
                      :tar "0.2.2" :host gnu :files
                      ("*" (:exclude ".git")) :source "GNU ELPA" :id
                      kind-icon :type git :protocol https :inherit t
                      :depth treeless :ref
                      "556b0fb92aac24979b2c501431c7d48f75a5169f"))
 (kirigami :source "elpaca-menu-lock-file" :recipe
           (:package "kirigami" :fetcher github :repo
                     "jamescherti/kirigami.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id kirigami :type git :protocol
                     https :inherit t :depth treeless :ref
                     "7038a9dcfa7e2d8848817508777d8ad878756cfb"))
 (lgr :source "elpaca-menu-lock-file" :recipe
      (:package "lgr" :fetcher github :repo "Fuco1/emacs-lgr" :files
                ("lgr.el") :source "MELPA" :id lgr :type git :protocol
                https :inherit t :depth treeless :ref
                "20f3a3db9a142e86a15d14597213de23d01b772c"))
 (ligature :source "elpaca-menu-lock-file" :recipe
           (:package "ligature" :fetcher github :repo
                     "mickeynp/ligature.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id ligature :type git :protocol
                     https :inherit t :depth treeless :ref
                     "6ac1634612dbd42f7eb81ecaf022bd239aabb954"))
 (llama :source "elpaca-menu-lock-file" :recipe
        (:package "llama" :fetcher github :repo "tarsius/llama" :files
                  ("llama.el" ".dir-locals.el") :source "MELPA" :id
                  llama :type git :protocol https :inherit t :depth
                  treeless :ref
                  "d430d48e0b5afd2a34b5531f103dcb110c3539c4"))
 (llm :source "elpaca-menu-lock-file" :recipe
      (:package "llm" :repo ("https://github.com/ahyatt/llm" . "llm")
                :tar "0.29.0" :host gnu :files ("*" (:exclude ".git"))
                :source "GNU ELPA" :id llm :type git :protocol https
                :inherit t :depth treeless :ref
                "d73f3d3e0c5a17d2336b4a42b1a0ff609bdf75db"))
 (load-relative :source "elpaca-menu-lock-file" :recipe
                (:package "load-relative" :fetcher github :repo
                          "rocky/emacs-load-relative" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id load-relative :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "b7987c265a64435299d6b02f960ed2c894c4a145"))
 (loop :source "elpaca-menu-lock-file" :recipe
       (:package "loop" :repo "Wilfred/loop.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id loop :type git :protocol https
                 :inherit t :depth treeless :ref
                 "9db6372791bbd0cf3fa907ed0ae3e6b7bcf6cc57"))
 (lsp-mode :source "elpaca-menu-lock-file" :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher
                     github :files (:defaults "clients/*.*") :source
                     "MELPA" :id lsp-mode :type git :protocol https
                     :inherit t :depth treeless :ref
                     "4c74da7ae51145f8e49c3544c90b410d96a742fa"))
 (lv :source "elpaca-menu-lock-file" :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el") :source "MELPA" :id lv :type git :protocol
               https :inherit t :depth treeless :ref
               "59a2a45a35027948476d1d7751b0f0215b1e61aa"))
 (magit :source "elpaca-menu-lock-file" :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi"
                   "docs/AUTHORS.md" "LICENSE" ".dir-locals.el"
                   ("git-hooks" "git-hooks/*")
                   (:exclude "lisp/magit-section.el"))
                  :source "MELPA" :id magit :type git :protocol https
                  :inherit t :depth treeless :ref
                  "098e6a02f18d0bee27eab0ad9e3c18a49b18d63d"))
 (magit-delta :source "elpaca-menu-lock-file" :recipe
              (:package "magit-delta" :fetcher github :repo
                        "dandavison/magit-delta" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id magit-delta :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "5fc7dbddcfacfe46d3fd876172ad02a9ab6ac616"))
 (magit-popup :source "elpaca-menu-lock-file" :recipe
              (:package "magit-popup" :fetcher github :repo
                        "magit/magit-popup" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id magit-popup :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "d8585fa39f88956963d877b921322530257ba9f5"))
 (magit-prime :source "elpaca-menu-lock-file" :recipe
              (:package "magit-prime" :fetcher github :repo
                        "Azkae/magit-prime" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id magit-prime :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "92c9990d028a7dfe90225a440c116ab7d9724f7c"))
 (magit-section :source "elpaca-menu-lock-file" :recipe
                (:package "magit-section" :fetcher github :repo
                          "magit/magit" :files
                          ("lisp/magit-section.el"
                           "docs/magit-section.texi"
                           "magit-section-pkg.el")
                          :source "MELPA" :id magit-section :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "098e6a02f18d0bee27eab0ad9e3c18a49b18d63d"))
 (magit-todos :source "elpaca-menu-lock-file" :recipe
              (:package "magit-todos" :fetcher github :repo
                        "alphapapa/magit-todos" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id magit-todos :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "7294a95580bddf7232f2d205efae312dc24c5f61"))
 (marginalia :source "elpaca-menu-lock-file" :recipe
             (:package "marginalia" :repo "minad/marginalia" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id marginalia :type git
                       :protocol https :inherit t :depth treeless :ref
                       "51a79bb82355d0ce0ee677151f041a3aba8cbfca"))
 (markdown-mode :source "elpaca-menu-lock-file" :recipe
                (:package "markdown-mode" :fetcher github :repo
                          "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id markdown-mode :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "182640f79c3ed66f82f0419f130dffc173ee9464"))
 (markdown-ts-mode :source "elpaca-menu-lock-file" :recipe
                   (:package "markdown-ts-mode" :repo
                             "LionyxML/markdown-ts-mode" :fetcher
                             github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id markdown-ts-mode
                             :type git :protocol https :inherit t
                             :depth treeless :ref
                             "2f1ee8b94cdf53cebc31ae08ecfbba846193d5e1"))
 (mcp-hub :source "elpaca-menu-lock-file" :recipe
          (:source nil :package "mcp-hub" :id mcp-hub :host github
                   :repo "lizqwerscott/mcp.el" :type git :protocol
                   https :inherit t :depth treeless :ref
                   "5c105a8db470eb9777fdbd26251548dec42c03f0"))
 (mistty :source "elpaca-menu-lock-file" :recipe
         (:package "mistty" :fetcher github :repo "szermatt/mistty"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id mistty :type git :protocol
                   https :inherit t :depth treeless :ref
                   "1752970d1d303fae173527fbcdb3458e865197bb"))
 (mix :source "elpaca-menu-lock-file" :recipe
      (:package "mix" :fetcher github :repo "ayrat555/mix.el" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id mix :type git :protocol https
                :inherit t :depth treeless :ref
                "16cc69cbf919769c191b1c49c1cab324fd0682a9"))
 (modern-sh :source "elpaca-menu-lock-file" :recipe
            (:package "modern-sh" :fetcher github :repo
                      "damon-kwok/modern-sh" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id modern-sh :type git
                      :protocol https :inherit t :depth treeless :ref
                      "65bc75828f7d13af713f1a728c038e2915944cd3"))
 (nerd-icons :source "elpaca-menu-lock-file" :recipe
             (:package "nerd-icons" :repo
                       "rainstormstudio/nerd-icons.el" :fetcher github
                       :files (:defaults "data") :source "MELPA" :id
                       nerd-icons :type git :protocol https :inherit t
                       :depth treeless :ref
                       "1db0b0b9203cf293b38ac278273efcfc3581a05f"))
 (nerd-icons-completion :source "elpaca-menu-lock-file" :recipe
                        (:package "nerd-icons-completion" :repo
                                  "rainstormstudio/nerd-icons-completion"
                                  :fetcher github :files
                                  ("*.el" "*.el.in" "dir" "*.info"
                                   "*.texi" "*.texinfo" "doc/dir"
                                   "doc/*.info" "doc/*.texi"
                                   "doc/*.texinfo" "lisp/*.el"
                                   "docs/dir" "docs/*.info"
                                   "docs/*.texi" "docs/*.texinfo"
                                   (:exclude ".dir-locals.el"
                                             "test.el" "tests.el"
                                             "*-test.el" "*-tests.el"
                                             "LICENSE" "README*"
                                             "*-pkg.el"))
                                  :source "MELPA" :id
                                  nerd-icons-completion :type git
                                  :protocol https :inherit t :depth
                                  treeless :ref
                                  "d09ea987ed3d2cc64137234f27851594050e2b64"))
 (nerd-icons-dired :source "elpaca-menu-lock-file" :recipe
                   (:package "nerd-icons-dired" :repo
                             "rainstormstudio/nerd-icons-dired"
                             :fetcher github :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id nerd-icons-dired
                             :type git :protocol https :inherit t
                             :depth treeless :ref
                             "929b62f01b93d30a3f42cc507fc45c84a2457b3f"))
 (no-littering :source "elpaca-menu-lock-file" :recipe
               (:package "no-littering" :fetcher github :repo
                         "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id no-littering :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "7bab26a5912074669aa6b4246f79bbdcfc0f65ba"))
 (nov :source "elpaca-menu-lock-file" :recipe
      (:package "nov" :fetcher git :url
                "https://depp.brause.cc/nov.el.git" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id nov :type git :protocol https
                :inherit t :depth treeless :ref
                "874daf5e4791a6d4f47741422c80e2736e907351"))
 (ob-async :source "elpaca-menu-lock-file" :recipe
           (:package "ob-async" :repo "astahlman/ob-async" :fetcher
                     github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id ob-async :type git :protocol
                     https :inherit t :depth treeless :ref
                     "9aac486073f5c356ada20e716571be33a350a982"))
 (ob-elixir :source "elpaca-menu-lock-file" :recipe
            (:package "ob-elixir" :repo "zweifisch/ob-elixir" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id ob-elixir :type git
                      :protocol https :inherit t :depth treeless :ref
                      "8e5d2f3c7adb0d5acde390264fec94627aa7af31"))
 (ob-git-permalink :source "elpaca-menu-lock-file" :recipe
                   (:package "ob-git-permalink" :fetcher github :repo
                             "kijimad/ob-git-permalink" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id ob-git-permalink
                             :type git :protocol https :inherit t
                             :depth treeless :ref
                             "6f8751e5c31a544c062295a20d16196efe1d4487"))
 (ob-gptel :source "elpaca-menu-lock-file" :recipe
           (:source nil :package "ob-gptel" :id ob-gptel :host github
                    :repo "jwiegley/ob-gptel" :type git :protocol
                    https :inherit t :depth treeless :ref
                    "cbed018a7d81de9ba8dc3220e1c4d10b7bb29b11"))
 (ob-rust :source "elpaca-menu-lock-file" :recipe
          (:package "ob-rust" :fetcher github :repo
                    "micanzhang/ob-rust" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id ob-rust :type git :protocol
                    https :inherit t :depth treeless :ref
                    "be059d231fafeb24a658db212a55ccdc55c0c500"))
 (orderless :source "elpaca-menu-lock-file" :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id orderless :type git
                      :protocol https :inherit t :depth treeless :ref
                      "3a2a32181f7a5bd7b633e40d89de771a5dd88cc7"))
 (org-download :source "elpaca-menu-lock-file" :recipe
               (:package "org-download" :repo "abo-abo/org-download"
                         :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id org-download :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "c8be2611786d1d8d666b7b4f73582de1093f25ac"))
 (org-modern :source "elpaca-menu-lock-file" :recipe
             (:package "org-modern" :repo "minad/org-modern" :fetcher
                       github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id org-modern :type git
                       :protocol https :inherit t :depth treeless :ref
                       "713beb72aed4db43f8a10feed72136e931eb674a"))
 (org-ql :source "elpaca-menu-lock-file" :recipe
         (:package "org-ql" :fetcher github :repo "alphapapa/org-ql"
                   :files (:defaults (:exclude "helm-org-ql.el"))
                   :source "MELPA" :id org-ql :type git :protocol
                   https :inherit t :depth treeless :ref
                   "4b8330a683c43bb4a2c64ccce8cd5a90c8b174ca"))
 (org-roam :source "elpaca-menu-lock-file" :recipe
           (:package "org-roam" :fetcher github :repo
                     "org-roam/org-roam" :files
                     (:defaults "extensions/*") :source "MELPA" :id
                     org-roam :type git :protocol https :inherit t
                     :depth treeless :ref
                     "7cd906b6f8b18a21766228f074aff24586770934"))
 (org-roam-ql :source "elpaca-menu-lock-file" :recipe
              (:package "org-roam-ql" :fetcher github :repo
                        "ahmed-shariff/org-roam-ql" :files
                        ("org-roam-ql.el") :source "MELPA" :id
                        org-roam-ql :type git :protocol https :inherit
                        t :depth treeless :ref
                        "e0174bdcadb2e9869d810b5756bea50f2109fb78"))
 (org-roam-ql-ql :source "elpaca-menu-lock-file" :recipe
                 (:package "org-roam-ql-ql" :fetcher github :repo
                           "ahmed-shariff/org-roam-ql" :files
                           ("org-roam-ql-ql.el") :source "MELPA" :id
                           org-roam-ql-ql :type git :protocol https
                           :inherit t :depth treeless :ref
                           "e0174bdcadb2e9869d810b5756bea50f2109fb78"))
 (org-super-agenda :source "elpaca-menu-lock-file" :recipe
                   (:package "org-super-agenda" :fetcher github :repo
                             "alphapapa/org-super-agenda" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                              "*.texinfo" "doc/dir" "doc/*.info"
                              "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              "docs/dir" "docs/*.info" "docs/*.texi"
                              "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el"
                                        "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE"
                                        "README*" "*-pkg.el"))
                             :source "MELPA" :id org-super-agenda
                             :type git :protocol https :inherit t
                             :depth treeless :ref
                             "fb20ad9c8a9705aa05d40751682beae2d094e0fe"))
 (ov :source "elpaca-menu-lock-file" :recipe
     (:package "ov" :fetcher github :repo "emacsorphanage/ov" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "MELPA" :id ov :type git :protocol https
               :inherit t :depth treeless :ref
               "e2971ad986b6ac441e9849031d34c56c980cf40b"))
 (ox-gfm :source "elpaca-menu-lock-file" :recipe
         (:package "ox-gfm" :fetcher github :repo "larstvei/ox-gfm"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id ox-gfm :type git :protocol
                   https :inherit t :depth treeless :ref
                   "4f774f13d34b3db9ea4ddb0b1edc070b1526ccbb"))
 (ox-pandoc :source "elpaca-menu-lock-file" :recipe
            (:package "ox-pandoc" :repo "emacsorphanage/ox-pandoc"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id ox-pandoc :type git
                      :protocol https :inherit t :depth treeless :ref
                      "1caeb56a4be26597319e7288edbc2cabada151b4"))
 (package-lint :source "elpaca-menu-lock-file" :recipe
               (:package "package-lint" :fetcher github :repo
                         "purcell/package-lint" :files
                         (:defaults "data" (:exclude "*flymake.el"))
                         :source "MELPA" :id package-lint :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "1c37329703a507fa357302cf6fc29d4f2fe631a8"))
 (pandoc-mode :source "elpaca-menu-lock-file" :recipe
              (:package "pandoc-mode" :fetcher github :repo
                        "joostkremers/pandoc-mode" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id pandoc-mode :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "8f46da90228a9ce22de24da234ba53860257640a"))
 (paredit :source "elpaca-menu-lock-file" :recipe
          (:package "paredit" :fetcher git :url
                    "https://paredit.org/paredit.git" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id paredit :type git :protocol
                    https :inherit t :depth treeless :ref
                    "af075775af91f2dbc63b915d762b4aec092946c4"))
 (parinfer-rust-mode :source "elpaca-menu-lock-file" :recipe
                     (:package "parinfer-rust-mode" :repo
                               "justinbarclay/parinfer-rust-mode"
                               :fetcher github :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :id parinfer-rust-mode
                               :type git :protocol https :inherit t
                               :depth treeless :ref
                               "6ee9f905c41f6368689ad12aa99516b9ee4fb06d"))
 (parseclj :source "elpaca-menu-lock-file" :recipe
           (:package "parseclj" :repo "clojure-emacs/parseclj"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id parseclj :type git :protocol
                     https :inherit t :depth treeless :ref
                     "6af22372e0fe14df882dd300b22b12ba2d7e00b0"))
 (parseedn :source "elpaca-menu-lock-file" :recipe
           (:package "parseedn" :repo "clojure-emacs/parseedn"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id parseedn :type git :protocol
                     https :inherit t :depth treeless :ref
                     "3407e4530a367b6c2b857dae261cdbb67a440aaa"))
 (pcre2el :source "elpaca-menu-lock-file" :recipe
          (:package "pcre2el" :fetcher github :repo "joddie/pcre2el"
                    :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id pcre2el :type git :protocol
                    https :inherit t :depth treeless :ref
                    "b4d846d80dddb313042131cf2b8fbf647567e000"))
 (pdf-tools :source "elpaca-menu-lock-file" :recipe
            (:package "pdf-tools" :fetcher github :repo
                      "vedang/pdf-tools" :files
                      (:defaults "README" ("build" "Makefile")
                                 ("build" "server"))
                      :source "MELPA" :id pdf-tools :type git
                      :protocol https :inherit t :depth treeless :ref
                      "365f88238f46f9b1425685562105881800f10386"))
 (pet :source "elpaca-menu-lock-file" :recipe
      (:package "pet" :fetcher github :repo "wyuenho/emacs-pet" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id pet :type git :protocol https
                :inherit t :depth treeless :ref
                "222f1da892462d7bea5c7a7bbcb6b5a5f4cb2158"))
 (plz :source "elpaca-menu-lock-file"
   :recipe
   (:package "plz" :repo
             ("https://github.com/alphapapa/plz.el.git" . "plz") :tar
             "0.9.1" :host gnu :files
             ("*" (:exclude ".git" "LICENSE")) :source "GNU ELPA" :id
             plz :type git :protocol https :inherit t :depth treeless
             :ref "e2d07838e3b64ee5ebe59d4c3c9011adefb7b58e"))
 (plz-event-source :source "elpaca-menu-lock-file" :recipe
                   (:package "plz-event-source" :repo
                             ("https://github.com/r0man/plz-event-source"
                              . "plz-event-source")
                             :tar "0.1.3" :host gnu :files
                             ("*" (:exclude ".git")) :source
                             "GNU ELPA" :id plz-event-source :type git
                             :protocol https :inherit t :depth
                             treeless :ref
                             "de89214ce14e2b82cbfdc30e1adcf3e77b1f250a"))
 (plz-media-type :source "elpaca-menu-lock-file" :recipe
                 (:package "plz-media-type" :repo
                           ("https://github.com/r0man/plz-media-type"
                            . "plz-media-type")
                           :tar "0.2.4" :host gnu :files
                           ("*" (:exclude ".git")) :source "GNU ELPA"
                           :id plz-media-type :type git :protocol
                           https :inherit t :depth treeless :ref
                           "b1127982d53affff082447030cda6e8ead3899cb"))
 (poetry :source "elpaca-menu-lock-file" :recipe
         (:package "poetry" :fetcher github :repo "cybniv/poetry.el"
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id poetry :type git :protocol
                   https :inherit t :depth treeless :ref
                   "1dff0d4a51ea8aff5f6ce97b154ea799902639ad"))
 (popup :source "elpaca-menu-lock-file" :recipe
        (:package "popup" :fetcher github :repo
                  "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                   "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                   "doc/*.texinfo" "lisp/*.el" "docs/dir"
                   "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el"
                             "*-test.el" "*-tests.el" "LICENSE"
                             "README*" "*-pkg.el"))
                  :source "MELPA" :id popup :type git :protocol https
                  :inherit t :depth treeless :ref
                  "45a0b759076ce4139aba36dde0a2904136282e73"))
 (posframe :source "elpaca-menu-lock-file" :recipe
           (:package "posframe" :fetcher github :repo
                     "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id posframe :type git :protocol
                     https :inherit t :depth treeless :ref
                     "3a80911b2f45ce6926196930bb7d5cc662c7b3c8"))
 (pr-review :source "elpaca-menu-lock-file" :recipe
            (:package "pr-review" :fetcher github :repo
                      "blahgeek/emacs-pr-review" :files
                      (:defaults "graphql") :source "MELPA" :id
                      pr-review :type git :protocol https :inherit t
                      :depth treeless :ref
                      "1bb67e6a10869ccef75812f421d35b0366d95cf5"))
 (python-pytest :source "elpaca-menu-lock-file" :recipe
                (:package "python-pytest" :fetcher github :repo
                          "wbolster/emacs-python-pytest" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id python-pytest :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "78b5ea1d19c7e365ac00649d13c733954b11f822"))
 (pyvenv :source "elpaca-menu-lock-file" :recipe
         (:package "pyvenv" :fetcher github :repo
                   "jorgenschaefer/pyvenv" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id pyvenv :type git :protocol
                   https :inherit t :depth treeless :ref
                   "31ea715f2164dd611e7fc77b26390ef3ca93509b"))
 (queue :source "elpaca-menu-lock-file" :recipe
        (:package "queue" :repo
                  ("https://github.com/emacsmirror/gnu_elpa" . "queue")
                  :tar "0.2" :host gnu :branch "externals/queue"
                  :files ("*" (:exclude ".git")) :source "GNU ELPA"
                  :id queue :type git :protocol https :inherit t
                  :depth treeless :ref
                  "f986fb68e75bdae951efb9e11a3012ab6bd408ee"))
 (rainbow-delimiters :source "elpaca-menu-lock-file" :recipe
                     (:package "rainbow-delimiters" :fetcher github
                               :repo "Fanael/rainbow-delimiters"
                               :files
                               ("*.el" "*.el.in" "dir" "*.info"
                                "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi"
                                "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi"
                                "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el"
                                          "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE"
                                          "README*" "*-pkg.el"))
                               :source "MELPA" :id rainbow-delimiters
                               :type git :protocol https :inherit t
                               :depth treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (recursion-indicator :source "elpaca-menu-lock-file" :recipe
                      (:package "recursion-indicator" :repo
                                "minad/recursion-indicator" :fetcher
                                github :files
                                ("*.el" "*.el.in" "dir" "*.info"
                                 "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi"
                                 "doc/*.texinfo" "lisp/*.el"
                                 "docs/dir" "docs/*.info"
                                 "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el"
                                           "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE"
                                           "README*" "*-pkg.el"))
                                :source "MELPA" :id
                                recursion-indicator :type git
                                :protocol https :inherit t :depth
                                treeless :ref
                                "4805275937585102aba0047169f047032201c5b9"))
 (rg :source "elpaca-menu-lock-file" :recipe
     (:package "rg" :fetcher github :repo "dajva/rg.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "MELPA" :id rg :type git :protocol https
               :inherit t :depth treeless :ref
               "a614e7d7709c7bf5c5accff4003d351c3f28ee98"))
 (rust-mode :source "elpaca-menu-lock-file" :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id rust-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "668069ad8b6ca20bd0d2334db1c0d046809affd6"))
 (s :source "elpaca-menu-lock-file" :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
               "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
               "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el"
                         "*-test.el" "*-tests.el" "LICENSE" "README*"
                         "*-pkg.el"))
              :source "MELPA" :id s :type git :protocol https :inherit
              t :depth treeless :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (semext :source "elpaca-menu-lock-file" :recipe
         (:source nil :package "semext" :id semext :host github :repo
                  "ahyatt/semext" :type git :protocol https :inherit t
                  :depth treeless :ref
                  "6d05e243d066c2f8b3cd44081ea31cb1c445e535"))
 (sesman :source "elpaca-menu-lock-file" :recipe
         (:package "sesman" :repo "vspinu/sesman" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id sesman :type git :protocol
                   https :inherit t :depth treeless :ref
                   "7bca68dbbab0af26a6a23be1ff5fa97f9a18e022"))
 (shell-maker :source "elpaca-menu-lock-file" :recipe
              (:package "shell-maker" :fetcher github :repo
                        "xenodium/shell-maker" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id shell-maker :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "6377cbdb49248d670170f1c8dbe045648063583e"))
 (shrink-path :source "elpaca-menu-lock-file" :recipe
              (:package "shrink-path" :fetcher gitlab :repo
                        "bennya/shrink-path.el" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id shrink-path :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (smex :source "elpaca-menu-lock-file" :recipe
       (:package "smex" :repo "nonsequitur/smex" :fetcher github
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id smex :type git :protocol https
                 :inherit t :depth treeless :ref
                 "55aaebe3d793c2c990b39a302eb26c184281c42c"))
 (spinner :source "elpaca-menu-lock-file" :recipe
          (:package "spinner" :repo
                    ("https://github.com/Malabarba/spinner.el"
                     . "spinner")
                    :tar "1.7.4" :host gnu :files
                    ("*" (:exclude ".git")) :source "GNU ELPA" :id
                    spinner :type git :protocol https :inherit t
                    :depth treeless :ref
                    "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (string-inflection :source "elpaca-menu-lock-file" :recipe
                    (:package "string-inflection" :fetcher github
                              :repo "akicho8/string-inflection" :files
                              ("*.el" "*.el.in" "dir" "*.info"
                               "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi"
                               "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi"
                               "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el"
                                         "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE"
                                         "README*" "*-pkg.el"))
                              :source "MELPA" :id string-inflection
                              :type git :protocol https :inherit t
                              :depth treeless :ref
                              "4a2f87d7b47f5efe702a78f8a40a98df36eeba13"))
 (svg-lib :source "elpaca-menu-lock-file" :recipe
          (:package "svg-lib" :repo
                    ("https://github.com/rougier/svg-lib" . "svg-lib")
                    :tar "0.3" :host gnu :files
                    ("*" (:exclude ".git")) :source "GNU ELPA" :id
                    svg-lib :type git :protocol https :inherit t
                    :depth treeless :ref
                    "925ed4a0215c197ba836e7810a93905b34bea777"))
 (swift-mode :source "elpaca-menu-lock-file" :recipe
             (:package "swift-mode" :repo "swift-emacs/swift-mode"
                       :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                        "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        "docs/dir" "docs/*.info" "docs/*.texi"
                        "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el"
                                  "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source "MELPA" :id swift-mode :type git
                       :protocol https :inherit t :depth treeless :ref
                       "cfae3b85ad09bd293df941261afbc21e41bbb5f8"))
 (swift-ts-mode :source "elpaca-menu-lock-file" :recipe
                (:package "swift-ts-mode" :fetcher github :repo
                          "rechsteiner/swift-ts-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id swift-ts-mode :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "17806f6f56f09c86c5e70af239bea4313aaaf0b8"))
 (tablist :source "elpaca-menu-lock-file" :recipe
          (:package "tablist" :fetcher github :repo
                    "emacsorphanage/tablist" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id tablist :type git :protocol
                    https :inherit t :depth treeless :ref
                    "fcd37147121fabdf003a70279cf86fbe08cfac6f"))
 (tempel :source "elpaca-menu-lock-file" :recipe
         (:package "tempel" :repo "minad/tempel" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id tempel :type git :protocol
                   https :inherit t :depth treeless :ref
                   "2432d483a3ff63d3e80c8e8e23422f45cf87e971"))
 (tempel-collection :source "elpaca-menu-lock-file" :recipe
                    (:package "tempel-collection" :repo
                              "Crandel/tempel-collection" :fetcher
                              github :files (:defaults "templates")
                              :source "MELPA" :id tempel-collection
                              :type git :protocol https :inherit t
                              :depth treeless :ref
                              "6292604c1d5ed0044ce0beb2d46c73697dc66ed3"))
 (tomlparse :source "elpaca-menu-lock-file" :recipe
            (:package "tomlparse" :fetcher github :repo
                      "johannes-mueller/tomlparse.el" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id tomlparse :type git
                      :protocol https :inherit t :depth treeless :ref
                      "e9424ef34c3393ea813dd5933eab6bb7e16614fe"))
 (transient :source "elpaca-menu-lock-file" :recipe
            (:package "transient" :fetcher github :repo
                      "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id transient :type git
                      :protocol https :inherit t :depth treeless :ref
                      "e4e5f2ab1701ccc19d25361f1c574da6d33ac080"))
 (tree-inspector :source "elpaca-menu-lock-file" :recipe
                 (:package "tree-inspector" :repo
                           ("https://github.com/mmontone/emacs-tree-inspector"
                            . "tree-inspector")
                           :tar "0.4" :host gnu :files
                           ("*" (:exclude ".git")) :source "GNU ELPA"
                           :id tree-inspector :type git :protocol
                           https :inherit t :depth treeless :ref
                           "da1e5a9df92bdbbaef439e34466760f6a5d707fb"))
 (treepy :source "elpaca-menu-lock-file" :recipe
         (:package "treepy" :repo "volrath/treepy.el" :fetcher github
                   :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                    "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                    "doc/*.texinfo" "lisp/*.el" "docs/dir"
                    "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el"
                              "*-test.el" "*-tests.el" "LICENSE"
                              "README*" "*-pkg.el"))
                   :source "MELPA" :id treepy :type git :protocol
                   https :inherit t :depth treeless :ref
                   "28f0e2c2c75ea186e8beb570a4a70087926ff80b"))
 (treesit-auto :source "elpaca-menu-lock-file" :recipe
               (:package "treesit-auto" :fetcher github :repo
                         "renzmann/treesit-auto" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                          "*.texinfo" "doc/dir" "doc/*.info"
                          "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          "docs/dir" "docs/*.info" "docs/*.texi"
                          "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el"
                                    "tests.el" "*-test.el"
                                    "*-tests.el" "LICENSE" "README*"
                                    "*-pkg.el"))
                         :source "MELPA" :id treesit-auto :type git
                         :protocol https :inherit t :depth treeless
                         :ref
                         "31466e4ccfd4f896ce3145c95c4c1f8b59d4bfdf"))
 (treeview :source "elpaca-menu-lock-file" :recipe
           (:package "treeview" :repo "tilmanrassy/emacs-treeview"
                     :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                      "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                      "doc/*.texinfo" "lisp/*.el" "docs/dir"
                      "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el"
                                "*-test.el" "*-tests.el" "LICENSE"
                                "README*" "*-pkg.el"))
                     :source "MELPA" :id treeview :type git :protocol
                     https :inherit t :depth treeless :ref
                     "9a1a16f84fc3c368443641f7a71aa2407ad91d38"))
 (trinary :source "elpaca-menu-lock-file" :recipe
          (:package "trinary" :fetcher github :repo
                    "emacs-elsa/trinary-logic" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id trinary :type git :protocol
                    https :inherit t :depth treeless :ref
                    "d4869d260f22d13a9a71327a6d40edc6980d022e"))
 (try :source "elpaca-menu-lock-file" :recipe
      (:package "try" :fetcher github :repo "larstvei/Try" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                 "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                 "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el"
                           "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source "MELPA" :id try :type git :protocol https
                :inherit t :depth treeless :ref
                "8831ded1784df43a2bd56c25ad3d0650cdb9df1d"))
 (ts :source "elpaca-menu-lock-file" :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el"
                          "*-test.el" "*-tests.el" "LICENSE" "README*"
                          "*-pkg.el"))
               :source "MELPA" :id ts :type git :protocol https
               :inherit t :depth treeless :ref
               "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (typst-preview :source "elpaca-menu-lock-file" :recipe
                (:package "typst-preview" :fetcher github :repo
                          "havarddj/typst-preview.el" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                           "*.texinfo" "doc/dir" "doc/*.info"
                           "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           "docs/dir" "docs/*.info" "docs/*.texi"
                           "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el"
                                     "tests.el" "*-test.el"
                                     "*-tests.el" "LICENSE" "README*"
                                     "*-pkg.el"))
                          :source "MELPA" :id typst-preview :host
                          github :rev :newest :type git :protocol
                          https :inherit t :depth treeless :ref
                          "7e89cf105e4fef5e79977a4a790d5b3b18d305f6"))
 (typst-ts-mode :source "elpaca-menu-lock-file" :recipe
                (:package "typst-ts-mode" :repo
                          "meow_king/typst-ts-mode" :tar "0.12.2"
                          :host codeberg :files
                          ("*" (:exclude ".git")) :source
                          "NonGNU ELPA" :id typst-ts-mode :type git
                          :protocol https :inherit t :depth treeless
                          :ref
                          "cf4d5282988068841efd51dc83f99091b41b91dd"))
 (undo-fu :source "elpaca-menu-lock-file" :recipe
          (:package "undo-fu" :fetcher codeberg :repo
                    "ideasman42/emacs-undo-fu" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                     "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                     "doc/*.texinfo" "lisp/*.el" "docs/dir"
                     "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el"
                               "*-test.el" "*-tests.el" "LICENSE"
                               "README*" "*-pkg.el"))
                    :source "MELPA" :id undo-fu :type git :protocol
                    https :inherit t :depth treeless :ref
                    "5684ef2aef5f60176472916b21869cf221e018cc"))
 (verb :source "elpaca-menu-lock-file" :recipe
       (:package "verb" :repo "federicotdn/verb" :fetcher github
                 :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id verb :type git :protocol https
                 :inherit t :depth treeless :ref
                 "40ad1f06aac3373db788aedffd0eba113b80972f"))
 (vertico :source "elpaca-menu-lock-file" :recipe
          (:package "vertico" :repo "minad/vertico" :files
                    (:defaults "extensions/vertico-*.el") :fetcher
                    github :source "MELPA" :id vertico :type git
                    :protocol https :inherit t :depth treeless :ref
                    "f3c2033ba63880d6265cf1e1eb9e987792042fc4"))
 (vertico-posframe :source "elpaca-menu-lock-file" :recipe
                   (:package "vertico-posframe" :repo
                             ("https://github.com/tumashu/vertico-posframe"
                              . "vertico-posframe")
                             :tar "0.9.2" :host gnu :files
                             ("*" (:exclude ".git")) :source
                             "GNU ELPA" :id vertico-posframe :type git
                             :protocol https :inherit t :depth
                             treeless :ref
                             "d6e06a4f1b34d24cc0ca6ec69d2d6c965191b23e"))
 (vterm :source "elpaca-menu-lock-file" :recipe
        (:package "vterm" :fetcher github :repo
                  "akermu/emacs-libvterm" :files
                  ("CMakeLists.txt" "elisp.c" "elisp.h"
                   "emacs-module.h" "etc" "utf8.c" "utf8.h" "vterm.el"
                   "vterm-module.c" "vterm-module.h")
                  :source "MELPA" :id vterm :type git :protocol https
                  :inherit t :depth treeless :ref
                  "54c29d14bca05bdd8ae60cda01715d727831e3f9"))
 (vundo :source "elpaca-menu-lock-file" :recipe
        (:package "vundo" :repo "casouri/vundo" :tar "2.4.0" :host
                  github :files ("*" (:exclude ".git" "test")) :source
                  "GNU ELPA" :id vundo :type git :protocol https
                  :inherit t :depth treeless :ref
                  "e0af8c5845abf884a644215a9cac37f39c13cd5a"))
 (websocket :source "elpaca-menu-lock-file" :recipe
            (:package "websocket" :repo "ahyatt/emacs-websocket"
                      :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id websocket :type git
                      :protocol https :inherit t :depth treeless :ref
                      "2195e1247ecb04c30321702aa5f5618a51c329c5"))
 (wgrep :source "elpaca-menu-lock-file" :recipe
        (:package "wgrep" :fetcher github :repo
                  "mhayashi1120/Emacs-wgrep" :files ("wgrep.el")
                  :source "MELPA" :id wgrep :type git :protocol https
                  :inherit t :depth treeless :ref
                  "49f09ab9b706d2312cab1199e1eeb1bcd3f27f6f"))
 (with-editor :source "elpaca-menu-lock-file"
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor"
             :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
              "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
              "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el"
                        "*-test.el" "*-tests.el" "LICENSE" "README*"
                        "*-pkg.el"))
             :source "MELPA" :id with-editor :type git :protocol https
             :inherit t :depth treeless :ref
             "64211dcb815f2533ac3d2a7e56ff36ae804d8338"))
 (ws-butler :source "elpaca-menu-lock-file" :recipe
            (:package "ws-butler" :fetcher git :url
                      "https://https.git.savannah.gnu.org/git/elpa/nongnu.git"
                      :branch "elpa/ws-butler" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id ws-butler :type git
                      :protocol https :inherit t :depth treeless :ref
                      "67c49cfdf5a5a9f28792c500c8eb0017cfe74a3a"))
 (xterm-color :source "elpaca-menu-lock-file" :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color"
                        :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id xterm-color :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "86fab1d247eb5ebe6b40fa5073a70dfa487cd465"))
 (yaml :source "elpaca-menu-lock-file" :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
                  "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
                  "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el"
                            "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source "MELPA" :id yaml :type git :protocol https
                 :inherit t :depth treeless :ref
                 "f2369fb4985ed054be47ae111760ff2075dff72a"))
 (yaml-mode :source "elpaca-menu-lock-file" :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher
                      github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                       "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi"
                       "doc/*.texinfo" "lisp/*.el" "docs/dir"
                       "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el"
                                 "*-test.el" "*-tests.el" "LICENSE"
                                 "README*" "*-pkg.el"))
                      :source "MELPA" :id yaml-mode :type git
                      :protocol https :inherit t :depth treeless :ref
                      "d91f878729312a6beed77e6637c60497c5786efa"))
 (yasnippet :source "elpaca-menu-lock-file" :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet"
                      :fetcher github :files
                      ("yasnippet.el" "snippets") :source "MELPA" :id
                      yasnippet :type git :protocol https :inherit t
                      :depth treeless :ref
                      "c1e6ff23e9af16b856c88dfaab9d3ad7b746ad37"))
 (zoom-window :source "elpaca-menu-lock-file" :recipe
              (:package "zoom-window" :fetcher github :repo
                        "emacsorphanage/zoom-window" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi"
                         "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         "docs/dir" "docs/*.info" "docs/*.texi"
                         "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el"
                                   "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source "MELPA" :id zoom-window :type git
                        :protocol https :inherit t :depth treeless
                        :ref
                        "8a0ae04de53583949a58e0aa8e7f64f03be7c9f8")))
