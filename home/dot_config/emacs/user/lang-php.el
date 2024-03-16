;;; lang-php.el --- -*- lexical-binding: t; -*-

(use-package php-mode)

(use-package composer
  :ensure
  (:host github :repo "zemacs-php/composer.el" :branch "master" :files ("*.el" "composer.json" "composer.lock" (:exclude "*test.el"))))

(use-package phpactor
  :ensure
  (:host github :repo "zemacs-php/phpactor.el" :branch "master" :files ("*.el" "composer.json" "composer.lock" (:exclude "*test.el"))))

(provide 'lang-php)

;;; lang-php.el ends here
