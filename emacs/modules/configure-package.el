(setq package-user-dir (expand-file-name "packages/" user-emacs-directory)
      package-archives '(("org" . "http://orgmode.org/elpa/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

; Check whether package was initialised, if not, prevent double-initialisation and initialise.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t) ;; Make all expanded code as minimal as possible.
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(provide 'configure-package)
