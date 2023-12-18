(setq default-directory "~/"
      gc-cons-threshold (* 128 1024 1024)
      read-process-output-max (* 1024 1024)
      native-comp-async-report-warnings-errors nil)

(add-to-list 'native-comp-eln-load-path
             (expand-file-name "cache/eln-cache/" user-emacs-directory))
(add-to-list 'load-path
             (expand-file-name "modules/" user-emacs-directory))
(require 'configure-package)
(require 'configure-cache)
(require 'configure-navigation)
(require 'configure-tex)
(require 'configure-coding)
(require 'configure-completion)
(require 'configure-appearance)
(require 'configure-keybinds)
