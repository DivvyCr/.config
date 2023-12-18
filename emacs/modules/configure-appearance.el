(setq
 inhibit-startup-message t
 frame-title-format '("Emacs | %b")
 help-window-select t
 select-enable-clipboard t
 use-short-answers t
 custom-safe-themes t
 )

(setq-default
 fill-column 80
 display-line-numbers-type 'relative
 scroll-conservatively most-positive-fixnum
 )

(show-paren-mode t)
(delete-selection-mode t)
(global-display-line-numbers-mode t)

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1))

(use-package all-the-icons
  :if (display-graphic-p)
  :custom
  (all-the-icons-default-adjust 0)
  (all-the-icons-scale-factor 1))

(require 'dv-modeline)
(with-eval-after-load 'all-the-icons (dv/modeline))

(use-package which-key
  :init (which-key-mode))

(use-package helpful
  :bind
  ([remap display-local-help] . helpful-at-point)
  ([remap describe-variable]  . helpful-variable)
  ([remap describe-function]  . helpful-callable)
  ([remap describe-command]   . helpful-command)
  ([remap describe-symbol]    . helpful-symbol)
  ([remap describe-key]       . helpful-key)
  :config
  (advice-add 'helpful--heading
	      :override #'(lambda (text) (propertize (concat text ":\n") 'face 'helpful-heading)))
  :custom
  (helpful-max-buffers 2))

(setq custom-theme-directory
      (expand-file-name "modules/themes/" user-emacs-directory))
(load-theme 'dv t)

(add-to-list 'default-frame-alist '(font . "JetBrainsMono NF-15.0"))
(set-face-attribute 'default t :font "JetBrainsMono NF-15.0")

(provide 'configure-appearance)
