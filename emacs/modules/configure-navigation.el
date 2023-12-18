(setq dired-recursive-copies t)
(setq dired-recursive-deletes t)
(setq dired-listing-switches "-AFGhlv --group-directories-first")
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(add-hook 'dired-mode-hook 'hl-line-mode)

;; (use-package dired-plus
;;   :vc (:fetcher github :repo "emacsmirror/dired-plus"))

(use-package projectile
  :custom
  (projectile-cache-file
   (dv/get-cache-path "projectile-cache.el"))
  (projectile-known-projects-file
   (dv/get-cache-path "projectile-projects.el"))
  (projectile-track-known-projects-automatically nil)
  :config
  (projectile-mode)
  )

(use-package consult
  :config
  (setq consult-narrow-key "<")
  (setq consult-widen-key ">")
  )

(use-package consult-lsp
  :after (consult lsp-mode))

(use-package consult-projectile
  :after (consult projectile)
  :config
  (defun dv/maybe-project-switch-buffer ()
    (interactive)
    (if (projectile-project-p)
	(consult-projectile-switch-to-buffer)
      (consult-buffer)))
  (defun dv/maybe-project-find-directory ()
    (interactive)
    (if (projectile-project-p)
	(consult-projectile-find-dir)
      (let ((consult-fd-args (append consult-fd-args (list "-t d"))))
	(consult-fd))))
  (defun dv/maybe-project-find-file ()
    (interactive)
    (if (projectile-project-p)
	(consult-projectile-find-file)
      (consult-fd)))
  )

(defun dv/split-switch-window-right ()
  "Split the window vertically and switch to it."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun dv/split-switch-window-below ()
  "Split the window horizontally and switch to it."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(provide 'configure-navigation)
