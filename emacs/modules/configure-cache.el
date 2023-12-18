(defvar dv/cache-directory
  (expand-file-name "cache" user-emacs-directory))

(defun dv/get-cache-path (filename)
  (expand-file-name filename dv/cache-directory))

(setq custom-file (dv/get-cache-path "custom.el"))
(ignore-errors (load custom-file))

(setq auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
	 ,(concat (file-name-as-directory temporary-file-directory) "\\2") t)
	(".*" ,(dv/get-cache-path "auto-saves/") t)))
(setq auto-save-list-file-prefix (dv/get-cache-path "auto-saves/.save-"))

(setq backup-directory-alist
      `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
	("." . ,(dv/get-cache-path "backups/"))))
(setq backup-by-copying t
      delete-old-versions t)

(setq transient-history-file (dv/get-cache-path "transient/history.el")
      transient-levels-file (dv/get-cache-path "transient/levels.el")
      transient-values-file (dv/get-cache-path "transient/values.el"))

(provide 'configure-cache)
