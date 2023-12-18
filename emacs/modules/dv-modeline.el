(require 'all-the-icons)
(require 'projectile)

;;
;; Faces.
;;

(defface dv/modeline-default '((t :inherit default)) "")
(defface dv/modeline-path-base-face '((t :inherit dv/modeline-default)) "")
(defface dv/modeline-path-main-face '((t :inherit dv/modeline-default)) "")
(defface dv/modeline-path-file-face '((t :inherit dv/modeline-default)) "")

(defface dv/modeline-inactive-default '((t :inherit default)) "")
(defface dv/modeline-path-base-inactive-face '((t :inherit dv/modeline-inactive-default)) "")
(defface dv/modeline-path-main-inactive-face '((t :inherit dv/modeline-inactive-default)) "")
(defface dv/modeline-path-file-inactive-face '((t :inherit dv/modeline-inactive-default)) "")

(defface dv/modeline-unsaved '((t :inherit default)) "")
(defface dv/modeline-n-face '((t :inherit default)) "")
(defface dv/modeline-i-face '((t :inherit default)) "")
(defface dv/modeline-v-face '((t :inherit default)) "")
(defface dv/modeline-r-face '((t :inherit default)) "")

(set-face-attribute 'mode-line nil :inherit 'dv/modeline-default)
(set-face-attribute 'mode-line-active nil :inherit 'dv/modeline-default)
(custom-set-faces '(mode-line-inactive ((t (:inherit 'dv/modeline-inactive-default)))))

;;
;; Utilities for `header-line' content.
;;

(defun dv/modeline-status ()
  "Return a custom, propretised icon for each of read-only,
read-write (saved), read-write (not saved)."
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond
     (modified  (all-the-icons-material "save" :face 'dv/modeline-unsaved :height 0.9))
     (read-only (all-the-icons-material "info_outline"))
     (t         ""))))

(defun dv/modeline-compose (mode left-content right-content)
  "Return the string of contents for `header-line'."
  (let* ((content-width (+ (length (dv/modeline-status))
			   (length mode)
			   (length left-content)
			   (length right-content)
			   8)) ;; This is to account for arbitrary spaces/symbols.
	 (extra-width (/ (window-right-divider-width)
			 (window-font-width nil 'header-line)))
	 (available-width (max 1 (- (window-total-width)
				    content-width extra-width 2))))
    (concat
     (dv/modeline-segment-evil-state)
     " " mode
     " " left-content
     " " (dv/modeline-status)
     ; (make-string available-width ?\ ) ;; Space between the left and the right.
     ; right-content
     )))

;;
;; The configuration.
;;

(defun dv/modeline-segment-evil-state ()
  ""
  (if (mode-line-window-selected-p)
      (propertize " " 'face (cond
			     ((evil-normal-state-p) 'dv/modeline-n-face)
			     ((evil-insert-state-p) 'dv/modeline-i-face)
			     ((evil-visual-state-p) 'dv/modeline-v-face)
			     ((evil-replace-state-p) 'dv/modeline-r-face)
			     (t 'dv/modeline-default)))
    " "))

(defun dv/modeline-segment-project-path (root)
  "Return a propertized path from ROOT to current file."
  (let ((root (directory-file-name root))
	(base (if vc-mode
		  (concat (substring-no-properties vc-mode 5) ":/")
		(concat (file-name-nondirectory root) "/")))
	(path (file-name-directory (file-relative-name buffer-file-name root))))
    (concat
     (propertize base
		 'face (if (mode-line-window-selected-p)
			   'dv/modeline-path-base-face
			 'dv/modeline-path-base-inactive-face))
     (propertize (if path path "")
		 'face (if (mode-line-window-selected-p)
			   'dv/modeline-path-main-face
			 'dv/modeline-path-main-inactive-face))
     (propertize (buffer-name)
		 'face (if (mode-line-window-selected-p)
			   'dv/modeline-path-file-face
			 'dv/modeline-path-file-inactive-face)))))

;; ---

(defun dv/modeline-mode-default ()
  "Return a basic string that describes the major mode, buffer name, and version control."
  (dv/modeline-compose (format-mode-line 'mode-name)
                       (buffer-name)
                       ""))

(defun dv/modeline-mode-minimal ()
  "Return a minimal string that describes the major mode, and the buffer name."
  (dv/modeline-compose (format-mode-line 'mode-name)
                       (buffer-name)
                       ""))

(defun dv/modeline-mode-project ()
  "Return a string that describes the major mode, path to file from project, and version control."
  (dv/modeline-compose (format-mode-line 'mode-name)
		       (dv/modeline-segment-project-path (projectile-project-root))
		       ""))

;;
;; The integration.
;;

(setq-default mode-line-format nil)

(defun dv/modeline ()
  "Enable my custom modeline (in the `header-line')."
  (interactive)
  (setq-default mode-line-format '((:eval
				    (cond ((not (buffer-file-name))    (dv/modeline-mode-minimal))
					  ((projectile-project-p)      (dv/modeline-mode-project))
					  (t                           (dv/modeline-mode-minimal))
					  )))))

(provide 'dv-modeline)
