;;
;; Spell-checking:
;;

(use-package jinx
  :init
  (global-jinx-mode)
  :config
  (setq jinx-languages "en_GB"))

;;
;; Completion:
;;

(setq completion-ignore-case t
      completion-cycle-threshold 3
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(use-package vertico
  :bind (:map vertico-map
	      ("<tab>" . minibuffer-complete)
	      ("C-<return>" . vertico-insert)
	      ("<return>" . vertico-exit)
	      ("C-n" . vertico-next)
	      ("C-p" . vertico-previous)
	      ("M-n" . vertico-next-group)
	      ("M-p" . vertico-previous-group))
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :config
  (setq vetrico-count 8)
  (setq vertico-resize t)
  (setq vertico-cycle nil)
  (setq vertico-multiform-categories
	'((jinx grid (vertico-grid-annotate . 10))
	  (t reverse)))

  (defun dv/vertico-candidate-padding (f cand prefix suffix index _start)
    "Add a space as padding to the left of each candidate."
    (funcall f cand (concat " " prefix) suffix index _start))
  (advice-add #'vertico--format-candidate :around #'dv/vertico-candidate-padding)
  )

(use-package orderless
  :config
  (setq completion-styles
	'(basic substring orderless))

  (setq orderless-matching-styles
	'(orderless-flex orderless-prefixes orderless-regexp))

  (setq orderless-affix-dispatch-alist
	'((?= . orderless-literal)
	  (?, . orderless-initialism)))

  (setq completion-category-defaults nil)
  (setq completion-category-overrides
	'((file (styles . (basic partial-completion orderless)))))
  )

(use-package corfu
  :bind (:map corfu-map
	      ("C-n" . corfu-next)
	      ("C-p" . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-auto t)
  (setq corfu-popupinfo-delay '(1.5 . 1.0))
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  )

(use-package marginalia
  :init (marginalia-mode))

;; TODO:
; (use-package embark)

(provide 'configure-completion)
