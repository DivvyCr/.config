(setq completion-ignore-case t
      completion-cycle-threshold 3
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

(use-package vertico
  :bind (:map vertico-map
	      ("<tab>" . minibuffer-complete)
	      ("C-<return>" . vertico-insert)
	      ("<return>" . vertico-exit))
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :custom
  (vertico-resize t)
  (vertico-cycle nil)
  (vertico-multiform-categories
   '((t reverse)))
  )

(use-package marginalia
  :init
  (marginalia-mode)
  )

(use-package orderless
  :custom
  (completion-styles
   '(basic substring orderless))

  (orderless-matching-styles
   '(orderless-flex orderless-prefixes orderless-regexp))

  (orderless-affix-dispatch-alist
   '((?= . orderless-literal)
     (?, . orderless-initialism)))

  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))))
  )

(use-package corfu
  :bind (:map corfu-map
	      ("C-n" . corfu-next)
	      ("C-p" . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-popupinfo-delay '(1.5 . 1.0))
  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (bound-and-true-p vertico--input)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  )

;; TODO:
; (use-package embark)

(provide 'configure-completion)
