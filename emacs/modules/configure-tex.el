;; Useful Resources:
;;  https://tex.stackexchange.com/questions/20843/useful-shortcuts-or-key-bindings-or-predefined-commands-for-emacsauctex
;;  https://tex.stackexchange.com/questions/50827/a-simpletons-guide-to-tex-workflow-with-emacs
;;  https://github.com/tecosaur/emacs-config/blob/master/config.org#latex

(use-package tex
  :ensure auctex
  :config
  (setq-default TeX-master nil)
  (setq TeX-parse-self t) ;; Parse on load
  (setq TeX-auto-save t) ;; Parse on save
  (setq TeX-PDF-mode t) ;; PDF instead of DVI

  (add-hook 'TeX-mode-hook (lambda ()
			     (visual-line-mode)
			     (latex-math-mode)
			     (reftex-mode)
			     (TeX-fold-mode)
			     (TeX-source-correlate-mode)))

  ;; Recenter on selected source line in the output file:
  (setq TeX-command-extra-options "--synctex=1")
  (setq TeX-source-correlate-start-server t)

  ;; Automatically 'refresh' the output file:
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package pdf-tools
  :config
  (pdf-tools-install)

  ;; Override `global-display-line-numbers-mode':
  (add-hook 'pdf-view-mode-hook #'(lambda () (display-line-numbers-mode -1)))

  ;; External programs are hard to manage on WSL2 alongside Emacs:
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))

(provide 'configure-tex)
