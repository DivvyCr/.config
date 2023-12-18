(defvar dv/leader-key ","
  "This key is used as a prefix for my universal, global keybinds.")

(defvar dv/switcher-key "SPC"
  "This key is used as a prefix for my global 'context switch' keybinds.")

(defvar dv/keymap (make-keymap)
  "My keymap to override vital keybinds.")

(define-minor-mode dv/keybinds-mode
  "My global minor-mode to house `dv/keymap'."
  :global t
  :keymap dv/keymap)
(dv/keybinds-mode)

;;
;; Custom keybinds:
;;

(use-package general)

;; Override non-evil keybinds via my minor-mode keymap:
(general-def dv/keymap
  "C--" "<escape>"
  "C-t" #'execute-extended-command
  )

;; Override evil keybinds via my minor-mode keymap:
(general-def dv/keymap
  :states '(normal insert visual replace motion emacs nil)
  "C--" "<escape>"
  "C-t" #'execute-extended-command
  )

;; My utility keybinds:
(general-def '(normal visual motion)
  :prefix dv/leader-key
  "" '(nil :which-key "Leader") ;; Unbind `dv/leader-key' to use as a prefix
  dv/leader-key #'evil-ex ;; Double-tap leader for `evil-ex'

  "b" #'ibuffer
  "d" #'(lambda () (interactive) (dired "."))
  "o" #'delete-other-windows
  "q" #'evil-quit
  "w" #'evil-write
  "/" #'consult-line
  "`" #'(lambda () (interactive) (load-file (buffer-file-name)))
  )

;; My context-switch keybinds:
(general-def '(normal visual motion)
  :prefix dv/switcher-key
  "" '(nil :which-key "Navigation") ;; Unbind `SPC' to use as a prefix
  dv/switcher-key #'consult-projectile
  
  "/" #'consult-ripgrep
  "`" #'((lambda () (interactive) (consult-fd "~/.config/")) :which-key "Search Config")
  "~" #'((lambda () (interactive) (consult-fd "~/")) :which-key "Search Home")
  "b" #'dv/maybe-project-switch-buffer
  "B" #'consult-buffer ;; Guaranteed global buffer switch
  "d" #'dv/maybe-project-find-directory
  "f" #'dv/maybe-project-find-file
  "w" #'dv/split-switch-window-right
  "W" #'dv/split-switch-window-below
  "m" #'consult-mark
  )

;; Always use ESC to quit:
(general-def
  :keymaps '(minibuffer-local-map
	     minibuffer-local-ns-map
	     minibuffer-local-completion-map
	     minibuffer-local-isearch-map)
  "<escape>" #'minibuffer-keyboard-quit
  )

;; Basic non-evil re-binds:
(general-def 
  "<escape>" #'keyboard-quit
  "C-=" #'eval-expression ;; Shadows `undo'
  "C-h C" #'describe-char ;; Shadows `describe-coding-system'
  "C-h M" #'describe-keymap
  )

;; Basic evil re-binds:
(general-def '(normal visual motion)
  "j" #'evil-next-visual-line
  "k" #'evil-previous-visual-line
  )

;; Un-binds:
(general-unbind 'insert "C-n" "C-p") ;; Using `corfu' for completion.
(general-unbind '(normal motion) ":") ;; Using `<leader><leader>', see above.
(general-unbind
  :prefix "C-x"
  "C-+" "C--" ;; Need `C--' for `keyboard-quit'; use `C-0' to adjust scale.
  )

;;
;; Vi-like keybinds:
;;

(use-package evil
  :init ;; Necessary for `evil-collection'
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq evil-echo-state nil)
  (setq evil-search-module 'evil-search)
  (setq evil-symbol-word-search t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-empty-ex-last-command nil) ;; Do not auto-complete last `evil-ex' command.
  )

(use-package evil-collection
  :after evil
  ;; Might want to limit this only to relevant modes:
  :config (evil-collection-init)
  (setq evil-collection-magit-use-y-for-yank nil)
  (setq evil-collection-magit-use-z-for-folds t)
  (setq evil-collection-magit-want-horizontal-movement t)
  )

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode))

(provide 'configure-keybinds)
