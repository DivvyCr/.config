(deftheme dv
  "Divvy's theme.")

(defconst dv/colours
  '(;; Fundamentals:
    (light  . (list "#b0b8c7" "#bbc2cf" "#c6ccd7" "#d2d7df" "#dde1e7")) ;; Based on Atom-One fg.
    (mid    . (list "#575d69" "#757c89" "#929aa8")) ;; Midpoints between darkest `light' and lightest `dark'.
    (dark   . (list "#17191e" "#1f2329" "#282c34" "#31353f" "#393f4a")) ;; Based on Atom-One bg.

    ;; Primary accents:
    (blue   . (list "#5289eb" "#6495ed" "#76a1ef" "#87adf1" "#99b9f3")) ;; Based on corn-flower blue.
    (orange . (list "#e9ad41" "#ebb452" "#edbc64" "#efc476" "#f1cb87")) ;; Based on the complementary of the above.

    ;; Secondary accents:
    (red    . (list "#ff5451" "#ff6d6a" "#ff8684")) ;; Based on pastel red.
    (green  . (list "#89e289" "#99e699" "#a9eaa9")) ;; Based on granny smith apple.

    ;; Other:
    (purple . (list "#aa7bf0" "#b58df2" "#c19ef4")) ;; Based on amethyst (approx.)
    (yellow . (list "#ffe662" "#ffe976" "#ffec89")) ;; Based on gold (approx.)
    ))

(defun dv/get-colour (colour variation)
  "Return a hex string for the VARIATION (1-5) of COLOUR from `dv/colours'.
The variation is either a shade, a tint, or the saturation of the colour."
  (nth variation (cdr (assoc colour dv/colours))))

(custom-theme-set-faces 'dv
 ;; ------------
 ;; FUNDAMENTALS

 `(default ((t (:foreground "#dfdfdf" :background "#1b1b1b"))))
 `(shadow  ((t (:background ,(dv/get-colour 'dark 2) ))))
 `(hl-line ((t (:background ,(dv/get-colour 'dark 2) :extend t))))

 `(internal-border ((t (:inherit shadow))))
 `(fringe          ((t (:inherit default))))
 `(window-divider  ((t (:foreground "#1b1b1b" :background "#1b1b1b"))))
 `(window-divider-first-pixel ((t (:inherit window-divider))))
 `(window-divider-last-pixel  ((t (:inherit window-divider))))

 `(region ((t (:background "#444444" :extend t))))

 `(highlight             ((t (:inherit region))))
 `(lazy-highlight        ((t (:inherit region))))
 `(highlight-symbol-face ((t (:inherit highlight))))

 `(line-number              ((t (:inherit default :foreground "#666666"))))
 `(line-number-current-line ((t (:inherit default :foreground "#eeeeee"))))

 `(error   ((t (:foreground ,(dv/get-colour 'red    1)))))
 `(warning ((t (:foreground ,(dv/get-colour 'orange 3)))))
 `(success ((t (:foreground ,(dv/get-colour 'green  2)))))

 `(variable-pitch ((t (:font "CaskaydiaMono Nerd Font-14.0"))))

 ;; ------
 ;; SYNTAX

 ;; Generic
 `(font-lock-builtin-face      ((t (:foreground ,(dv/get-colour 'blue 4)))))
 `(font-lock-keyword-face      ((t (:foreground ,(dv/get-colour 'blue 1) :bold t))))

 ;; Action
 `(font-lock-function-name-face ((t (:foreground ,(dv/get-colour 'blue   2) :slant italic))))
 `(font-lock-variable-name-face ((t (:foreground ,(dv/get-colour 'blue   5)))))
 `(font-lock-type-face          ((t (:foreground ,(dv/get-colour 'orange 4)))))
 `(font-lock-constant-face      ((t (:foreground ,(dv/get-colour 'blue   3)))))
 `(font-lock-number-face        ((t (:foreground "#ff8ad5"))))

 ;; Text
 `(font-lock-comment-face           ((t (:foreground ,(dv/get-colour 'mid   1)))))
 `(font-lock-comment-delimiter-face ((t (:foreground ,(dv/get-colour 'dark  5)))))
 `(font-lock-string-face            ((t (:foreground ,(dv/get-colour 'green 3)))))
 `(font-lock-doc-face               ((t (:foreground ,(dv/get-colour 'green 1) :italic t))))

 ;; Other
 `(font-lock-negation-char-face ((t (:foreground ,(dv/get-colour 'red 2) :bold t))))
 `(font-lock-warning-face       ((t (:inherit warning))))

 ;; -----
 ;; EMACS

 `(isearch ((t (:inverse-video t))))
 `(header-line ((t (:inherit dv/modeline-default))))
 `(minibuffer-prompt ((t (:foreground ,(dv/get-colour 'blue 4)))))
 
 `(completions-annotations ((t (:italic t))))
 `(completions-common-part      ((t (:foreground ,(dv/get-colour 'mid  1) :background nil))))
 `(completions-first-difference ((t (:inherit minibuffer-prompt :bold t :underline t))))

 `(link         ((t (:foreground "#64C3ED" :underline t))))
 `(link-visited ((t (:foreground "#92E5F2" :underline t))))
 `(button       ((t (:inherit link))))

 `(show-paren-match    ((t (:foreground "#ff8ad5" :bold t))))
 `(show-paren-mismatch ((t (:background ,(dv/get-colour 'red 2) :underline t))))

 `(vertical-border ((t (:inherit shadow :foreground ,(dv/get-colour 'dark 2)))))
 `(separator-line ((t (:inherit default :extend t))))

 `(message-header-name ((t (:inherit font-lock-keyword-face))))
 `(message-header-subject ((t (:inherit default :bold t :font "CaskaydiaMono Nerd Font-15.0"))))
 `(message-header-to ((t (:inherit font-lock-doc-face))))
 `(message-header-cc ((t (:inherit font-lock-string-face))))
 `(message-header-newsgroups ((t (:inherit font-lock-warning-face))))
 `(message-header-other ((t (:inherit font-lock-builtin-face :italic t))))
 `(message-header-xheader ((t (:inherit message-header-other))))

 `(shr-h6 ((t (:inherit shr-text :foreground ,(dv/get-colour 'blue 3)))))
 `(shr-h5 ((t (:inherit shr-h6))))
 `(shr-h4 ((t (:inherit shr-h6))))
 `(shr-h3 ((t (:inherit shr-h6 :bold t))))
 `(shr-h2 ((t (:foreground ,(dv/get-colour 'blue 2) :bold t :height 1.2))))
 `(shr-h1 ((t (:foreground ,(dv/get-colour 'blue 1) :bold t :height 1.4))))

 ; ;; --------
 ; ;; PACKAGES

 `(dv/modeline-default ((t (:foreground "#dcdcdc" :background "#121212" :bold t))))
 `(dv/modeline-path-base-face ((t (:inherit dv/modeline-default :foreground "#2f71e7"))))
 `(dv/modeline-path-main-face ((t (:inherit dv/modeline-default :foreground "#5289eb"))))
 `(dv/modeline-path-file-face ((t (:inherit dv/modeline-default :foreground "#7ba5f0"))))

 `(dv/modeline-inactive-default ((t (:background "#222222" :bold t))))
 `(dv/modeline-path-base-inactive-face ((t (:inherit dv/modeline-inactive-default :foreground "#888888"))))
 `(dv/modeline-path-main-inactive-face ((t (:inherit dv/modeline-inactive-default :foreground "#ababab"))))
 `(dv/modeline-path-file-inactive-face ((t (:inherit dv/modeline-inactive-default :foreground "#dcdcdc"))))

 `(dv/modeline-unsaved ((t (:inherit error :background nil))))
 `(dv/modeline-n-face ((t (:background "#3676e8"))))
 `(dv/modeline-i-face ((t (:background "#85ea4d"))))
 `(dv/modeline-v-face ((t (:background "#eab24d"))))
 `(dv/modeline-r-face ((t (:background "#ea644d"))))

 `(help-key-binding ((t (:foreground "#ff8ad5" :background nil))))
 `(helpful-heading ((t (:foreground "#ffffff" :background nil :bold t))))

 `(which-key-key-face		      ((t (:foreground ,(dv/get-colour 'orange 4) :bold t))))
 `(which-key-group-description-face   ((t (:foreground ,(dv/get-colour 'orange 4) :bold nil))))
 `(which-key-command-description-face ((t (:foreground ,(dv/get-colour 'blue   4)))))
 
 `(corfu-current ((t :inherit highlight :foreground nil)))

 `(vertico-current ((t (:inherit highlight :foreground nil))))
 `(vertico-group-separator ((t (:background "#121212" :underline (:position -5)))))
 `(vertico-group-title ((t (:background "#121212" :bold t :underline (:position -5)))))
 
 `(consult-highlight-mark ((t (:underline t))))
 `(consult-highlight-match ((t (:underline t))))
 `(consult-projectile-projects ((t (nil))))
 `(consult-bookmark ((t (nil))))
 `(consult-buffer ((t (nil))))
 `(consult-file ((t (nil))))

 `(orderless-match-face-0 ((t (:inherit completions-first-difference :foreground "#ff8ad5" :underline nil))))
 `(orderless-match-face-1 ((t (:inherit orderless-match-face-0))))
 `(orderless-match-face-2 ((t (:inherit orderless-match-face-0))))
 `(orderless-match-face-3 ((t (:inherit orderless-match-face-0))))

 `(marginalia-documentation ((t (:inherit font-lock-doc-face :background nil))))
 `(marginalia-file-priv-no  ((t (:foreground nil :background nil))))

 `(elfeed-search-date-face ((t (:inherit font-lock-comment-face))))
 `(elfeed-search-feed-face ((t (:inherit font-lock-string-face))))
 `(elfeed-search-tag-face ((t (:inherit font-lock-type-face))))
 `(elfeed-search-title-face ((t (:foreground ,(dv/get-colour 'light 1) :bold nil))))
 `(elfeed-search-unread-title-face ((t (:inherit default :bold t))))

 `(lsp-headerline-breadcrumb-separator-face ((t (:background "#121212" :height 0.8))))
 `(lsp-face-semhl-default-library ((t (:foreground nil :background nil))))
 `(lsp-face-semhl-definition ((t (:inherit nil))))
 `(lsp-face-semhl-interface ((t (:foreground nil :background nil))))
 `(lsp-face-semhl-operator ((t (:inherit default))))
 `(lsp-face-semhl-property ((t (:inherit font-lock-function-call-face))))
 `(lsp-face-semhl-number ((t (:inherit font-lock-number-face))))

 ; `(flycheck-error   ((t (:background "#70393E")))) ;; Custom shade to imitate opacity.
 ; `(flycheck-warning ((t (:background "#685738")))) ;; Custom shade to imitate opacity.
 ; `(flycheck-info    ((t (:background "#41554A")))) ;; Custom shade to imitate opacity.

 ; `(flycheck-posframe-background-face ((t (:background ,(dv/get-colour 'dark 2)))))
 ; `(flycheck-posframe-border-face     ((t (:foreground ,(dv/get-colour 'mid  1)))))
 ; `(flycheck-posframe-error-face      ((t (:foreground ,(dv/get-colour 'red    1) :height 1.1))))
 ; `(flycheck-posframe-warning-face    ((t (:foreground ,(dv/get-colour 'orange 2) :height 1.1))))
 ; `(flycheck-posframe-info-face       ((t (:foreground ,(dv/get-colour 'green  2) :height 1.1))))

 `(TeX-fold-folded-face ((t nil)))

 `(font-latex-bold-face ((t (:foreground "#ffffff" :bold t))))
 `(font-latex-italic-face ((t (:foreground "#ffffff" :italic t))))
 `(font-latex-warning-face      ((t (:inherit error :bold t))))
 `(font-latex-math-face         ((t (:foreground ,(dv/get-colour 'orange 4)))))
 `(font-latex-script-char-face  ((t (:foreground ,(dv/get-colour 'mid    3))))) ;
 `(font-latex-string-face       ((t (:inherit font-lock-string-face))))
 `(font-latex-verbatim-face     ((t (:foreground ,(dv/get-colour 'purple 2)))))
 `(font-latex-sectioning-0-face ((t (:inherit default :foreground ,(dv/get-colour 'blue 1) :bold t :height 1.10))))
 `(font-latex-sectioning-1-face ((t (:inherit default :foreground ,(dv/get-colour 'blue 2) :bold t :height 1.05))))
 `(font-latex-sectioning-2-face ((t (:inherit default :foreground ,(dv/get-colour 'blue 3) :bold t))))
 `(font-latex-sectioning-3-face ((t (:inherit default :foreground ,(dv/get-colour 'blue 5) :bold t))))
 `(font-latex-sectioning-4-face ((t (:inherit default :foreground ,(dv/get-colour 'blue 5)))))
 `(font-latex-sectioning-5-face ((t (:inherit default :foreground ,(dv/get-colour 'blue 5)))))

 ; `(company-tooltip	     ((t (:foreground nil :background ,(dv/get-colour 'dark 3)))))
 ; `(company-tooltip-selection ((t (:foreground nil :background ,(dv/get-colour 'dark 5)))))
 ; `(company-tooltip-common    ((t (:foreground ,(dv/get-colour 'mid 1)))))
 ; `(company-tooltip-search    ((t (:underline t)))) ;; Does not work??

 ; `(diff-hl-insert ((t (:foreground ,(dv/get-colour 'green  2) :background ,(dv/get-colour 'green  2)))))
 ; `(diff-hl-change ((t (:foreground ,(dv/get-colour 'orange 4) :background ,(dv/get-colour 'orange 4)))))
 ; `(diff-hl-delete ((t (:foreground ,(dv/get-colour 'red    1) :background ,(dv/get-colour 'red    1)))))

 ; `(transient-key             ((t (:foreground ,(dv/get-colour 'orange 4) :bold t))))
 ; `(transient-argument          ((t (:inherit transient-key :bold nil))))
 ; `(transient-inactive-argument ((t (:foreground ,(dv/get-colour 'mid 2)))))
 ; `(transient-value             ((t (:inherit transient-key :bold nil))))
 ; `(transient-inactive-value    ((t (:foreground ,(dv/get-colour 'mid 2)))))
 ; `(transient-disabled-suffix   ((t (:foreground "#000000" :background ,(dv/get-colour 'red   1)))))
 ; `(transient-enabled-suffix    ((t (:foreground "#000000" :background ,(dv/get-colour 'green 1)))))

 ; `(magit-hash ((t (:foreground ,(dv/get-colour 'mid 1)))))
 ; `(magit-process-ok ((t (:foreground ,(dv/get-colour 'green 1)))))
 ; `(magit-process-ng ((t (:foreground ,(dv/get-colour 'red   1)))))
 ; `(magit-branch-local   ((t (:foreground ,(dv/get-colour 'blue 2) :bold t))))
 ; `(magit-branch-remote  ((t (:foreground ,(dv/get-colour 'blue 5)))))
 ; `(magit-diff-added             ((t (:foreground ,(dv/get-colour 'green  1) :background "#256625"))))
 ; `(magit-diff-added-highlight   ((t (:inherit magit-diff-added))))
 ; `(magit-diff-removed           ((t (:foreground ,(dv/get-colour 'red    3) :background "#a50300"))))
 ; `(magit-diff-removed-highlight ((t (:inherit magit-diff-removed))))
 ; `(magit-diff-base              ((t (:foreground ,(dv/get-colour 'yellow 3) :background "#b19400"))))
 ; `(magit-diff-base-highlight    ((t (:inherit magit-diff-base))))
 ; `(magit-diff-context           ((t (:inherit default))))
 ; `(magit-diff-context-highlight ((t (:inherit shadow))))
 ; `(magit-diff-hunk-heading           ((t (:background ,(dv/get-colour 'dark 5) :italic t))))
 ; `(magit-diff-hunk-heading-highlight ((t (:background ,(dv/get-colour 'mid  1) :italic t))))
 ; `(magit-section-highlight   ((t (:inherit shadow))))
 ; `(magit-section-heading     ((t (:foreground ,(dv/get-colour 'yellow 2) :bold t))))
 ; `(magit-section-child-count ((t (:foreground ,(dv/get-colour 'yellow 2) :italic t))))
 )

(set-face-attribute 'mode-line nil
		    :weight 'bold
		    :foreground (dv/get-colour 'light 5)
		    :background (dv/get-colour 'dark 4)
		    :overline nil
		    :underline nil
		    :box nil
		    ; :box (list :line-width 8 :color (dv/get-colour 'dark 2))
		    :inherit nil)
; (set-face-attribute 'mode-line-inactive nil
; 		    :weight 'bold
; 		    :height 32
; 		    :overline nil
; 		    :underline nil
; 		    :box nil
; 		    :inherit 'shadow)
; (set-face-attribute 'header-line nil
; 		    :weight 'bold
; 		    :foreground (dv/get-colour 'light 5)
; 		    :background (dv/get-colour 'dark  4)
; 		    :overline nil
; 		    :underline nil
; 		    :box (list :line-width 8 :color (dv/get-colour 'dark 2))
; 		    :inherit nil)

(provide-theme 'dv)
; (provide 'dv-theme)
