;; A couple basic modus settings
(setq modus-themes-mixed-fonts nil)
(setq modus-themes-italic-constructs t)

;; And a couple basic org settings
(setq org-fontify-todo-headline t)
(setq org-fontify-done-headline t)

;; Try not to go crazy while testing
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-control-modifier 'control)

;; Set up font faces
(set-face-attribute 'default nil
		    :font "Rec Mono Duotone" :height 160)
(set-face-attribute 'fixed-pitch nil
		    :family (face-attribute 'default :family))

;; Override faces for Org
;; Do this in a function that can be added to the theme
;; load hook to make testing less painful
(defun eh/modus-org-faces ()
  (set-face-attribute 'org-headline-todo nil
		      :weight 'normal :inherit 'default)
  (set-face-attribute 'org-headline-done nil
		      :weight 'normal :slant 'italic)
  )
(add-hook 'modus-themes-after-load-theme-hook #'eh/modus-org-faces)
(add-hook 'org-mode-hook #'eh/modus-org-faces) ;; hacky

;; And finally, load modus
(load-theme 'modus-operandi)


