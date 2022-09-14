;; Try not to go crazy while testing
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-control-modifier 'control)

;; Set up font faces
(set-face-attribute 'default nil
		    :font "Menlo" :height 160)

;; And finally, load modus
(load-theme 'modus-operandi)
