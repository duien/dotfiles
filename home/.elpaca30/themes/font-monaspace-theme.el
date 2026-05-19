(deftheme font-monaspace
  "Set up fonts with various Monaspace flavors")

(custom-theme-set-faces
 'font-monaspace
 '(default                ((t :family "Monaspace Neon" :weight light :height 160)))
 '(fixed-pitch            ((t :family "Monaspace Neon")))
 '(font-lock-comment-face ((t :family "Monaspace Radon" :slant normal :weight extralight)))
 '(font-lock-doc-face     ((t :family "Monaspace Radon" :slant normal :weight extralight)))
 ;; NOTE This only works with modus-related themes. I have no idea how to
 ;; customize the weight that emacs selects automatically (or if I even can)
 '(bold                   ((t :family "Monaspace Neon" :weight bold)))

 '(org-headline-todo      ((t :family "Monaspace Radon" :weight extralight)))
 '(org-headline-done      ((t :family "Monaspace Radon" :weight extralight :slant normal)))
 '(org-todo               ((t :family "Monaspace Radon")))
 '(org-done               ((t :family "Monaspace Radon" :slant normal)))
 )

(provide-theme 'font-monaspace)
