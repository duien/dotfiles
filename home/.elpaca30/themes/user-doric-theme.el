(deftheme user-doric
  "Customizations to any of the doric themes")

(custom-theme-set-faces
 'user-doric
 ;; '(font-lock-string-face ((t :inherit 'italic :foreground "#834328")))
 '(font-lock-string-face ((t :inherit 'italic)))
 '(mode-line ((t :box nil)))
 '(mode-line-inactive ((t :box nil)))
 '(font-lock-comment-face ((t :foreground "#8E8570")))
 '(font-lock-comment-delimiter-face ((t :inherit 'font-lock-comment-face :foreground nil)))

 '(eh/org-keyword-todo ((t :inherit 'diff-refine-added :foreground "#605d48")))
 '(eh/org-keyword-read ((t :inherit 'diff-refine-changed :foreground "#605d48")))
 )
