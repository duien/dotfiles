(deftheme user-qud
  "Customizations for my custom theme")
(custom-theme-set-faces
   'user-qud
   ;; faces for incomplete items
   '(eh/org-keyword-todo     ((t :background "#009403" :inherit org-todo)))
   '(eh/org-keyword-idea     ((t :background "#b154cf" :inherit org-todo)))
   '(eh/org-keyword-question ((t :background "#0096ff" :inherit org-todo)))
   '(eh/org-keyword-read     ((t :background "#98875f" :inherit org-todo)))
   '(eh/org-keyword-next     ((t :background "#cfc041" :inherit org-todo)))
   '(eh/org-keyword-halt     ((t :background "#f15f22" :inherit org-todo)))
   ;; faces for complete items
   '(eh/org-keyword-done     ((t :foreground "#009403" :inherit org-done)))
   '(eh/org-keyword-kill     ((t :foreground "#a64a2e" :inherit org-done)))
   '(eh/org-keyword-answer   ((t :foreground "#0096ff" :inherit org-done)))
   '(eh/org-keyword-yes      ((t :foreground "#00c420" :inherit org-done)))
   '(eh/org-keyword-no       ((t :foreground "#d74200" :inherit org-done)))
   '(eh/org-keyword-rode     ((t :foreground "#98875f" :inherit org-done))))

(provide-theme 'user-qud)
