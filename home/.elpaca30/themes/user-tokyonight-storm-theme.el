(deftheme user-tokyonight-storm
  "Customizations for TokyoNight's Storm variant")
(custom-theme-set-faces
   'user-tokyonight-storm

   '(line-number ((t :foreground "#3b4261")))
   '(shadow ((t :foreground "#737aa2")))

   '(outline-1 ((t :inherit bold :foreground "#bb9af7" :background "#1f2335")))
   '(outline-2 ((t :inherit bold :foreground "#41a6b5")))
   '(outline-3 ((t :inherit bold :foreground "#73daca")))
   '(outline-4 ((t :inherit bold :foreground "#9ece6a")))
   '(outline-5 ((t :inherit bold :foreground "#9d7cd8")))
   '(outline-6 ((t :inherit bold :foreground "#41a6b5")))
   '(outline-7 ((t :inherit bold :foreground "#73daca")))
   '(outline-8 ((t :inherit bold :foreground "#9ece6a")))

   '(markdown-header-face-1 ((t :inherit outline-1)))
   '(markdown-header-face-2 ((t :inherit outline-2)))
   '(markdown-header-face-3 ((t :inherit outline-3)))
   '(markdown-header-face-4 ((t :inherit outline-4)))
   '(markdown-header-face-5 ((t :inherit outline-5)))
   '(markdown-header-face-6 ((t :inherit outline-6)))
   '(markdown-markup-face ((t :foreground "#545c7e")))

   '(org-ellipsis ((t :weight reset))) ;; TODO not working
   ;; '(org-hide ((t :foreground "#3b4261")))
   '(org-superstar-leading ((t :foreground "#3b4261")))
   '(org-level-1 ((t :inherit outline-1 :foreground unspecified)))
   '(org-level-2 ((t :inherit outline-2 :foreground unspecified)))
   '(org-level-3 ((t :inherit outline-3 :foreground unspecified)))
   '(org-level-4 ((t :inherit outline-4 :foreground unspecified)))
   '(org-level-5 ((t :inherit outline-5 :foreground unspecified)))
   '(org-level-6 ((t :inherit outline-6 :foreground unspecified)))
   '(org-level-7 ((t :inherit outline-7 :foreground unspecified)))
   '(org-level-8 ((t :inherit outline-8 :foreground unspecified)))

   '(org-headline-todo ((t :foreground "#c0caf5" :weight reset :inherit nil)))
   '(org-headline-done ((t :foreground "#565f89" :weight reset :slant italic :inherit nil)))

   ;; faces for incomplete items
   '(org-todo ((t :background "#737aa2" :foreground "#24283b")))
   '(eh/org-keyword-todo ((t :background "#9ece6a" :inherit org-todo)))
   '(eh/org-keyword-idea ((t :background "#9d7cd8" :inherit org-todo)))
   '(eh/org-keyword-question ((t :background "#7aa2f7" :inherit org-todo)))
   '(eh/org-keyword-read     ((t :background "#e0af68" :inherit org-todo)))
   '(eh/org-keyword-next     ((t :background "#f7768e" :inherit org-todo)))
   '(eh/org-keyword-halt     ((t :background "#db4b4b" :inherit org-todo)))
   '(eh/org-keyword-bury     ((t :background "#545c7e" :foreground "#a9b1d6" :weight reset)))

   ;; ;; faces for complete items
   '(org-done ((t :background "#292e42" :foreground "#a9b1d6" :weight reset)))
   '(eh/org-keyword-done     ((t :foreground "#9ece6a" :inherit org-done)))
   '(eh/org-keyword-kill     ((t :foreground "#db4b4b" :inherit org-done)))
   '(eh/org-keyword-answer   ((t :foreground "#7aa2f7" :inherit org-done)))
   '(eh/org-keyword-yes      ((t :foreground "#9ece6a" :inherit org-done)))
   '(eh/org-keyword-no       ((t :foreground "#f7768e" :inherit org-done)))
   '(eh/org-keyword-rode     ((t :foreground "#98875f" :inherit org-done)))
   )

(provide-theme 'user-tokyonight-storm)
