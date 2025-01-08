(deftheme user-modus-operandi-tinted
  "Created 2023-05-19. Additions to Modus Operandi's tinted variant
for my personal preferences and org keywords")

(modus-themes-with-colors
  (custom-theme-set-faces
   'user-modus-operandi-tinted

   `(fill-column-indicator ((t :height 1 :foreground ,bg-dim :background ,bg-dim)))
   `(font-lock-string-face ((t :inherit italic)))
   `(org-block ((t :foreground ,fg-main :background ,bg-dim :inherit modus-themes-fixed-pitch)))

   `(org-headline-todo ((t :inherit modus-themes- :weight reset)))
   `(org-headline-done ((t :inherit font-lock-comment-face :weight reset)))
   `(org-checkbox ((t :foreground ,yellow-warmer :background ,bg-dim)))
   `(org-checkbox-statistics-todo ((t :foreground ,rust :background ,bg-dim :weight bold :inherit modus-themes-fixed-pitch)))
   `(org-checkbox-statistics-done ((t :foreground ,border :background ,bg-dim :weight reset :slant italic :inherit modus-themes-fixed-pitch)))

   `(org-ellipsis ((t :inherit (shadow default))))
   `(org-superstar-header-bullet ((t :weight reset :inherit modus-themes-fixed-pitch)))
   `(org-superstar-leading ((t :foreground ,bg-inactive :distant-foreground ,bg-inactive :inherit modus-themes-fixed-pitch)))
   `(org-hide ((t :foreground ,bg-main :distant-foreground ,bg-main :inherit modus-themes-fixed-pitch)))
   ;; `(org-document-info-)

   `(org-todo ((t :weight bold :background ,rust :foreground ,bg-main :inherit modus-themes-fixed-pitch)))

   `(org-done ((t :weight reset :background ,bg-dim :foreground ,rust :inherit modus-themes-fixed-pitch)))
  ))

(provide-theme 'user-modus-operandi-tinted)
