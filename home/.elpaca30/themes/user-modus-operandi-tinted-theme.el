(deftheme user-modus-operandi-tinted
  "Created 2023-05-19. Additions to Modus Operandi's tinted variant
for my personal preferences and org keywords")

(modus-themes-with-colors
  (custom-theme-set-faces
   'user-modus-operandi-tinted

   `(fill-column-indicator ((t :height 1 :foreground ,bg-dim :background ,bg-dim)))
   `(font-lock-string-face ((t :inherit italic)))

   `(org-headline-todo ((t :weight reset :foreground reset)))
   `(org-headline-done ((t :inherit font-lock-comment-face :weight reset :foreground nil)))
   `(org-checkbox ((t :foreground ,yellow-warmer :background ,bg-dim)))
   `(org-checkbox-statistics-todo ((t :foreground ,rust :background ,bg-dim :weight bold :inherit modus-themes-fixed-pitch)))
   `(org-checkbox-statistics-done ((t :foreground ,border :background ,bg-dim :weight reset :slant italic :inherit modus-themes-fixed-pitch)))

   `(org-block ((t :foreground ,fg-main :background ,bg-dim :inherit modus-themes-fixed-pitch)))
   `(org-ellipsis ((t :inherit (shadow default))))
   `(org-superstar-header-bullet ((t :weight reset :inherit modus-themes-fixed-pitch)))
   `(org-superstar-leading ((t :foreground ,bg-inactive :distant-foreground ,bg-inactive :inherit modus-themes-fixed-pitch)))
   `(org-hide ((t :foreground ,bg-main :distant-foreground ,bg-main :inherit modus-themes-fixed-pitch)))
   ;; `(org-document-info-)

   `(org-todo ((t :weight bold :background ,rust :foreground ,bg-main :inherit modus-themes-fixed-pitch)))
   `(eh/org-keyword-todo ((t :inherit org-todo :background ,olive)))
   `(eh/org-keyword-idea ((t :inherit org-todo :background ,maroon)))
   `(eh/org-keyword-question ((t :inherit org-todo :background ,slate)))
   `(eh/org-keyword-read ((t :inherit org-todo :background ,gold)))
   `(eh/org-keyword-next ((t :inherit org-todo :background ,rust)))
   `(eh/org-keyword-halt ((t :inherit org-todo :background ,red-intense)))
   `(eh/org-keyword-bury ((t :inherit org-todo :background ,bg-dim :foreground ,border)))

   `(org-done ((t :weight reset :background ,bg-dim :foreground ,rust :inherit modus-themes-fixed-pitch)))
   `(eh/org-keyword-done ((t :inherit org-done :foreground ,olive)))
   `(eh/org-keyword-kill ((t :inherit org-done :foreground ,rust)))
   `(eh/org-keyword-answer ((t :inherit org-done :foreground ,slate)))
   `(eh/org-keyword-yes ((t :inherit eh/org-keyword-done)))
   `(eh/org-keyword-no ((t :inherit eh/org-keyword-kill)))
   `(eh/org-keyword-meh ((t :inherit org-done :foreground ,border)))
   `(eh/org-keyword-rode ((t :inherit org-done :foreground ,gold)))
  ))

(provide-theme 'user-modus-operandi-tinted)
