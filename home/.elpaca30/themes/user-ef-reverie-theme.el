(deftheme user-ef-reverie
  "Customizations to any of the ef themes")

(ef-themes-with-colors
  (custom-theme-set-faces
   'user-ef-reverie
   '(mode-line ((t :box nil)))
   '(mode-line-inactive ((t :box nil)))
   '(font-lock-string-face ((t :inherit 'italic)))

   '(org-headline-todo ((t :inherit nil :weight reset)))
   '(org-headline-done ((t :inherit font-lock-comment-face :weight reset)))
   '(org-ellipsis ((t :inherit (shadow default))))
   `(org-superstar-header-bullet ((t :weight reset :inherit ef-themes-fixed-pitch)))
   `(org-superstar-leading ((t :foreground ,bg-inactive :distant-foreground ,bg-inactive :inherit ef-themes-fixed-pitch)))
   `(org-hide ((t :foreground ,bg-main :distant-foreground ,bg-main :inherit ef-themes-fixed-pitch)))

   `(org-todo ((t :weight bold :background ,fg-dim :foreground ,bg-inactive :inherit ef-themes-fixed-pitch)))
   `(eh/org-keyword-todo ((t :inherit org-todo :background ,green :foreground ,bg-main)))
   `(eh/org-keyword-idea ((t :inherit org-todo :background ,magenta :foreground ,bg-main)))
   `(eh/org-keyword-read ((t :inherit org-todo :background ,fg-dim :foreground ,bg-main)))
   `(eh/org-keyword-question ((t :inherit org-todo :background ,blue :foreground ,bg-main)))
   `(eh/org-keyword-next ((t :inherit org-todo :background ,bg-graph-yellow-0 :foreground ,bg-graph-red-0)))
   `(eh/org-keyword-halt ((t :inherit org-todo :background ,bg-graph-red-0 :foreground ,bg-graph-yellow-0)))
   `(eh/org-keyword-bury ((t :inherit org-todo :background ,bg-inactive :foreground ,fg-changed)))

   `(org-done ((t :weight reset :background ,bg-inactive :foreground ,fg-dim :inherit ef-themes-fixed-pitch)))
   `(eh/org-keyword-done ((t :inherit org-done :foreground ,fg-added)))
   `(eh/org-keyword-kill ((t :inherit org-done :foreground ,fg-removed)))
   `(eh/org-keyword-yes ((t :inherit org-done :background ,bg-added-faint :foreground ,green)))
   `(eh/org-keyword-no ((t :inherit org-done :background ,bg-removed-faint :foreground ,red)))
   `(eh/org-keyword-answer ((t :inherit org-done :background ,bg-info :foreground ,blue)))
   ))


(provide-theme 'user-ef-reverie)
