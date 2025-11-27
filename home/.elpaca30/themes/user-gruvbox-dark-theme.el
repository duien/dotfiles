(deftheme user-gruvbox-dark
  "Customizations to the gruvbox-dark-medium theme")

(custom-theme-set-faces
 'user-gruvbox-dark
 '(font-lock-string-face ((t :inherit italic)))
 '(font-lock-comment-face ((t :inherit italic)))

 '(tab-bar ((t :background "#3c3836")))
 '(tab-bar-tab ((t :background "#282828")))
 '(tab-bar-tab-inactive ((t :background "#504945" :foreground "#bdae93")))

 '(org-ellipsis ((t :foreground unspecified :inherit (shadow default))))
 '(org-superstar-leading ((t :foreground "#3c3836")))
 '(org-superstar-header-bullet ((t :foreground "#bdae93" :weight reset)))
 '(org-code ((t :foreground "#d3869b")))
 '(org-verbatim ((t :foreground "#8ec07c")))
 '(org-link ((t :foreground "#458588")))
 '(org-tag ((t :foreground "#fe8019" :background "#3c3836")))

 '(org-headline-todo ((t :foreground "#fdf4c1" :weight reset)))
 '(org-headline-done ((t :foreground unspecified
                         :weight reset
                         :inherit font-lock-comment-face)))

 '(org-todo ((t :weight bold :background "#928374" :foreground "#282828")))
 '(eh/org-keyword-todo ((t :inherit org-todo :background "#98971a")))
 '(eh/org-keyword-idea ((t :inherit org-todo :background "#b16286")))
 '(eh/org-keyword-question ((t :inherit org-todo :background "#458588")))
 '(eh/org-keyword-read ((t :inherit org-todo)))
 '(eh/org-keyword-next ((t :inherit org-todo :background "#d79921")))
 '(eh/org-keyword-halt ((t :inherit org-todo :background "#cc241d")))
 '(eh/org-keyword-bury ((t :inherit org-todo :background "#3c3836" :foreground "#928374")))

 '(org-done ((t :weight reset :background "#3c3836" :foreground "#a89984")))
 '(eh/org-keyword-done ((t :inherit org-done :foreground "#79740e")))
 '(eh/org-keyword-kill ((t :inherit org-done :foreground "#9d0006")))
 '(eh/org-keyword-answer ((t :inherit org-done :foreground "#458588")))
 '(eh/org-keyword-yes ((t :inherit org-done :foreground "#98971a")))
 '(eh/org-keyword-no ((t :inherit org-done :foreground "#cc241d")))
 '(eh/org-keyword-meh ((t :inherit org-done :foreground "#7c6f64")))
 '(eh/org-keyword-rode ((t :inherit org-done :foreground "#a89984")))
 )

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (when (fboundp 'load-theme-if-enabled) (add-hook 'after-save-hook #'load-theme-if-enabled nil t))
;; End:
