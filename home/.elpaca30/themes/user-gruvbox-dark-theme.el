(deftheme user-gruvbox-dark
  "Customizations to the gruvbox-dark-medium theme")

(custom-theme-set-faces
 'user-gruvbox-dark
 '(font-lock-string-face ((t :inherit italic)))
 '(font-lock-comment-face ((t :inherit italic)))

 '(org-ellipsis ((t :foreground unspecified :inherit (shadow default))))
 '(org-superstar-leading ((t :foreground "#3c3836")))
 '(org-superstar-header-bullet ((t :foreground "#bdae93" :weight reset)))

 ;; '(org-headline-todo ((t :foreground "#ebdbb2")))
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

;; (load-theme 'user-gruvbox-dark)


;;   (gruvbox-dark0_hard      "#1d2021")
;;   (gruvbox-dark0           "#282828")
;;   (gruvbox-dark0_soft      "#32302f")
;;   (gruvbox-dark1           "#3c3836")
;;   (gruvbox-dark2           "#504945")
;;   (gruvbox-dark3           "#665c54")
;;   (gruvbox-dark4           "#7c6f64")

;;   (gruvbox-gray            "#928374")

;;   (gruvbox-light0_hard     "#ffffc8")
;;   (gruvbox-light0          "#fdf4c1")
;;   (gruvbox-light1          "#ebdbb2")
;;   (gruvbox-light2          "#d5c4a1")
;;   (gruvbox-light3          "#bdae93")
;;   (gruvbox-light4          "#a89984")

;;   (gruvbox-bright_red      "#fb4933")
;;   (gruvbox-bright_green    "#b8bb26")
;;   (gruvbox-bright_yellow   "#fabd2f")
;;   (gruvbox-bright_blue     "#83a598")
;;   (gruvbox-bright_purple   "#d3869b")
;;   (gruvbox-bright_aqua     "#8ec07c")
;;   (gruvbox-bright_orange   "#fe8019")

;;   (gruvbox-neutral_red     "#fb4933")
;;   (gruvbox-neutral_green   "#b8bb26")
;;   (gruvbox-neutral_yellow  "#fabd2f")
;;   (gruvbox-neutral_blue    "#83a598")
;;   (gruvbox-neutral_purple  "#d3869b")
;;   (gruvbox-neutral_aqua    "#8ec07c")
;;   (gruvbox-neutral_orange  "#fe8019")

;;   (gruvbox-faded_red       "#cc241d")
;;   (gruvbox-faded_green     "#98971a")
;;   (gruvbox-faded_yellow    "#d79921")
;;   (gruvbox-faded_blue      "#458588")
;;   (gruvbox-faded_purple    "#b16286")
;;   (gruvbox-faded_aqua      "#689d6a")
;;   (gruvbox-faded_orange    "#d65d0e")

;;   (gruvbox-dark_red        "#421e1e")
;;   (gruvbox-dark_blue       "#2b3c44")
;;   (gruvbox-dark_aqua       "#36473a")

;;   (gruvbox-delimiter-one   "#458588") ; faded_blue
;;   (gruvbox-delimiter-two   "#b16286") ; faded_purple
;;   (gruvbox-delimiter-three "#8ec07c") ; neutral_aqua
;;   (gruvbox-delimiter-four  "#d65d0e") ; faded_orange
;;   (gruvbox-white           "#ffffff")
;;   (gruvbox-black           "#000000")
;;   (gruvbox-sienna          "#dd6f48")
;;   (gruvbox-lightblue4      "#66999d")
;;   (gruvbox-burlywood4      "#bbaa97")
;;   (gruvbox-aquamarine4     "#83a598")
;;   (gruvbox-turquoise4      "#61acbb")

;;   (gruvbox-accent-00       "#fb4934") ; neutral_red
;;   (gruvbox-accent-01       "#b8bb26") ; neutral_green
;;   (gruvbox-accent-02       "#fabd2f") ; neutral_yellow
;;   (gruvbox-accent-03       "#83a598") ; neutral_blue
;;   (gruvbox-accent-04       "#d3869b") ; neutral_purple
;;   (gruvbox-accent-05       "#8ec07c") ; neutral_aqua
;;   (gruvbox-accent-06       "#fe8019") ; neutral_orange
;;   (gruvbox-accent-07       "#fb4934")
;;   (gruvbox-accent-08       "#b8bb26")
;;   (gruvbox-accent-09       "#fabd2f")
;;   (gruvbox-accent-10       "#83a598")
;;   (gruvbox-accent-11       "#d3869b")
;;   (gruvbox-accent-12       "#8ec07c")
;;   (gruvbox-accent-13       "#fe8019")
;;   (gruvbox-accent-14       "#fb4934")
;;   (gruvbox-accent-15       "#b8bb26")

;;   (gruvbox-ediff-current-diff-A        "#4f2121")
;;   (gruvbox-ediff-current-diff-B        "#243c24")
;;   (gruvbox-ediff-current-diff-C        "#4f214f")
;;   (gruvbox-ediff-current-diff-Ancestor "#21214f")
;;   (gruvbox-ediff-fine-diff-A           "#761919")
;;   (gruvbox-ediff-fine-diff-B           "#1c691c")
;;   (gruvbox-ediff-fine-diff-C           "#761976")
;;   (gruvbox-ediff-fine-diff-Ancestor    "#12129d")

;;   (gruvbox-bg gruvbox-dark0)
;;   (gruvbox-bg_inactive gruvbox-dark0_soft)
