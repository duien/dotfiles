(deftheme user-spacemacs-light
  "Customizations to the light version of spacemacs")

(custom-theme-set-faces
 'user-spacemacs-light
 ;; tab bar
 '(tab-bar ((t :inherit tab-bar-tab-inactive
               :foreground unspecified
               :background "#e3dedd"                 ; bg3
               :box nil)))
 '(tab-bar-tab ((t :box unspecified :inherit default)))

 ;; don't add a background to block begin/end
 ;; TODO get other attributes from default
 '(org-block-begin-line ((t :background "#fbf8ef"))) ; bg1
 '(org-block-end-line ((t :background "#fbf8ef")))   ; bg1

 ;; org keyword faces
 ;; TODO these need to be fixed up some -- inheritance is funky
 '(org-todo ((t :inverse-video t :weight bold)))
 '(eh/org-keyword-todo ((t :foreground "#42ae2c"     ; suc
                           :background "#edf2e9"     ; green-bg
                           ;; :inverse-video t
                           :inherit org-todo)))
 '(eh/org-keyword-idea ((t :foreground "#9380b2"     ; cblk-ln
                           :background "#efeae9"     ; bg-alt
                           ;; :inverse-video t
                           :inherit org-todo)))
 '(eh/org-keyword-question ((t :foreground "#3a81c3" ; blue / keyword / head1
                               :background "#edf1ed" ; blue-bg
                               ;; :inverse-video t
                               :inherit org-todo)))
 '(eh/org-keyword-read ((t :foreground "#b1951d"     ; yellow
                           :background "#f6f1e1"     ; yellow-bg
                           ;; :inverse-video t
                           :inherit org-todo)))
 '(eh/org-keyword-next ((t :foreground "#dc752f"     ; war
                           :background "#faede4"     ; red-bg
                           ;; :inverse-video t
                           :inherit org-todo)))
 '(eh/org-keyword-halt ((t :foreground "#e0211d"     ; err
                           :background "#eed9d2"     ; red-bg-s
                           ;; :inverse-video t
                           :inherit org-todo)))
 '(eh/org-keyword-bury ((t :foreground "#a49da5"     ; comment-light
                           :background "#efeae9"     ; bg-2
                           :weight reset
                           :inverse-video nil
                           :inherit org-todo)))
 '(eh/org-keyword-done ((t :inverse-video nil
                           :weight reset
                           :inherit eh/org-keyword-todo)))
 '(eh/org-keyword-kill ((t :inverse-video nil
                           :weight reset
                           :inherit eh/org-keyword-halt
                           :background "#faede4")))  ; red-bg
 '(eh/org-keyword-answer ((t :inverse-video nil
                             :weight reset
                             :inherit eh/org-keyword-question)))
 '(eh/org-keyword-yes ((t :inverse-video nil
                          :weight reset
                          :inherit eh/org-keyword-done
                          :background "#dae9d0")))   ; green-bg-s
 '(eh/org-keyword-no ((t :inverse-video nil
                         :weight reset
                         :inherit eh/org-keyword-kill
                         :background "#eed8d2")))    ; red-bg-s
 '(eh/org-keyword-meh ((t :inverse-video nil
                          :weight reset
                          :inherit eh/org-keyword-bury)))
 '(eh/org-keyword-rode ((t :inverse-video nil
                           :weight reset
                           :inherit eh/org-keyword-read)))
 '(org-headline-done ((t :inherit font-lock-comment-face
                         :foreground unspecified
                         :weight reset)))
 '(org-headline-todo ((t :foreground unspecified
                         :background unspecified
                         :weight reset)))
 `(org-ellipsis ((t :weight ,(face-attribute 'default :weight) :slant normal)))
 '(org-superstar-header-bullet ((t :weight reset)))
 '(org-superstar-leading ((t :weight reset
                             :foreground "#e3dedd"))); bg-3
 '(org-hide ((t :foreground "#fbf8ef"                ; bg-1
                :distant-foreground "#fbf8ef")))     ; bg-1
 ;; '(nano-modeline-active-status-RW ((t :background nil)))
 ;; '(nano-modeline-active-status-** ((t :background nil)))
 ;; '(nano-modeline-active-status-RO ((t :background nil)))
 '(fill-column-indicator ((t :foreground "#efeae9"   ; bg-2
                             :background "#efeae9"))); bg-2
 '(mode-line-inactive ((t :background "#e8e3f0"      ; cblk-bg
                          :box nil)))
 '(mode-line ((t :background "#9380b2"               ; cblk-ln
                 :foreground "#e8e3f0" :box nil)))   ; cblk-bg
 ;; '(mode-line-buffer-id ((t :foreground "#4e3163"))))
 '(mode-line-buffer-id ((t :foreground "#e8e3f0"     ; cblk-bg
                           :weight bold)))
 '(highlight-indent-guides-odd-face ((t :background "#fbf8ef")))
 '(highlight-indent-guides-even-face ((t :background "#fbf8ef")))
 '(highlight-indent-guides-top-odd-face ((t :background "#efeae9")))
 '(highlight-indent-guides-top-even-face ((t :background "#efeae9")))
 '(highlight-indent-guides-character-face ((t :foreground "#fbf8ef")))
 '(highlight-indent-guides-top-character-face ((t :foreground "#efeae9")))
 )
;; This is not something that a theme should be setting (and it's not clear to
;; me why it ends up sticking around after the theme is deactivated again)
(custom-theme-set-variables
 'user-spacemacs-light
 '(org-fontify-done-headline t)
 '(org-fontify-todo-headline t))

(provide-theme 'user-spacemacs-light)
