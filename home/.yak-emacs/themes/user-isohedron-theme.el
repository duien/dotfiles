(deftheme user-isohedron
  "Customizations to my custom theme")
(custom-theme-set-faces
   'user-isohedron
   ;; faces for custom telephone-based modeline
   '(eh/telephone-line-status-locked
     ((t :background "#FB6C6A")))
   '(eh/telephone-line-status-normal
     ((t :background "#554e6a")))
   '(eh/telephone-line-status-unsaved
     ((t :background "#F0B400")))
   ;; faces for incomplete items
   '(eh/org-keyword-todo
     ((t :background "#84bd00" :foreground "#f7f3ee" :inherit org-todo)))
   '(eh/org-keyword-idea
     ((t :background "#ce5cff" :foreground "#f7f3ee" :inherit org-todo)))
   '(eh/org-keyword-read
     ((t :background "#b9a992" :foreground "#f7f3ee" :inherit org-todo)))
   '(eh/org-keyword-question
     ((t :background "#75a3ff" :foreground "#f7f3ee" :inherit org-todo)))
   '(eh/org-keyword-bury
     ((t :background "#f1ece4" :foreground "#93836c" :inherit org-todo)))
   '(eh/org-keyword-next
     ((t :background "#f0b400" :foreground "#f7f3ee" :inherit org-todo)))
   '(eh/org-keyword-halt
     ((t :background "#f08c00" :foreground "#f7f3ee" :inherit org-todo)))
   ;; faces for complete items
   '(eh/org-keyword-done
     ((t :foreground "#81895d" :inherit org-done)))
   '(eh/org-keyword-kill
     ((t :foreground "#957f5f" :inherit org-done)))
   '(eh/org-keyword-yes
     ((t :background "#e2e9ca" :foreground "#84bd00" :inherit org-done)))
   '(eh/org-keyword-no
     ((t :background "#f6e1ca" :foreground "#fb6c6a" :inherit org-done)))
   '(eh/org-keyword-answer
     ((t :background "#dde3f2" :foreground "#75a3ff" :inherit org-done)))

   ;; make only top indent guide visible
   '(highlight-indent-guides-character-face ((t :foreground "#f7f3ee")))
   '(highlight-indent-guides-top-character-face ((t :foreground "#f1ece4")))
   ;; '(highlight-indent-guides-face ((t :background "#f7f3ee")))
   '(highlight-indent-guides-top-even-face ((t :background "red")))
   ;; '(highlight-indent-guides-top-face ((t :background "#f1ece4")))

   ;; fix font weights in mood-line
   '(mood-line-buffer-name ((t :inherit bold)))
   '(mood-line-buffer-status-modified ((t :background "#fb6c6a" :foreground "#f7f3ee"))) ; iso-ui-red
   '(mood-line-buffer-status-narrowed ((t :background "#df870c" :foreground "#f7f3ee"))) ; iso-ui-orange
   '(mood-line-buffer-status-read-only ((t :background "#fb6c6a" :foreground "#f7f3ee")))
   '(mood-line-unimportant ((t :inherit shadow)))
   '(mood-line-major-mode ((t :inherit bold)))
   '(mood-line-status-error ((t :foreground "#fb6c6a" :weight reset)))
   '(mood-line-status-warning ((t :foreground "#df870c" :weight reset)))
   '(mood-line-status-success ((t :foreground "#8aa60b" :weight reset)))
   )
(provide-theme 'user-isohedron)
