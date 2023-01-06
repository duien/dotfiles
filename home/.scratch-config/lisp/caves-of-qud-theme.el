(autothemer-deftheme
 caves-of-qud "A theme with colors from Caves of Qud"
 ((((class color) (min-colors #xFFFFFF)))

  (crimson "#a64a2e")
  (scarlet "#d74200")
  (persimmon "#f15f22")
  (orange "#e99f10")
  (brown "#98875f")
  (yellow "#cfc041")
  (dark-green "#009703")
  (green "#00c420")
  (dark-blue "#0048bd")
  (blue "#0096ff")
  (dark-cyan "#40a4b9")
  (cyan "#77bfcf")
  (purple "#b154cf")
  (magenta "#da5bd6")
  (deepest "#0D3433")
  (viridian "#0f3b3a")
  (darker-gray "#155352")
  (dark-gray "#3C716E")
  (gray "#638E8B")
  (light-gray "#8AACA7")
  (lighter-gray "#b1c9c3")
  (white "#ffffff"))

 ((default (:background viridian :foreground lighter-gray))
  (bold (:weight 'bold))
  (italic (:slant 'italic))

  ;;; Editor UI Elements

  ;; mode line
  (mode-line (:background dark-gray :foreground viridian))
  (mode-line-inactive (:background deepest :foreground darker-gray))
  (mode-line-highlight (:inverse-video t))
  (header-line (:background darker-gray))
  ;; tab-bar
  (tab-bar (:background deepest :foreground dark-cyan))
  (tab-bar-tab (:foreground light-gray :background viridian :overline scarlet))
  (tab-bar-tab-inactive (:inherit 'tab-tar))
  ;; line numbers
  (line-number (:foreground darker-gray))
  (line-number-minor-tick (:foreground dark-gray))
  (line-number-major-tick (:foreground dark-gray :inherit 'bold))
  (line-number-current-line (:foreground viridian :background darker-gray :inherit 'bold))
  ;; dividers
  (fringe (:inherit 'default))
  (vertical-border (:foreground dark-gray))
  (internal-border (:background deepest))

  ;;; Font Lock Faces
  (font-lock-comment-face (:foreground dark-cyan :slant 'italic))
  (font-lock-comment-delimiter-face (:inherit 'font-lock-comment-face))
  (font-lock-string-face (:foreground green))
  (font-lock-keyword-face (:foreground persimmon))
  (font-lock-builtin-face (:foreground brown))
  (font-lock-constant-face (:foreground yellow))
  (font-lock-function-name-face (:foreground magenta))
  (font-lock-variable-name-face (:foreground blue))
  (font-lock-type-face (:foreground cyan))

  ;;; Basic Faces
  (highlight (:background dark-blue))
  (shadow (:foreground dark-cyan))
  (link (:foreground blue :underline t))
  (link-visited (:foreground purple :underline t))

  ;;; Package-specific faces

  ;; Diff markers in fringe
  (diff-hl-change (:background purple :foreground purple))
  (diff-hl-insert (:background dark-green :foreground dark-green))
  (diff-hl-delete (:background crimson :foreground crimson))

  ;; Vue/MMM
  (mmm-default-submode-face (:background 'unspecified))

  ;; Org-Mode
  (org-document-title (:foreground white :height 1.25))
  (org-level-1 (:foreground white :background darker-gray :extend t :inherit 'bold :height 1.125))
  ;; (org-level-1 (:foreground purple :inherit 'bold :height 1.125))
  (org-level-2 (:foreground white :inherit 'bold))
  (org-level-3 (:inherit 'org-level-2))
  (org-level-4 (:inherit 'org-level-2))
  (org-level-5 (:inherit 'org-level-2))
  (org-level-6 (:inherit 'org-level-2))
  (org-level-7 (:inherit 'org-level-2))
  (org-level-8 (:inherit 'org-level-2))
  (org-block (:background deepest))
  (org-block-begin-line (:foreground dark-gray))
  (org-block-end-line (:inherit 'org-block-begin-line))
  (org-ellipsis (:inherit 'shadow :background 'reset))
  (org-headline-todo (:weight 'reset :slant 'reset))
  (org-headline-done (:inherit '(italic) :weight 'reset :foreground dark-gray))
  (org-todo (:foreground viridian :background dark-gray :inherit 'bold))
  (org-done (:foreground dark-gray :background deepest :weight 'reset))
  (org-superstar-leading (:foreground darker-gray))
  ))

;; (setq header-line-format "header")
