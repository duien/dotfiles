(require 'autothemer)

(autothemer-deftheme
 floraverse "A theme with colors from Floraverse"
 ((((class color) (min-colors #xFFFFFF)))

  (black                      "#0e0d15") ;; ansi-background
  (deep                       "#08002e") ;; ansi-black
  (dark                       "#12133E")
  ;; (dark                       "#07053B")

  (violet                     "#302387")
  (blue-violet                "#3342A1")
  (blue                       "#1d6da1") ;; ansi-blue
  (blue-light                 "#40a4cf") ;; ansi-brblue

  (purple                     "#844DBF")
  (purple-muted               "#4A346C")
  (purple-dark                "#452A90")

  (magenta-dark               "#331e4d") ;; ansi-brblack
  (magenta-purple             "#64097D")
  (magenta-red                "#891E73")
  (magenta                    "#B237BD")
  (magenta-light              "#CF65AC")
  (pink-light                 "#f12aae") ;; ansi-brmagenta
  (pink                       "#b7077e") ;; ansi-magenta
  ;; (pink-dark                  "#64002c") ;; ansi-red
  (pink-dark                  "#880B3E")
  (red                        "#d02063") ;; ansi-brred

  (green                      "#5d731a") ;; ansi-green
  (green-light                "#b4ce59") ;; ansi-brgreen
  (yellow                     "#fac357") ;; ansi-bryellow
  (orange                     "#cd751c") ;; ansi-yellow

  (cosmic-green               "#B3FDD1") ; cosmic spectrum green
  (cyan-muted                 "#87D2AA")
  (cyan                       "#62caa8") ;; ansi-brcyan
  (cyan-dark                  "#42a38c") ;; ansi-cyan

  (chalk-brown                "#909F8E")
  (chalk-green                "#8CBB8B")
  (chalk-green-light          "#B3F99C")
  (chalk-green-beige          "#E5FBCD")
  (cosmic-latte               "#FFF8E7") ; cosmic latte
  (chalk-beige                "#fff5db") ;; ansi-brwhite
  (chalk-beige-dark           "#f3e0b8") ;; ansi-white
  (chalk-gray-beige           "#dbd1b9") ;; ansi-foreground
  (chalk-gray                 "#bbbbbb")
  (chalk-blue                 "#7997AA")
  (chalk-violet               "#9295C9")
  (chalk-purple               "#957EA8")
  (chalk-magenta              "#B586A7")
  (chalk-pink                 "#C696A9")
  (chalk-orange               "#C6A6A2")
  (chalk-yellow               "#E0CFBE"))

 (
  (default (:background deep :foreground chalk-gray-beige))
  (bold (:weight 'bold))
  (italic (:slant 'italic))

  ;;; Editor UI Elements

  ;; mode line
  (mode-line (:background cyan-dark :foreground violet))
  (mode-line-inactive (:background violet :foreground chalk-violet))
  ;; (mode-line-highlight (:inverse-video t))
  ;; (header-line (:background darker-gray))
  ;; tab-bar
  (tab-bar (:background violet :foreground chalk-violet))
  (tab-bar-tab (:background deep))
  (tab-bar-tab-inactive (:inherit 'tab-tar))
  ;; line numbers
  (line-number (:foreground purple-muted))
  (line-number-minor-tick (:foreground purple))
  (line-number-major-tick (:foreground magenta))
  (line-number-current-line (:foreground purple :background magenta-dark :inherit 'bold))
  ;; dividers
  (fringe (:inherit 'default))
  (vertical-border (:foreground cyan-dark))
  ;; (internal-border (:background violet))
  (fill-column-indicator (:foreground purple-muted))

  ;;; Font Lock Faces
  (font-lock-comment-face (:foreground purple :slant 'italic))
  (font-lock-comment-delimiter-face (:foreground purple-muted :inherit 'font-lock-comment-face))
  ;; (font-lock-string-face (:foreground chalk-green :slant 'italic))
  ;; (font-lock-keyword-face (:foreground chalk-magenta))
  ;; (font-lock-builtin-face (:foreground chalk-violet))
  ;; (font-lock-constant-face (:foreground chalk-gray))
  ;; (font-lock-function-name-face (:foreground chalk-pink))
  ;; (font-lock-variable-name-face (:foreground chalk-blue))
  ;; (font-lock-type-face (:foreground chalk-orange))
  (font-lock-string-face (:foreground green-light :slant 'italic))
  (font-lock-keyword-face (:foreground magenta))
  (font-lock-builtin-face (:foreground yellow))
  (font-lock-constant-face (:foreground blue-light))
  (font-lock-function-name-face (:foreground pink-light))
  (font-lock-variable-name-face (:foreground cyan))
  (font-lock-type-face (:foreground orange))

  ;; ;;; Basic Faces
  (highlight (:background magenta-purple))
  (hl-line (:background magenta-dark))
  (shadow (:foreground purple-muted))
  (link (:foreground blue-light :underline t))
  (link-visited (:foreground magenta :underline t))

  ;; ;;; Package-specific faces

  ;; ;; Diff markers in fringe
  (diff-hl-change (:background blue :foreground blue))
  (diff-hl-insert (:background green :foreground green))
  (diff-hl-delete (:background pink :foreground pink))

  ;; ;; Vue/MMM
  ;; (mmm-default-submode-face (:background 'unspecified))

  ;; ;; Nano Modeline
  ;; (nano-modeline-active-status-** (:background yellow :inherit 'nano-modeline-active))
  ;; (nano-modeline-active-status-RO (:background crimson :inherit 'nano-modeline-active))
  ;; (nano-modeline-active-status-RW (:background dark-cyan :inherit 'nano-modeline-active))

  ;; ;; Org-Mode
  (org-document-title (:foreground cosmic-green :height 1.25))
  (org-level-1 (:foreground chalk-green-beige :inherit 'bold :height 1.125))
  (org-level-2 (:foreground chalk-green :inherit 'bold))
  (org-level-3 (:foreground chalk-pink :inherit 'org-level-2))
  (org-level-4 (:foreground chalk-blue :inherit 'org-level-2))
  (org-level-5 (:foreground chalk-magenta :inherit 'org-level-2))
  (org-level-6 (:foreground chalk-violet :inherit 'org-level-2))
  (org-level-7 (:foreground chalk-orange :inherit 'org-level-2))
  (org-level-8 (:foreground chalk-brown :inherit 'org-level-2))
  (org-block (:background dark))
  (org-hide (:foreground deep))
  ;; (org-block-begin-line (:foreground dark-gray))
  ;; (org-block-end-line (:inherit 'org-block-begin-line))
  (org-ellipsis (:foreground purple :background deep :underline nil :weight 'normal)) ;; TODO weight
  (org-headline-todo (:weight 'reset :slant 'reset :foreground chalk-beige))
  (org-headline-done (:inherit '(italic) :weight 'reset :foreground blue-violet))
  (org-todo (:foreground cosmic-green :background cyan-dark :inherit 'bold))
  (org-done (:foreground purple-muted :background dark :weight 'reset))
  (org-superstar-leading (:foreground purple-muted))
  (org-superstar-header-bullet (:weight 'reset))
  (org-agenda-done (:inherit 'org-headline-done))
  (org-scheduled-today (:inherit '(bold org-headline-todo)))
  (org-code (:foreground blue-light))
  (org-verbatim (:foreground cyan))

  ;; TODO Move these out to user customizations
  (eh/org-keyword-todo (:foreground green-light :background green :inherit 'org-todo))
  (eh/org-keyword-idea (:foreground magenta  :background magenta-purple :inherit 'org-todo))
  (eh/org-keyword-question (:foreground blue-light :background blue :inherit 'org-todo))
  (eh/org-keyword-read (:foreground purple :background purple-dark :inherit 'org-todo))
  (eh/org-keyword-next (:foreground orange :background yellow :inherit 'org-todo))
  (eh/org-keyword-halt (:foreground pink-dark :background red :inherit 'org-todo))
  (eh/org-keyword-bury (:foreground purple :inherit 'org-done))
  (eh/org-keyword-done (:foreground green :inherit 'org-done))
  (eh/org-keyword-answer (:foreground blue-light :inherit 'org-done))
  (eh/org-keyword-kill (:foreground pink-dark :inherit 'org-done))
  (eh/org-keyword-yes (:foreground green-light :inherit 'org-done))
  (eh/org-keyword-no (:foreground red :inherit 'org-done))
  (eh/org-keyword-meh (:foreground blue :inherit 'org-done))
  (eh/org-keyword-rode (:foreground purple :inherit 'org-done))

  ;; Terminal
  (ansi-color-red (:foreground pink-dark :background pink-dark))
  (ansi-color-blue (:foreground blue :background blue))
  (ansi-color-cyan (:foreground cyan-dark :background cyan-dark))
  (ansi-color-black (:foreground deep :background deep))
  (ansi-color-green (:foreground green :background green))
  (ansi-color-white (:foreground chalk-beige-dark :background chalk-beige-dark))
  (ansi-color-yellow (:foreground orange :background orange))
  (ansi-color-magenta (:foreground pink :background pink))

  (ansi-color-bright-red (:foreground red :background red))
  (ansi-color-bright-blue (:foreground blue-light :background blue-light))
  (ansi-color-bright-cyan (:foreground cyan :background cyan))
  (ansi-color-bright-black (:foreground magenta-dark :background magenta-dark))
  (ansi-color-bright-green (:foreground green-light :background green-light))
  (ansi-color-bright-white (:foreground chalk-beige :background chalk-beige))
  (ansi-color-bright-yellow (:foreground yellow :background yellow))
  (ansi-color-bright-magenta (:foreground pink-light :background pink-light))

  (vterm-color-black   (:foreground deep             :background magenta-dark))
  (vterm-color-red     (:foreground pink-dark        :background red))
  (vterm-color-green   (:foreground green            :background green-light))
  (vterm-color-yellow  (:foreground orange           :background yellow))
  (vterm-color-blue    (:foreground blue             :background blue-light))
  (vterm-color-magenta (:foreground pink             :background pink-light))
  (vterm-color-cyan    (:foreground cyan-dark        :background cyan))
  (vterm-color-white   (:foreground chalk-beige-dark :background chalk-beige))

  ;; ;; Magit
  ;; ;; (magit-blame-date nil)
  ;; ;; (magit-blame-name nil)
  ;; ;; (magit-blame-hash nil)
  ;; ;; (magit-blame-summary nil)
  ;; ;; (magit-blame-heading (:weight 'normal :slant 'normal :extend t :inherit 'magit-blame-highlight))
  ;; ;; (magit-blame-dimmed (:weight 'normal :slant 'normal :inherit 'magit-dimmed))
  ;; ;; (magit-blame-margin (:weight 'normal :slant 'normal :inherit 'magit-blame-highlight))
  ;; ;; (magit-blame-highlight (:extend t :foreground white :background deepest))
  ;; ;; (magit-reflog-other (:foreground cyan))
  ;; ;; (magit-reflog-remote (:foreground cyan))
  ;; ;; (magit-reflog-cherry-pick (:foreground green))
  ;; ;; (magit-reflog-rebase (:foreground magenta))
  ;; ;; (magit-reflog-reset (:foreground scarlet))
  ;; ;; (magit-reflog-checkout (:foreground dark-blue))
  ;; ;; (magit-reflog-merge (:foreground green))
  ;; ;; (magit-reflog-amend (:foreground magenta))
  ;; ;; (magit-reflog-commit (:foreground green))
  ;; ;; (magit-bisect-bad (:foreground crimson))
  ;; ;; (magit-bisect-skip (:foreground orange))
  ;; ;; (magit-bisect-good (:foreground dark-green))
  ;; ;; (magit-sequence-exec (:inherit 'magit-hash))
  ;; ;; (magit-sequence-onto (:inherit 'magit-sequence-done))
  ;; ;; (magit-sequence-done (:inherit 'magit-hash))
  ;; ;; (magit-sequence-drop (:foreground crimson))
  ;; ;; (magit-sequence-head (:foreground cyan))
  ;; ;; (magit-sequence-part (:foreground yellow))
  ;; ;; (magit-sequence-stop (:foreground lighter-gray))
  ;; ;; (magit-sequence-pick (:inherit 'default))
  ;; ;; (magit-filename (:weight 'normal))
  ;; ;; (magit-cherry-equivalent (:foreground magenta))
  ;; ;; (magit-cherry-unmatched (:foreground cyan))
  ;; ;; (magit-signature-error (:foreground cyan))
  ;; ;; (magit-signature-revoked (:foreground purple))
  ;; ;; (magit-signature-expired-key (:inherit 'magit-signature-expired))
  ;; ;; (magit-signature-expired (:foreground orange))
  ;; ;; (magit-signature-untrusted (:foreground light-gray))
  ;; ;; (magit-signature-bad (:weight 'bold :foreground scarlet))
  ;; ;; (magit-signature-good (:foreground green))
  ;; ;; (magit-keyword-squash (:inherit 'font-lock-warning-face))
  ;; ;; (magit-keyword (:inherit 'font-lock-string-face))
  ;; ;; (magit-refname-pullreq (:inherit 'magit-refname))
  ;; ;; (magit-refname-wip (:inherit 'magit-refname))
  ;; ;; (magit-refname-stash (:inherit 'magit-refname))
  ;; ;; (magit-refname (:foreground lighter-gray))
  ;; (magit-head (:inherit 'magit-branch-current :foreground magenta))
  ;; ;; (magit-branch-warning (:inherit 'warning))
  ;; ;; (magit-branch-upstream (:slant 'italic))
  ;; (magit-branch-current (:inverse-video t :inherit '(magit-branch-local bold)))
  ;; (magit-branch-local (:foreground blue))
  ;; (magit-branch-remote-head (:inverse-video t :inherit '(magit-branch-remote bold)))
  ;; (magit-branch-remote (:foreground brown))
  ;; (magit-tag (:foreground yellow))
  ;; (magit-hash (:foreground gray))
  ;; ;; (magit-dimmed (:foreground gray))
  ;; ;; (magit-header-line-key (:inherit 'font-lock-builtin-face))
  ;; ;; (magit-header-line (:inherit 'magit-section-heading))
  ;; ;; (magit-header-line-log-select (:inherit 'bold))
  ;; ;; (magit-log-date (:weight 'normal :slant 'normal :foreground lighter-gray))
  ;; ;; (magit-log-author (:weight 'normal :slant 'normal :foreground persimmon))
  ;; ;; (magit-log-graph (:foreground lighter-gray))
  ;; ;; (magit-diffstat-removed (:foreground crimson))
  ;; ;; (magit-diffstat-added (:foreground dark-green))
  ;; ;; (magit-diff-whitespace-warning (:inherit 'trailing-whitespace))
  ;; (magit-diff-context-highlight (:extend t :foreground lighter-gray :background darker-gray))
  ;; ;; (magit-diff-their-highlight (:inherit 'magit-diff-added-highlight))
  ;; ;; (magit-diff-base-highlight (:extend t :foreground yellow :background brown))
  ;; ;; (magit-diff-our-highlight (:inherit 'magit-diff-removed-highlight))
  ;; (magit-diff-removed-highlight (:extend t :foreground white :background crimson))
  ;; (magit-diff-added-highlight (:extend t :foreground white :background dark-green))
  ;; ;; (magit-diff-context (:extend t :foreground lighter-gray))
  ;; ;; (magit-diff-their (:inherit 'magit-diff-added))
  ;; ;; (magit-diff-base (:extend t :foreground white :background brown))
  ;; ;; (magit-diff-our (:inherit 'magit-diff-removed))
  ;; (magit-diff-removed (:extend t :foreground crimson))
  ;; (magit-diff-added (:extend t :foreground dark-green))
  ;; ;; (magit-diff-conflict-heading (:inherit 'magit-diff-hunk-heading))
  ;; (magit-diff-lines-boundary (:extend t :background gray))
  ;; (magit-diff-lines-heading (:extend t :background blue :inherit 'magit-diff-hunk-heading-highlight))
  ;; ;; (magit-diff-revision-summary-highlight (:inherit 'magit-diff-hunk-heading-highlight))
  ;; ;; (magit-diff-revision-summary (:inherit 'magit-diff-hunk-heading))
  ;; ;; (magit-diff-hunk-region (:extend t :inherit 'bold))
  ;; ;; (magit-diff-hunk-heading-selection (:extend t :foreground persimmon :inherit 'magit-diff-hunk-heading-highlight))
  ;; (magit-diff-hunk-heading-highlight (:extend t :foreground white :background gray))
  ;; (magit-diff-hunk-heading (:extend t :foreground white :background dark-gray))
  ;; ;; (magit-diff-file-heading-selection (:extend t :background darker-gray :inherit 'magit-diff-file-heading-highlight))
  ;; ;; (magit-diff-file-heading-highlight (:extend t :inherit 'magit-section-highlight))
  ;; ;; (magit-diff-file-heading (:weight 'bold :extend t))
  ;; ;; (magit-mode-line-process-error (:inherit 'error))
  ;; ;; (magit-mode-line-process (:inherit 'mode-line-emphasis))
  ;; ;; (magit-process-ng (:foreground scarlet :inherit 'magit-section-heading))
  ;; ;; (magit-process-ok (:foreground green :inherit 'magit-section-heading))
  ;; (magit-section-child-count (:foreground gray))
  ;; ;; (magit-section-heading-selection (:extend t :foreground persimmon))
  ;; ;; (magit-section-secondary-heading (:weight 'bold :extend t))
  ;; (magit-section-heading (:weight 'bold :extend t :foreground yellow))
  ;; (magit-section-highlight (:extend t :background darker-gray))

  ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'floraverse)


;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda () (load-theme 'floraverse t)))
;; End:
