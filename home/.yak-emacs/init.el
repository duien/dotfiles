;; init.el -*- lexical-binding: t; -*- no-byte-compile: t -*-
;;
;;
;;                      ▓██   ██▓ ▄▄▄       ██ ▄█▀
;;                       ▒██  ██▒▒████▄     ██▄█▒
;;                        ▒██ ██░▒██  ▀█▄  ▓███▄░
;;                        ░ ▐██▓░░██▄▄▄▄██ ▓██ █▄
;;                        ░ ██▒▓░ ▓█   ▓██▒▒██▒ █▄
;;                         ██▒▒▒  ▒▒   ▓▒█░▒ ▒▒ ▓▒
;;                       ▓██ ░▒░   ▒   ▒▒ ░░ ░▒ ▒░
;;                       ▒ ▒ ░░    ░   ▒   ░ ░░ ░
;;                       ░ ░           ░  ░░  ░
;;                       ░ ░
;;
;;             ▄████▄   ▒█████   ███▄    █   █████▒██▓  ▄████ 
;;            ▒██▀ ▀█  ▒██▒  ██▒ ██ ▀█   █ ▓██   ▒▓██▒ ██▒ ▀█▒
;;            ▒▓█    ▄ ▒██░  ██▒▓██  ▀█ ██▒▒████ ░▒██▒▒██░▄▄▄░
;;            ▒▓▓▄ ▄██▒▒██   ██░▓██▒  ▐▌██▒░▓█▒  ░░██░░▓█  ██▓
;;            ▒ ▓███▀ ░░ ████▓▒░▒██░   ▓██░░▒█░   ░██░░▒▓███▀▒
;;            ░ ░▒ ▒  ░░ ▒░▒░▒░ ░ ▒░   ▒ ▒  ▒ ░   ░▓   ░▒   ▒
;;              ░  ▒     ░ ▒ ▒░ ░ ░░   ░ ▒░ ░      ▒ ░  ░   ░
;;            ░        ░ ░ ░ ▒     ░   ░ ░  ░ ░    ▒ ░░ ░   ░
;;            ░ ░          ░ ░           ░         ░        ░
;;            ░


;;; Prepare for configuration

;; prevent configuration ghosts by disabling persistence for customize
(setq custom-file (make-temp-file ""))

;; use straight and use-package to manage config
(setq straight-use-package-by-default t)
(setq use-package-enable-imenu-support t)
(setq straight-vc-git-default-protocol 'ssh)

;; load uncompiled lisp if it's newer
;; (does this need to happen later to not interfere with initial compilation?)
(setq load-prefer-newer t)

;; quiet compilation warnings

;; utf-8 everywhere hopefully
(prefer-coding-system 'utf-8)

;; put config files in reasonable places
(use-package no-littering)

;; TODO don't litter backup files everywhere

;;; Set basic variables

(setq user-full-name "Emily Hyland"
      user-mail-address "hello@duien.com")

;; make home the default directory regardless of where emacs is started from
(cd "~/")

;; set org directory early so it can be used in other setup
(setq org-directory "~/Org/")


(setq-default fill-column 80)
(setq sentence-end-double-space nil
      vc-follow-symlinks t
      dired-use-ls-dired nil
      mouse-wheel-flip-direction t
      mouse-wheel-tilt-scroll t
      custom-safe-themes t
      use-short-answers t
      bookmark-set-fringe-mark nil)
(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-screen t)

;; disable automatic `distant-foreground' switch
;; TODO Maybe consider tweaking specific faces that fail with a generous
;;      threshold (8000 seemed reasonable) instead of disabling totally
(setq face-near-same-color-threshold 0)

;; Display menu-bar-mode only in GUI frames
;; From https://emacs.stackexchange.com/a/29443/6853
;; (menu-bar-mode -1)
(defun eh/contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                             (if (display-graphic-p frame)
                                  1 0)))
(add-hook 'after-make-frame-functions 'eh/contextual-menubar)

(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "lisp/")))
(setq custom-theme-directory (concat user-emacs-directory
                                     (convert-standard-filename "themes/")))

;; TODO figure out how to get frame splitting reasonable
;; (this it not it)
;; (setq split-width-threshold 80)

;;; Some foundational pieces to make a functional UI

;; stop shouting at me all the time
(use-package mode-line-bell
  :config
  (mode-line-bell-mode))

;; allow repeating some commands without repeating prefixes
(use-package repeat
  :straight (:type built-in)
  :config (repeat-mode))

;; make killing and opening lines act sensibly
(use-package crux
  :bind
  ([remap move-beginning-of-line] . 'crux-move-beginning-of-line)
  ([remap kill-line] . 'crux-smart-kill-line)
  ([remap open-line] . 'crux-smart-open-line)
  ("C-x w s" . 'crux-swap-windows))

;; disable pinch-to-zoom and mouse wheel text scaling
(global-unset-key (kbd "<pinch>"))
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

;; set up soft-wrapping
(use-package emacs
  :straight nil
  :preface
  (defun eh/disable-horiz-scroll-with-visual-line ()
    (setq-local mouse-wheel-tilt-scroll (not visual-line-mode)))
  :config
  (setq-default word-wrap t)
  (setq-default truncate-lines t)
  (setq comment-auto-fill-only-comments t)
  :hook
  (visual-line-mode . eh/disable-horiz-scroll-with-visual-line))

;; attemp to tame indentation
(use-package emacs
  :straight nil
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq standard-indent 2)
  (setq js-indent-level 2)
  (setq css-indent-offset 2)
  (setq tab-always-indent t
	      require-final-newline t))

;; typing with an active region replaces it
(delete-selection-mode 1)

;; attempt to let someone else tame indentation
(use-package editorconfig
  :config
  (editorconfig-mode))

;; show line numbers in programming modes
(use-package display-line-numbers
  :straight nil
  :init
  (setq-default display-line-numbers-widen t
                display-line-numbers-width 3)
  :hook prog-mode)

;; show fill column in programming modes
(use-package display-fill-column-indicator
  :straight nil
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  )

;; show indent guides in programming modes
(use-package highlight-indent-guides
  :disabled
  :init
  (setq highlight-indent-guides-method 'bitmap) ; character, column, bitmap, or fill
  (setq highlight-indent-guides-responsive 'top) ; nil, top, or stack
  (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)
  :hook
  (prog-mode . highlight-indent-guides-mode))

;; show tabs
(use-package tab-bar
  :straight nil
  :preface
  ;; add spaces around the tab name (horizontally with spaces, and vertically
  ;; with text properties)
  (defun eh/tab-bar-tab-name-format-comfortable (tab i)
    (propertize (concat
                 (propertize " " 'display '(raise 0.2))
                 (tab-bar-tab-name-format-default tab i)
                 (propertize " " 'display '(raise -0.2))
                 )
                'face (funcall tab-bar-tab-face-function tab)))
  :init
  (setq tab-bar-tab-name-format-function #'eh/tab-bar-tab-name-format-comfortable)
  (setq tab-bar-close-button-show nil
        tab-bar-auto-width nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-format
        '(tab-bar-format-history
          tab-bar-format-tabs
          tab-bar-separator))
  :bind
  ("C-x t n" . tab-bar-new-tab)
  ("C-x t <return>" . tab-switch))

;;; Manageable mode-line

;; don't junk up the mode-line
(use-package minions
  :after (fontaine)
  :init
  ;; (setq minions-mode-line-lighter "#")
  ;; (setq minions-mode-line-lighter "≡")
  ;; other chars: м ≡ ‡
  ;; TODO Find a way to hook this up to auto-refresh with fontaine change
  (setq minions-mode-line-lighter
        (pcase fontaine-current-preset
          ('jetbrains "≡")
          (t          "#")))

  (setq minions-mode-line-delimiters '("" . ""))
  :config
  (minions-mode))

(use-package mood-line
  :after (fontaine)
  :preface
  (defun eh/mood-line-segment-dedicated ()
      (if (window-dedicated-p (selected-window)) "◆"))
  :init
  ;; TODO Put general mode line config somewhere better
  (setq mode-line-percent-position nil)
  (setq mode-line-position-line-format
        (pcase fontaine-current-preset
          ('jetbrains '(" ℓ%l"))
          (t          '(" +%l"))))
  (setq mode-line-position-column-line-format '(" +%l:%c"))
  :config
  (setq mood-line-format
        (mood-line-defformat
         :left
         ( " "
          ((mood-line-segment-modal) . " ")
          ((mood-line-segment-buffer-status) . " ")
          ((mood-line-segment-buffer-name) . " ")
          ((mood-line-segment-cursor-position) . " ")
          )
         :right
         (
          ((mood-line-segment-vc) . " ")
          (mood-line-segment-major-mode)
          (eh/mood-line-segment-dedicated)
          )
         :padding ""
         ))
  ;; :config
  ;; override their non-customizable cursor position segment
  (defun mood-line-segment-cursor-position ()
    (format-mode-line (car mode-line-position-column-line-format) 'mood-line-unimportant)
    ;; (format-mode-line (car mode-line-position-line-format) 'mood-line-unimportant)
    )
  (defun mood-line-segment-major-mode ()
    "Displays the current major mode in the mode-line."
    (concat (format-mode-line minions-mode-line-modes 'mood-line-major-mode) ""))
  (mood-line-mode 1))


;;; Minibuffer completion

;; set up good candidate menus
(use-package consult
  :init
  (setq consult-preview-key "M-.")
  (setq consult-narrow-key "<")
  :config
  ;; disable automatic preview for consult-theme until I find a way to make it
  ;; play nice with having multiple themes enabled
  ;; (consult-customize consult-theme
  ;;                    :preview-key
  ;;                    '("M-."
  ;;                      :debounce 0.5 "<up>" "<down>"
  ;;                      :debounce 1 any))
  (consult-customize consult-line
                     :preview-key
                     '(:debounce 0.5 "<up>" "<down>"
                       :debounce 1 any))
  :bind
  ("C-x b" . 'consult-buffer)
  ("C-x 4 b" . 'consult-buffer-other-window)
  ("C-x 5 b" . 'consult-buffer-other-frame)
  ("C-x i" . 'consult-imenu)
  ("C-h t" . 'consult-theme)
  ("C-h i" . 'consult-info))

;; vertical completion menu
(use-package vertico
  :config (vertico-mode))

;; show additional info about candidates
(use-package marginalia
  :config (marginalia-mode))

;; completion matching on space-separated tokens
(use-package orderless
  :init
  (setq orderless-matching-styles '(orderless-regexp))
  :config
  (setq completion-styles '(orderless)))

;; show commands on pause
(use-package which-key
  :init
  (setq which-key-compute-remaps t)
  :config (which-key-mode))

;;; Basic appearance

;; easily configure and switch between fonts
(use-package fontaine
  :preface
  (defvar eh/base-font-height
    ;; (if (> (x-display-pixel-width) 2500)
    ;;     160 140)
    ;; 140
    160
    "The main font size, based on display resolution.")
  :demand ;; our hook won't activate the package but implies deferred loading
  :init
  (setq fontaine-presets
	`((jetbrains ;; basic and functional, but attractive
	   :default-family "JetBrains Mono"
	   :default-weight light
     :default-height ,(- eh/base-font-height 10))
	  (cascadia ;; nice italic, readable but still fun
	   :default-family "Cascadia Code PL"
     :default-weight light
     :line-spacing 1
     ;; :default-height ,(+ eh/base-font-height 10)
	   :default-weight normal)
    (comic-code ;; totally silly, line height is too big, but it's great
     :default-family "Comic Code Ligatures"
     :default-height ,(- eh/base-font-height 20))
    (codelia ;; a good mix of personality and readability
     :default-family "Codelia Ligatures")
    (input ;; space-efficient, good variable pitch pairing options
     :default-family "Input Mono Condensed"
     :variable-pitch-family "Input Sans Narrow"
     :default-weight light)
    (monolisa ;; decent mix of personality and utility
     :default-family "MonoLisa"
     ;; :default-weight light
     :default-height ,(- eh/base-font-height 10))
    (monolisa-script
     :default-family "MonoLisa Script"
     :default-height ,(- eh/base-font-height 10))
    (plex ;; nice italic, excellent variable pitch pairing
     :default-family "IBM Plex Mono"
     :variable-pitch-family "iA Writer Quattro V")
    (operator ;; poor character coverage, but awesome
     ;; NOTE: To make this work, enable _only_ the Light and Medium weights
     :default-family "Operator Mono SSm"
     :default-weight regular)
    (belinsky ;; weights don't have the same line-height
     :default-family "Belinsky Text"
     :default-weight regular)
    (victor ;; a fun script italic
     :default-family "Victor Mono")
    (julia ;; great character set, unexciting font
     :default-family "JuliaMono"
     :default-weight regular)
    (apple-sf ;; very simple, very well done, good pairings
     :default-family "SF Mono"
     ;; :variable-pitch-family "SF Pro Text"
     :variable-pitch-family "New York Small"
     :variable-pitch-height 1.1 ;; (for new york, but not working)
     :default-weight light)
    (pragmata ;; a classic for a reason
     :default-family "PragmataPro Liga")
    (vctr
     :default-family "VCTR Mono"
     :default-weight light
     )
    (berkeley
     :default-family "Berkeley Mono")
    (md-io
     :default-family "MD IO Trial"
     :default-weight light)
    (dank
     :default-family "Dank Mono")
    (poly
     :default-family "PolySans Trial")
    (dolph
     :default-family "Dolph YY Test"
     :variable-pitch-family "Dolph KOO Test"
     :default-weight light
     :default-height ,(- eh/base-font-height 10))
    (name
     :default-family "AT Name Mono Trial")
    (dico
     :default-family "Dico Mono TEST")
	  (t
	   :default-height ,eh/base-font-height)))
  :config
  (fontaine-set-preset (fontaine-restore-latest-preset))
  :hook
  (fontaine-set-preset . fontaine-store-latest-preset)
  (fontaine-set-preset . diff-hl-maybe-redefine-bitmaps))
;; (diff-hl-maybe-redefine-bitmaps)

;; Set up a variable and hook to automatically load a theme after another theme
;; is enabled (this allows for more robust customizations to themes than what
;; can be done with a second call to `custom-theme-set-faces')

;; quickly swap to added window and modeline padding
(use-package spacious-padding)
;; (set-face-attribute 'mode-line-active nil :inherit 'mode-line)

(defvar linked-themes '() "Alist of theme to another theme to load automatically after it.")
(defun eh/load-linked-user-theme (theme)
  "Check `linked-themes' for THEME and load the associated theme if present."
  (let ((theme-to-load (cdr (assoc theme linked-themes))))
    (if theme-to-load
        (load-theme theme-to-load))))
(add-hook 'enable-theme-functions 'eh/load-linked-user-theme)

;; Also set automatic appearance-switching so that the title bar text stays visible.
;; This runs an excessive number of times, but that doesn't really seem to be a problem
(defun eh/set-ns-appearance-from-theme (theme)
  (let ((appearance (frame-parameter nil 'background-mode)))
    (modify-all-frames-parameters `((ns-appearance . ,appearance)))))
(add-hook 'enable-theme-functions 'eh/set-ns-appearance-from-theme)
(add-hook 'ns-appearance-change-functions 'eh/set-ns-appearance-from-theme)

;; a theme for all seasons
(use-package modus-themes
  :straight (modus-themes :type git :host github :repo "protesilaos/modus-themes")
  :init
  (setq modus-themes-italic-constructs t
	      modus-themes-bold-constructs t
        modus-themes-mixed-fonts t)
  (setq modus-themes-common-palette-overrides
	      '((border-mode-line-active unspecified)
	        (border-mode-line-inactive unspecified)
	        (comment fg-dim)
	        (string green)))
  (setq modus-vivendi-tinted-palette-overrides
	      '((string cyan)))
  (add-to-list 'linked-themes '(modus-operandi-tinted . user-modus-operandi-tinted))
  (add-to-list 'linked-themes '(modus-vivendi-tinted . user-modus-vivendi-tinted))
  ;; :config
  ;; (load-theme 'modus-operandi-tinted)
  )

;;; Manage windows and buffers

;; keep track of recently opened files
(use-package recentf
  :straight (:type built-in)
  :init
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude
	      `(,(expand-file-name "straight/build/" user-emacs-directory)
          ,(expand-file-name "eln-cache/" user-emacs-directory)
          ,(expand-file-name "etc/" user-emacs-directory)
          ,(expand-file-name "var/" user-emacs-directory)
          ,(expand-file-name "init.org" org-directory)))
  :config (recentf-mode)
  ;; TODO Does removing this stop the issue with tracking `init.org' even though
  ;; it should be ignored? It doesn's seem like it should since I think the
  ;; other ignores actually are working (...or are they?)
  ;; :hookf
  ;; (buffer-list-update . recentf-track-opened-file)
  )

;; allow undoing and redoing window layout changes
(use-package winner
  :straight (:type built-in)
  :bind ("C-x w <left>" . 'winner-undo)
  :bind ("C-x w <right>" . 'winner-redo)
  :config (winner-mode))

;; projects defined mostly by git repos
;; load project before projectile to prevent project from over-writing the
;; projectile binding I set up when something else loads it later
(use-package project)
(use-package projectile
  :after (project)
  :preface
  (defun eh/wrapped-project-name (before after &optional fallback)
    (let ((project-name (projectile-project-name)))
      (if (string= "-" project-name)
          fallback
        (concat before (projectile-project-name) after " " fallback))))
  (defun eh/name-tab-with-project-or-default ()
    (eh/wrapped-project-name "[" "]" (tab-bar-tab-name-current)))
  :init
  (setq tab-bar-tab-name-function #'eh/name-tab-with-project-or-default)
  (setq projectile-project-search-path '(("~/Code" . 3)
                                         ("~/.homesick/repos" . 1)))
  ;; ignoring specific buffers by name
  (setq projectile-globally-ignored-buffers
        '("*scratch*"
          "*lsp-log*"))

  ;; ignoring buffers by their major mode
  (setq projectile-globally-ignored-modes
        '("erc-mode"
          "help-mode"
          "info-mode"
          "completion-list-mode"
          "Buffer-menu-mode"
          "gnus-.*-mode"
          "occur-mode"))
  ;; (bind-key "C-x p" 'projectile-command-map)
  :config
  ;; try out a permissive bottom-up project search
  ;; (pretty much prevents ever using top-down)
  (setq projectile-project-root-files-bottom-up
        (append
         (remove "TAGS" (remove "DESCRIPTION" projectile-project-root-files))
         projectile-project-root-files-bottom-up))
  ;; the `Project.toml' file used to detect a julia project is also used to
  ;; configure Maestro, and was clobbering rails
  (projectile-update-project-type 'julia :precedence 'low)
  (projectile-add-known-project "~/Org")
  (projectile-add-known-project "~/Notes")
  (setq frame-title-format
      '("%b" (:eval (eh/wrapped-project-name " [" "]"))))
  ;; '("%b" (:eval (eh/name-tab-with-project-or-default))))
  (projectile-mode)
  :bind
  (("C-x p" . 'projectile-command-map)
   ("C-x p b" . 'consult-project-buffer)))

;;; Utilities

;; show imenu entries in a sidebar
(use-package imenu-list
  :bind
  ("C-x /" . 'imenu-list-smart-toggle))

;; don't save white-space, but allow it to persist in the buffer after saving
(use-package ws-butler
  :config
  (ws-butler-global-mode))

;; search with ag
(use-package ag)

;;; Advanced appearance

(use-package ligature
  :straight '(ligature :type git :host github
		                   :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   ;; 'prog-mode
   t
   ;; ligature list for PragmataPro (except comment lig like FIXME)
   '("!!" "!=" "!==" "!!!" "!≡" "!≡≡" "!>" "!=<" "#("
     "#_" "#{" "#?" "#>" "##" "#_(" "%=" "%>" "%>%" "%<%"
     "&%" "&&" "&*" "&+" "&-" "&/" "&=" "&&&" "&>" "$>"
     "***" "*=" "*/" "*>" "++" "+++" "+=" "+>" "++=" "--"
     "-<" "-<<" "-=" "->" "->>" "---" "-->" "-+-" "-\\/"
     "-|>" "-<|" ".." "..." "..<" ".>" ".~" ".=" "/*" "//"
     "/>" "/=" "/==" "///" "/**" ":::" "::" ":=" ":≡" ":>"
     ":=>" ":(" ":-(" ":)" ":-)" ":/" ":\\" ":3" ":D" ":P"
     ":>:" ":<:" "<$>" "<*" "<*>" "<+>" "<-" "<<" "<<<" "<<="
     "<=" "<=>" "<>" "<|>" "<<-" "<|" "<=<" "<~" "<~~" "<<~"
     "<$" "<+" "<!>" "<@>" "<#>" "<%>" "<^>" "<&>" "<?>" "<.>"
     "</>" "<\\>" "<\">" "<:>" "<~>" "<**>" "<<^" "<!" "<@"
     "<#" "<%" "<^" "<&" "<?" "<." "</" "<\\" "<\"" "<:" "<->"
     "<!--" "<--" "<~<" "<==>" "<|-" "<<|" "<-<" "<-->" "<<=="
     "<==" "=<<" "==" "===" "==>" "=>" "=~" "=>>" "=/=" "=~="
     "==>>" "≡≡" "≡≡≡" "≡:≡" ">-" ">=" ">>" ">>-" ">>=" ">>>"
     ">=>" ">>^" ">>|" ">!=" ">->" "??" "?~" "?=" "?>" "???"
     "?." "^=" "^." "^?" "^.." "^<<" "^>>" "^>" "\\\\" "\\>"
     "\\/-" "@>" "|=" "||" "|>" "|||" "|+|" "|->" "|-->" "|=>"
     "|==>" "|>-" "|<<" "||>" "|>>" "|-" "||-" "~=" "~>" "~~>"
     "~>>" "[[" "]]" "\">" "_|_")
	 ;; '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
   ;;   ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
   ;;   "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
   ;;   "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
   ;;   "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
   ;;   "..." "+++" ";;;" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
   ;;   "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
   ;;   "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
   ;;   ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
   ;;   "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
   ;;   "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
   ;;   "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
   ;;   "\\\\" "://")
   )
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode)
  ;; :hook
  ;; regenerate ligatures when switching fonts (somehow)
  ;; (fontaine-set-preset . )
  )

(use-package olivetti
  :init
  (setq olivetti-style nil)
  :hook
  org-mode
  gfm-mode)

;;; Version control

;; the best git interface
(use-package magit)

;; show diff in fringe
(use-package diff-hl
  :after (magit)
  :demand
  :config
  (global-diff-hl-mode)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

;;; Languages

(use-package fish-mode
  :init
  (setq fish-indent-offset 2))
(use-package haml-mode)
(use-package slim-mode)
(use-package sass-mode)
(use-package json-mode)
(use-package yaml-mode)
(use-package rspec-mode)
(use-package markdown-mode
  :mode
  (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode)))

;; (treesit-language-available-p 'ruby)
;; (setq major-mode-remap-alist '((ruby-mode . ruby-ts-mode)))
(use-package ruby-mode
  :straight (:type built-in)
  :init
  (setq ruby-method-call-indent nil))

;; (use-package enh-ruby-mode
;;   :init
;;   (setq major-mode-remap-alist '((ruby-mode . enh-ruby-mode)))) ;; not enabled by default yet, but has some advantages

(use-package elixir-mode
  :config
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))
(use-package elixir-ts-mode
  :straight '(:type git :host github :repo "duien/elixir-ts-mode")
  :config
  (add-hook 'elixir-ts-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

;;; Extra color themes

(use-package autothemer)
;; TODO make these themes properly load autothemer
(use-package isohedron-theme
  :straight '(isohedron-theme :type git :host github
		                          :repo "duien/isohedron-theme")
  :init
  (add-to-list 'linked-themes '(isohedron . user-isohedron))
  :config
  (load-theme 'isohedron))
(use-package caves-of-qud-theme
  :straight '(caves-of-qud-theme :type git :host github
                                 :repo "duien/caves-of-qud-theme")
  :init
  (add-to-list 'linked-themes '(caves-of-qud . user-qud)))

(use-package ef-themes
  :straight (ef-themes :type git :host github :repo "protesilaos/ef-themes"))
(use-package spacemacs-theme
  :init
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-comment-italic t
        spacemacs-theme-keyword-italic t
        spacemacs-theme-org-height nil)
  (add-to-list 'linked-themes '(spacemacs-light . user-spacemacs-light)))

;;; Org

(use-package org
  :straight (:type built-in)
  :init
  ;; where to put things
  (setq org-agenda-files `(,org-directory ,(concat org-directory "skylight/"))
        org-refile-targets '((org-agenda-files . (:maxlevel . 5)))
        org-default-notes-file (concat org-directory "inbox.org"))
  ;; archiving
  (setq org-archive-location (concat org-directory "Archive/%s_archive::datetree/"))
  ;; logging
  (setq org-log-done t
        org-log-into-drawer nil)
  ;; startup
  (setq org-startup-truncated t
        org-startup-folded 'showall)
  ;; how commands work - movement
  (setq org-insert-heading-respect-content t
        org-cycle-separator-lines 1
        org-M-RET-may-split-lines '((default . t)
                                    (item . nil)))
  ;; how commands work - editing
  (setq org-ctrl-k-protect-subtree t
        org-fold-catch-invisible-edits 'show
        org-src-preserve-indentation t
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil))
        org-tags-column 0
        org-auto-align-tags nil)
  ;; how agenda works
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-dim-blocked-tasks t)
  ;; stars (combine with org-superstar)
  (setq org-hide-leading-stars nil
        org-indent-mode-turns-on-hiding-stars nil)
  ;; emphasis markers
  (setq org-emphasis-alist
        '(("*" bold)
          ("_" italic)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+"
           (:strike-through t))))
  ;; fontification
  (setq org-fontify-whole-block-delimiter-line t
        org-fontify-whole-heading-line t
        org-fontify-todo-headline t
        org-fontify-done-headline t
        org-cycle-level-faces nil)
  ;; popup buffers
  (setq org-use-fast-todo-selection 'expert
        org-src-window-setup 'current-window
        org-agenda-window-setup 'other-window
        org-agenda-restore-windows-after-quit t)
  ;; todo keywords - current suggested
  (setq org-todo-keywords
        '((sequence "BURY(b)" "NEXT(n)" "TODO(t)" "HALT(h)" "|" "DONE(d)" "KILL(k@)")
          (sequence "QUEST(q)" "|" "MEH(m)" "YES(Y)" "NO(N)" "ANSWER(a@)")
          (type "IDEA(i)" "GOAL(g)" "|")
          (sequence "READ(R)" "|" "RODE(r)")
          ))

  ;; capture
  (setq org-capture-templates
        '(("t" "Some thing" entry (file+headline "~/Org/inbox.org" "Inbox")
           "* TODO %?\n%u\n%i")
          ("T" "Some thing (with context)" entry (file+headline "~/Org/inbox.org" "Inbox")
           "* TODO %?\n%u\n%a\n%i")
          ;; TODO Find a way to dynamically grab the latest dox file
          ;; ("w" "Work thing" entry (file+headline "~/Org/dox-23Q4.org" "Inbox")
          ;;  "* TODO %?\n%a\n%i")
          ("y" "Yaks thing" entry (file+headline "~/Org/yaks.org" "Inbox")
           "* TODO %?\n%u\n%i")
          ("Y" "Yaks thing (with context)" entry (file+headline "~/Org/yaks.org" "Inbox")
           "* TODO %?\n%u\n%a\n%i")
          ))
  ;; load the complex keyword config
  ;; TODO add the rest of my org stuff
  (load "org-configuration")
  (eh/define-org-keywords)
  :config
  (advice-add 'org-cycle-set-startup-visibility
              :after 'eh/org-hide-done-entries-in-buffer)
  :bind
  (:map org-mode-map
        ("C-o" . 'crux-smart-open-line))
  (:map org-src-mode-map
        ("C-c C-c" . 'org-edit-src-exit))
  :hook
  (org-mode . org-indent-mode)
  (fontaine-set-preset . eh/define-org-keywords)
  (org-ctrl-c-ctrl-c . eh/hook-edit-src-block))

;; make org prettier
(use-package org-superstar
  :after (org)
  :init
  (setq org-superstar-cycle-headline-bullets nil
        org-superstar-special-todo-items t
        org-superstar-leading-fallback "·"
        org-superstar-leading-bullet "·"
        org-superstar-remove-leading-stars nil
        org-superstar-prettify-item-bullets nil)
  :hook
  (org-mode . org-superstar-mode))

;; (use-package org-appear
;;   :after (org)
;;   :hook (org-mode . org-appear-mode))
;; ;; TEST
;; (setq org-hide-emphasis-markers t)

;;; Misc Stuff to be Organized

(use-package outline
  :straight (:type built-in)
  :init
  (setq outline-blank-line t
        outline-minor-mode-use-buttons 'in-margins))


(defun eh/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(bind-key "C-x w r" #'eh/toggle-window-split)

;; make various other-window functions prefer not to use this window
(defun eh/toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))
(bind-key "C-x w d" #'eh/toggle-window-dedication)

(bind-key "C-x w f" #'toggle-frame-fullscreen)

;; don't evaluate a whole buffer without confirmation
(defun confirm-eval (&optional ARG PRED)
  (interactive)
  (if (use-region-p)
      t
    (yes-or-no-p "Evaluate buffer?")))
(advice-add 'elisp-eval-region-or-buffer :before-while 'confirm-eval)

;; restart org-mode preserving visibility
(defun eh/org-mode-restart ()
  "Restart org mode preserving visibility"
  (interactive)
  (org-save-outline-visibility 'use-markers (org-mode-restart)))

(use-package persistent-scratch
  :init
  ;; only allow killing scratch when killing all of emacs
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  :config
  (persistent-scratch-setup-default)
  (with-current-buffer "*scratch*"
    (persistent-scratch-mode 1)))

(use-package expand-region
  :bind
  ("C-=" . 'er/expand-region))

(use-package rainbow-mode
  :init
  (setq rainbow-ansi-colors nil
        rainbow-x-colors nil))

(use-package helpful
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key))

(use-package elec-pair
  :straight (:type built-in)
  :config (electric-pair-mode))

(use-package lua-mode
  :init
  (setq lua-indent-level 2))

(use-package vterm)

(use-package rust-mode)

(use-package julia-mode)

(use-package swift-mode
  :init
  (setq swift-mode:basic-offset 2))

(use-package raku-mode)

(use-package elpher)

(use-package combobulate)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((elixir-ts-mode elixir-mode) "elixir-ls"))
  :hook (elixir-mode . eglot-ensure)
  :hook (elixir-ts-mode . eglot-ensure)
  )

(use-package company-mode
  :hook (elixir-mode . company-mode)
  :hook (elixir-ts-mode . company-mode))

;; When I create a buffer, I don't want it silently deleted, but there don't
;; seem to be any smooth ways to make that happen. My current best attempt:
;; - if the buffer is not visiting a file
;;   (because then this would already happen)
;; - and it's not a hidden or special buffer as identified by name prefix
;; - then set the `buffer-offer-save' local variable that causes emacs to
;;   prompt for saving it before quitting
;; this will not affect manually killing the buffer
(defun eh/set-offer-save-in-created-buffers ()
  (unless (or buffer-file-name
              (string-match-p "^magit" (buffer-name))
              (string-match-p "^ " (buffer-name))
              (string-match-p "^*" (buffer-name)))
    (setq buffer-offer-save t)))
(add-hook 'first-change-hook #'eh/set-offer-save-in-created-buffers)

;; Highlight todo comments in programming modes
(use-package hl-todo
  :init
  (setq hl-todo-keyword-faces
        '(("TODO" . eh/org-keyword-todo)
          ("FIXME" . eh/org-keyword-halt)
          ("NOTE" . eh/org-keyword-read)
          ("HACK" . eh/org-keyword-idea)))
  :hook prog-mode)

;;; Finally

;; ensure a server is started (for opening from dock)
(require 'server)
(unless (server-running-p) (server-start))

;;; TODO
;; - kill-visual-line in visual-line-mode-map
;;   - separate binding to kill actual (logical?) line (oh, probably still just
;;     kill-line or crux-kill-line)
;; - crux altered key-binds in org-mode
;; - project files in consult-buffer default sources
;; - go to buffer in default perspective (also from command line)
;; - save light and dark themes (like fontaine)
;; - use-package imenu integration gets bogus marginalia annotations
;; -
;;
;; - add-previous-buffer
;;   the equivalent of C-x 3 C-x [LEFT]
;;   (the previous buffer is opened in a left split and point is there)


;; Local Variables:
;; eval: (outline-minor-mode)
;; End:
