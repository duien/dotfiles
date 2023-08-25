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


(setq-default fill-column 80)
(setq sentence-end-double-space t
      vc-follow-symlinks t
      dired-use-ls-dired nil
      mouse-wheel-flip-direction t
      mouse-wheel-tilt-scroll t
      custom-safe-themes t
      use-short-answers t
      bookmark-set-fringe-mark nil)
(setq frame-inhibit-implied-resize t)

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
  :hook
  prog-mode)

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
  :init
  (setq minions-mode-line-lighter "≡")
  :config
  (minions-mode))

;; TODO Put this somewhere better
;; (setq mode-line-position-line-format '(" %l"))
;; (setq mode-line-position-line-format '(" +%l"))
(setq mode-line-percent-position nil)
(setq mode-line-position-line-format '(" ℓ%l"))
(setq mode-line-position-column-line-format '(" +%l:%c"))


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
  ;; (load "consult-tab-bar")
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
    (if (> (x-display-pixel-width) 2500)
        160 140)
    "The main font size, based on display resolution.")
  :demand ;; our hook won't activate the package but implies deferred loading
  :init
  (setq fontaine-presets
	`((jetbrains ;; basic and functional, but attractive
	   :default-family "JetBrains Mono"
	   :default-weight light)
	  (cascadia ;; nice italic, readable but still fun
	   :default-family "Cascadia Code PL"
     ;; :default-height ,(+ eh/base-font-height 10)
	   :default-weight normal)
    (comic-code ;; totally silly, line height is too big, but it's great
     :default-family "Comic Code Ligatures")
    (codelia ;; a good mix of personality and readability
     :default-family "Codelia Ligatures")
    (input ;; space-efficient, good variable pitch pairing options
     :default-family "Input Mono Condensed"
     :variable-pitch-family "Input Sans Narrow"
     :default-weight light)
    (monolisa ;; decent mix of personality and utility
     :default-family "MonoLisa")
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
     :variable-pitch-family "SF Pro Text"
     ;; :variable-pitch-family "New York Small"
     ;; :variable-pitch-height 1.1 ;; (for new york, but not working)
     :default-weight light)
    (pragmata ;; a classic for a reason
     :default-family "PragmataPro Liga")
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

(defvar linked-themes '() "Alist of theme to another theme to load automatically after it.")
(defun eh/load-linked-user-theme (theme)
  "Check `linked-themes' for THEME and load the associated theme if present."
  (let ((theme-to-load (cdr (assoc theme linked-themes))))
    (if theme-to-load
        (load-theme theme-to-load))))
(add-hook 'enable-theme-functions 'eh/load-linked-user-theme)

;; a theme for all seasons
(use-package modus-themes
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
  (add-to-list 'linked-themes '(modus-operandi-tinted . modus-operandi-tinted-user))
  :config
  (load-theme 'modus-operandi-tinted))

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
          ,(expand-file-name "var/" user-emacs-directory)))
  :config (recentf-mode)
  :hook
  (buffer-list-update . recentf-track-opened-file))

;; allow undoing and redoing window layout changes
(use-package winner
  :straight (:type built-in)
  :config (winner-mode))

;; projects defined mostly by git repos
(use-package projectile
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

;; integrate projectile with perspectives and tabs
(use-package perspective
  ;; this doesn't work when opening files from the command-line, so disable
  ;; (this also disables the two following, since they have an `after')
  :disabled
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  (setq persp-show-modestring nil)
  :config (persp-mode))
(use-package perspective-tabs
  :after (perspective)
  :straight (:host sourcehut :repo "woozong/perspective-tabs")
  :config (perspective-tabs-mode))
(use-package persp-projectile
  :after (perspective projectile))

;;; Utilities

;; show imenu entries in a sidebar
(use-package imenu-list
  :bind
  ("C-x /" . 'imenu-list-smart-toggle))

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
(use-package elixir-mode
  :config
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  ) ;; (use-package elixir-ts-mode)

;;; Extra color themes

(use-package autothemer)
;; TODO make these themes properly load autothemer
(use-package isohedron-theme
  :straight '(isohedron-theme :type git :host github
		                          :repo "duien/isohedron-theme")
  :config
  (custom-theme-set-faces
   'isohedron
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
   ))
(use-package caves-of-qud-theme
  :straight '(caves-of-qud-theme :type git :host github
                                 :repo "duien/caves-of-qud-theme")
  :config
  (custom-theme-set-faces
   'caves-of-qud
   ;; faces for custom telephone-based modeline
   '(eh/telephone-line-status-locked  ((t :background "#A64A2E")))
   '(eh/telephone-line-status-normal  ((t :background "#40A4B9")))
   '(eh/telephone-line-status-unsaved ((t :background "#E99F10")))
   ;; faces for incomplete items
   '(eh/org-keyword-todo     ((t :background "#009403" :inherit org-todo)))
   '(eh/org-keyword-idea     ((t :background "#b154cf" :inherit org-todo)))
   '(eh/org-keyword-question ((t :background "#0096ff" :inherit org-todo)))
   '(eh/org-keyword-read     ((t :background "#98875f" :inherit org-todo)))
   '(eh/org-keyword-next     ((t :background "#cfc041" :inherit org-todo)))
   '(eh/org-keyword-halt     ((t :background "#f15f22" :inherit org-todo)))
   ;; faces for complete items
   '(eh/org-keyword-done     ((t :foreground "#009403" :inherit org-done)))
   '(eh/org-keyword-kill     ((t :foreground "#a64a2e" :inherit org-done)))
   '(eh/org-keyword-answer   ((t :foreground "#0096ff" :inherit org-done)))
   '(eh/org-keyword-yes      ((t :foreground "#00c420" :inherit org-done)))
   '(eh/org-keyword-no       ((t :foreground "#d74200" :inherit org-done)))
   '(eh/org-keyword-rode     ((t :foreground "#98875f" :inherit org-done)))))

(use-package ef-themes)

;;; Org

(use-package org
  :straight (:type built-in)
  :init
  ;; where to put things
  (setq org-directory "~/Org/"
        org-agenda-files `(,org-directory) ;; could we just '(org-directory)?
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

;;; Misc Stuff to be Organized

(use-package outline
  :straight (:type built-in)
  :init
  (setq outline-blank-line t
        outline-minor-mode-use-buttons 'in-margins))

(use-package ws-butler
  :config
  (ws-butler-global-mode))

(use-package ag)

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

(use-package nano-modeline
  :disabled
  :straight (:type git :host github :repo "rougier/nano-modeline" :branch "main")
  :init
  (setq nano-modeline-position 'bottom)
  (setq nano-modeline-prefix 'status)
  ;; TODO set this up to change based on fontaine preset
  (setq nano-modeline-space-top 0.2
        nano-modeline-space-bottom -0.2)
  ;; (setq nano-modeline-space-top 0.1
  ;;       nano-modeline-space-bottom 0)
  :config (nano-modeline-mode))

(use-package persistent-scratch
  :init
  ;; persistent-scratch-scratch-buffer-p-function
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  :config
  ;; TODO the autosave seems to work, but minor mode isn't activated?
  (persistent-scratch-setup-default)
  (with-current-buffer "*scratch*"
    (persistent-scratch-mode 1)))

;; automatically switch themes with system appearance change
;; (defun eh/load-appearance-theme (appearance)
;;   "Load the appropriate light or dark theme based on APPEARANCE"
;;   (mapc 'disable-theme custom-enabled-themes)
;;   (pcase appearance
;;     ;; ('light (modus-themes-load-theme 'modus-operandi-tinted))
;;     ;; ('dark (modus-themes-load-theme 'modus-vivendi-tinted))
;;     ('light (enable-theme 'isohedron))
;;     ('dark (enable-theme 'caves-of-qud))
;;     ))
;; (add-hook 'ns-system-appearance-change-functions #'eh/load-appearance-theme)

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

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(use-package spacemacs-theme
  :init
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-comment-italic t
        spacemacs-theme-keyword-italic t
        spacemacs-theme-org-height nil)
  :config
  (load-theme 'spacemacs-light t t)
  ;; TODO these aren't working quite right with the transition to custom
  ;; (they aren't registering unless I redo them again after loading the theme)
  (custom-theme-set-faces
   'spacemacs-light
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
                             :weight bold))))
  ;; This is not something that a theme should be setting (and it's not clear to
  ;; me why it ends up sticking around after the theme is deactivated again)
  (custom-theme-set-variables
   'spacemacs-light
   '(org-fontify-done-headline t)
   '(org-fontify-todo-headline t))
  ;; (custom-theme-set-variables
  ;;  'spacemacs-dark
  ;;  '(org-fontify-done-headline t)
  ;;  '(org-fontify-todo-headline t))
  )

;; (use-package variable-pitch
;;   :straight (:type built-in)
;;   :hook
;;   gfm-mode
;;   org-mode)

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

;;; TODO
;; - kill-visual-line in visual-line-mode-map
;;   - separate binding to kill actual (logical?) line (oh, probably still just
;;     kill-line or crux-kill-line)
;; - crux altered key-binds in org-mode
;; ✓ expand-region
;; ✓ project detection in scratch buffer
;; - project files in consult-buffer default sources
;; - go to buffer in default perspective (also from command line)
;; - something like my org keyword definer for theme overlays
;; - save light and dark themes (like fontaine)
;; ✓ set up indent guide
;; - bind frame-toggle-fullscreen
;; - try out embark
;; - use-package imenu integration gets bogus marginalia annotations
;; -
;;
;; - add-previous-buffer
;;   the equivalent of C-x 3 C-x [LEFT]
;;   (the previous buffer is opened in a left split and point is there)


;; Local Variables:
;; eval: (outline-minor-mode)
;; End:
