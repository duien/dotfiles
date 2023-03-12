;; init.el -*- lexical-binding: t; -*- no-byte-compile: t -*- 

;;; Prepare for configuration

;; prevent configuration ghosts by disabling persistence for customize
(setq custom-file (make-temp-file ""))

;; use straight and use-package to manage config
(setq straight-use-package-by-default t)
(setq use-package-enable-imenu-support t)

;; quiet compilation warnings

;; utf-8 everywhere hopefully
(prefer-coding-system 'utf-8)

;; put config files in reasonable places
(use-package no-littering)

;; TODO don't litter backup files everywhere

;;; Set basic variables

(setq user-full-name "Emily Hyland"
      user-mail-address "hello@duien.com")

(setq-default fill-column 80)
(setq sentence-end-double-space t
      vc-folow-symlinks t
      dired-use-ls-dired nil
      mouse-wheel-flip-direction t
      mouse-wheel-tilt-scroll t
      custom-safe-themes t
      use-short-answers t
      bookmark-set-fringe-mark nil)

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
  ([remap open-line] . 'crux-smart-open-line))

;; set up soft-wrapping 
(use-package emacs
  :straight nil
  :preface
  (defun eh/disable-horiz-scroll-with-visual-line ()
    (setq-local mouse-wheel-tilt-scroll (not visual-line-mode)))
  :config
  (setq-default word-wrap t)
  (setq-default truncate-lines t)
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
  (setq fish-indent-offset 2)
  (setq tab-always-indent t
	      require-final-newline t))

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
  ;; add spaces around the tab name
  (defun eh/tab-bar-tab-name-format-comfortable (tab i)
    (propertize (concat " " (tab-bar-tab-name-format-default tab i) " ")
                'face (funcall tab-bar-tab-face-function tab)))
  :init
  (setq tab-bar-tab-name-format-function #'eh/tab-bar-tab-name-format-comfortable)
  (setq tab-bar-close-button-show nil
        tab-bar-auto-width nil)
  (setq tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator)))

;;; Manageable mode-line

;; don't junk up the mode-line
(use-package minions
  :init
  (setq minions-mode-line-lighter "≡")
  :config
  (minions-mode))

;; TODO Put this somewhere better
(setq mode-line-position-line-format '(" %l"))
;; (setq mode-line-position-line-format '(" +%l"))
;; (setq mode-line-position-column-line-format '(" +%l:%c"))


;;; Minibuffer completion

;; set up good candidate menus
(use-package consult
  :init
  (setq consult-preview-key "M-.")
  (setq consult-narrow-key "<")
  :bind
  ("C-x b" . 'consult-buffer)
  ("C-x 4 b" . 'consult-buffer-other-window)
  ("C-x 5 b" . 'consult-buffer-other-frame))

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
	`((jetbrains
	   :default-family "JetBrains Mono"
	   :default-weight light)
	  (cascadia
	   :default-family "Cascadia Code PL"
     :default-height ,(+ eh/base-font-height 10)
	   :default-weight normal)
	  (t
	   :default-height ,eh/base-font-height)))
  :config
  (fontaine-set-preset (fontaine-restore-latest-preset))
  :hook
  (fontaine-set-preset . fontaine-store-latest-preset))

;; a theme for all seasons
(use-package modus-themes
  :preface
  (defun eh/load-modus-appearance-theme (appearance)
    "Load the appropriate light or dark modus theme based on APPEARANCE"
    (pcase appearance
      ('light (modus-themes-load-theme 'modus-operandi-tinted))
      ('dark (modus-themes-load-theme 'modus-vivendi-tinted))))
  :init
  (setq modus-themes-italic-constructs t
	      modus-themes-bold-constructs t)
  (setq modus-themes-common-palette-overrides
	      '((border-mode-line-active unspecified)
	        (border-mode-line-inactive unspecified)
	        (comment fg-dim)
	        (string green)))
  (setq modus-vivendi-tinted-palette-overrides
	      '((string cyan)))
  (add-hook 'ns-system-appearance-change-functions #'eh/load-modus-appearance-theme)
  :config
  ;; TODO this may not be necessary since the hook is called on startup
  (eh/load-modus-appearance-theme ns-system-appearance))

;;; Manage windows and buffers

;; keep track of recently opened files
(use-package recentf
  :straight (:type built-in)
  :init
  (setq recentf-exclude
	`(,(expand-file-name "straight/build/" user-emacs-directory)
          ,(expand-file-name "eln-cache/" user-emacs-directory)
          ,(expand-file-name "etc/" user-emacs-directory)
          ,(expand-file-name "var/" user-emacs-directory)))
  :config (recentf-mode))

;; allow undoing and redoing window layout changes
(use-package winner
  :straight (:type built-in)
  :config (winner-mode))

;;; Utilities

;; show imenu entries in a sidebar
(use-package imenu-list
  ;; TODO bind imenu-list-smart-toggle
  )

;;; Advanced appearance

(use-package ligature
  :straight '(ligature :type git :host github
		       :repo "mickeynp/ligature.el")
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
	 '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://"))
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
  org-mode)

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

(use-package fish-mode)
(use-package haml-mode)
(use-package slim-mode)
(use-package sass-mode)
(use-package json-mode)
(use-package yaml-mode)

;;; Extra color themes

(use-package autothemer)
(use-package isohedron-theme
  :straight '(isohedron-theme :type git :host github
		                          :repo "duien/isohedron-theme"))
(use-package caves-of-qud-theme
  :straight '(caves-of-qud-theme :type git :host github
                                 :repo "duien/caves-of-qud-theme"))

;;; Misc Stuff to be Organized

(use-package outline
  :straight (:type built-in)
  :config
  (setq outline-blank-line t
        outline-minor-mode-use-buttons 'in-margins))

;; Local Variables:
;; eval: (outline-minor-mode)
;; End:
