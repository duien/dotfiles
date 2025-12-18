;; -*- lexical-binding: t -*-

;;; Initialize package management

(add-to-list 'load-path (concat user-emacs-directory
                                (convert-standard-filename "lisp/")))

(setq use-package-enable-imenu-support t) ;; Must be set before use-package is loaded

(load "elpaca-init")

(elpaca elpaca-use-package
  ;; Install use-package support for all subsequent package configurations
  (elpaca-use-package-mode))

;;; Configure a sane environment

(use-package emacs
  ;; Set basic variables that should happen early in the process and/or aren't
  ;; specifically associated with some other configuration group
  :ensure nil
  :config
  (prefer-coding-system 'utf-8)
  (setq org-directory "~/Org/") ;; set up very early for other dependencies
  :custom
  (custom-file (make-temp-file "emacs-custom-") "Prevent configuration ghosts originating from customize")
  (user-full-name "Emily Hyland")
  (user-mail-address "hello@duien.com")
  (fill-column 80 "This may not work here, but it seems like it does?")
  (sentence-end-double-space nil)
  (vc-follow-symlinks t)
  (dired-use-ls-dired nil)
  (use-short-answers t)
  (bookmark-set-fringe-mark nil)
  (frame-inhibit-implied-resize t)
  (inhibit-startup-screen t)
  (face-near-same-color-threshold 0 "Prevent weird issuse from `distant-foreground' color specifications")
  (custom-theme-directory (concat user-emacs-directory (convert-standard-filename "themes/")))
  (safe-local-variable-directories `(,(expand-file-name custom-theme-directory)))
  (custom-safe-themes t))

(use-package mode-line-bell
  ;; Please do not beep every time I hit escape
  :ensure t
  :config
  (mode-line-bell-mode))

(use-package no-littering
  ;; Have packages put files in reasonable places
  ;; I don't think waiting is necessary, but seems safer?
  :ensure (:wait t)
  :config
  (message "loaded no-littering")
  (no-littering-theme-backups))

;;; UI BASICS

(use-package emacs ; contextual menu bar
  ;; Display menu-bar-mode only in GUI frames
  ;; From https://emacs.stackexchange.com/a/29443/6853
  :ensure nil
  :preface
  (defun eh/contextual-menubar (&optional frame)
    "Display the menubar in FRAME (default: selected frame) if on a
    graphical display, but hide it if in terminal."
    (interactive)
    (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))
  :hook
  (after-make-frame . 'eh/contextual-menubar))

;;;; Wrap/scale/scroll

(use-package emacs
  ;; Get rid of weird scaling shortcuts
  :ensure nil
  :config
  (global-unset-key (kbd "<pinch>"))
  (global-unset-key (kbd "C-<wheel-up>"))
  (global-unset-key (kbd "C-<wheel-down>")))

(use-package emacs
  ;; Make the mouse wheel work the way I want
  :ensure nil
  :custom
  (mouse-wheel-flip-direction t)
  (mouse-wheel-tilt-scroll t))

;;;; Editing utilities

(use-package delsel
  :ensure nil
  :config (delete-selection-mode))

(use-package imenu
  :ensure nil
  :config (setq imenu-flatten 'annotation))

(use-package repeat
  :ensure nil
  :config (repeat-mode))

(use-package crux
  :ensure t
  :bind
  ([remap move-beginning-of-line] . 'crux-move-beginning-of-line)
  ([remap kill-line] . 'crux-smart-kill-line)
  ([remap open-line] . 'crux-smart-open-line)
  ("C-x w s" . 'crux-swap-windows))

(use-package emacs
  ;; Set up soft-wrapping and don't let horizontal scroll interfere
  :ensure nil
  :preface
  (defun eh/disable-horiz-scroll-with-visual-line ()
    (setq-local mouse-wheel-tilt-scroll (not visual-line-mode)))
  :custom
  (word-wrap t)
  (truncate-lines t)
  ;; (comment-auto-fill-only-comments t)
  :hook
  (visual-line-mode . eh/disable-horiz-scroll-with-visual-line))

;;;;; Themes and appearance

(load "theme-utils")

(use-package emacs ;; keep title bar visible in light or dark themes
  ;; TODO Figure out a way to reset on theme disable?
  :ensure nil
  :preface
  (defun eh/set-ns-appearance-from-theme (theme)
    (let ((appearance (frame-parameter nil 'background-mode)))
      (modify-all-frames-parameters `((ns-appearance . ,appearance)))))
  :hook
  (enable-theme-functions . eh/set-ns-appearance-from-theme))

;; Load mood-line before enabling a theme to prevent weirdness?
(use-package mood-line
  :ensure t
  :preface
  (defun eh/mood-line-segment-major-mode ()
    "Displays the current major mode in the mode-line."
    (concat (format-mode-line minions-mode-line-modes 'mood-line-major-mode) ""))
  (defun eh/mood-line-segment-cursor-position ()
    "Displays the line and column position of the cursor"
    (format-mode-line (car mode-line-position-column-line-format) 'mood-line-unimportant))
  :config
  (setq mood-line-format
        (mood-line-defformat
         :left ((propertize " " 'display '(raise 0.2))
                ((mood-line-segment-modal) . " ")
                ((mood-line-segment-buffer-status) . " ")
                ((mood-line-segment-buffer-name) . " ")
                ((eh/mood-line-segment-cursor-position) . " ")
                (propertize " " 'display '(raise -0.2)))
         :right (((mood-line-segment-vc) . " ")
                 (eh/mood-line-segment-major-mode)
                 (mood-line-segment-misc-info))))
  (mood-line-mode))


(use-package fontaine
  :ensure t
  :demand t
  :init
  (setq fontaine-presets
        ;; these can have some issues in 30 if some weights are disabled, but
        ;; it also seems to do better with actually loading the intended weights
        '((source   :default-family "Source Code Pro")
          (vctr     :default-family "VCTR Mono"
                    ;; :default-height 160
                    )
          (input    :default-family "Input Mono Narrow"
                    :default-weight light)
          (comic    :default-family "Comic Code Ligatures"
                    ;; :default-height 130
                    )
          (codelia  :default-family "Codelia Ligatures"
                    ;; :default-height 140
                    )
          (belinsky :default-family "Belinsky Text"
                    ;; :default-height 140
                    :default-weight regular)
          (pragmata :default-family "PragmataPro VF Liga"
                    :default-weight extralight)
          (jet      :default-family "Jetbrains Mono")
          (plex     :default-family "IBM Plex Mono"
                    :default-weight normal
                    :variable-pitch-family "iA Writer Quattro V")
          (sf       :default-family "SF Mono"
                    :variable-pitch-family "SF Pro Text")
          (berkeley :default-family "Berkeley Mono"
                    :default-weight light)
          (md-io    :default-family "MD IO Trial"
                    :default-weight light)
          (degular  :default-family "Degular Mono"
                    :default-weight light)
          (t
           ;; :default-height 150
           :default-height 160
           :default-weight light)))
  :config
  ;; (fontaine-set-preset (fontaine-restore-latest-preset))
  (eh/fontaine-apply-current-preset)
  :preface
  (defun eh/fontaine-apply-current-preset (&optional _theme)
    (fontaine-set-preset (fontaine-restore-latest-preset)))
  :hook
  (fontaine-set-preset . fontaine-store-latest-preset)
  (enable-theme-functions . eh/fontaine-apply-current-preset)
  )

;; temporarily disabled because of weird fontaine incompatibility
(use-package spacious-padding :disabled t :after (fontaine) :ensure t)

(use-package modus-themes
  :ensure t
  :init
  (add-to-list 'linked-themes '(modus-operandi-tinted . user-modus-operandi-tinted))
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t)
  (setq modus-themes-common-palette-overrides
        '((comment fg-dim)
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          ))
  (setq modus-operandi-tinted-palette-overrides
        '((string green)
          (type green-faint)
          (bg-mode-line-active bg-blue-intense)))
  (setq modus-operandi-palette-overrides
      `((bg-mode-line-active bg-blue-intense)
        (fg-mode-line-active fg-main)
        (string green)
        (comment fg-dim)))
  (setq modus-vivendi-tinted-palette-overrides
        '((string cyan)))
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  :config
  (modus-themes-include-derivatives-mode t)
  (load-theme 'modus-operandi-tinted t)
  )

(use-package ef-themes :ensure t
  :init
  (setq ef-dream-palette-overrides
        '((string green-faint)
          (type red-faint)))
  (setq ef-reverie-palette-overrides
        '((string green-faint)
          (type red-faint)))
  (add-to-list 'linked-themes '(ef-dream . user-ef))
  (add-to-list 'linked-themes '(ef-reverie . user-ef))
  (add-to-list 'linked-themes '(ef-reverie . user-ef-reverie)))

(use-package isohedron-theme
  :ensure (:host github :repo "duien/isohedron-theme")
  :init
  (add-to-list 'linked-themes '(isohedron . user-isohedron))
  ;; :config (load-theme 'isohedron t)
  )
(use-package caves-of-qud-theme
  :ensure (:host github :repo "duien/caves-of-qud-theme")
  :init
  (add-to-list 'linked-themes '(caves-of-qud . user-qud)))

(use-package doric-themes :ensure t
  :init
  (add-to-list 'linked-themes '(doric-beach . user-doric)))

(use-package gruvbox-theme :ensure t
  :init
  (setq gruvbox-bold-constructs t)
  (add-to-list 'linked-themes '(gruvbox-dark-medium . user-gruvbox-dark)))

;; actually set a theme on startup

;; (load-theme 'isohedron t)
;; TODO give this a memory like fontaine (and set up other niceties like the
;; enable/disable, user overrides, and menu bar color)

;;; UNSORTED PAST HERE

(use-package persistent-scratch
  :ensure t
  :init
  ;; only allow killing scratch when killing all of emacs
  (with-current-buffer "*scratch*"
    (setq default-directory "~/") ;; not related, but experimenting
    (emacs-lock-mode 'kill))
  :config
  (persistent-scratch-setup-default)
  (with-current-buffer "*scratch*"
    (persistent-scratch-mode 1)))

(use-package which-key
  :ensure nil
  :config
  (which-key-mode))

;; tame indentation
(use-package editorconfig
  :ensure nil
  :hook
  (prog-mode . editorconfig-mode))

(use-package emacs
  :ensure nil
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq standard-indent 2)
  (setq js-indent-level 2)
  (setq css-indent-offset 2)
  (setq tab-always-indent t
	      require-final-newline t))

;; show line numbers in programming modes
(use-package display-line-numbers
  :ensure nil
  :init
  (setq-default display-line-numbers-widen t
                display-line-numbers-width 3)
  :hook prog-mode)

;; show fill column in programming modes
(use-package display-fill-column-indicator
  :ensure nil
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  )


;; show tabs
(use-package tab-bar
  :ensure nil
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
  ("C-x t n" . tab-bar-new-tab))

(use-package minions
  :ensure t
  :init
  (setq mode-line-position-column-line-format '(" +%l:%c"))
  (setq minions-mode-line-lighter "#"
        minions-mode-line-delimiters '("" . ""))
  :config
  (minions-mode))

;; Complete bits of words in any order
(use-package orderless
  :ensure t
  :config
  ;; previously I've been using just orderless-regexp
  (setq orderless-matching-styles '(orderless-regexp))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Show a vertical list of completion candidates immediately
(use-package vertico
  :ensure t
  :config (vertico-mode))

(use-package consult
  :ensure t
  :init
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  :config
  (consult-customize consult-line
                     :preview-key
                     '(:debounce 0.5 "<up>" "<down>"
                                 :debounce 1 any))
  (consult-customize consult-theme :preview-key nil)
  :bind
  ("C-x b" . 'consult-buffer)
  ("C-x 4 b" . 'consult-buffer-other-window)
  ("C-x 5 b" . 'consult-buffer-other-frame)
  ("C-x i" . 'consult-imenu)
  ("C-h t" . 'consult-theme)
  ("C-h i" . 'consult-info))

;; Skipping marginalia for now

;; Skipping linked thetmes and ns-appearance-change hooks

;; Keep track of recently opened files
(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude
	      `(,(expand-file-name "elpaca/builds/" user-emacs-directory)
          ,(expand-file-name "eln-cache/" user-emacs-directory)
          ,(expand-file-name "etc/" user-emacs-directory)
          ,(expand-file-name "var/" user-emacs-directory)
          ,(expand-file-name "init.org" org-directory)))
  :config (recentf-mode))

;; Allow undoing and redoing window layout changes
(use-package winner
  :ensure nil
  :bind ("C-x w <left>" . 'winner-undo)
  :bind ("C-x w <right>" . 'winner-redo)
  :config (winner-mode))

;; don't save white-space, but allow it to persist in the buffer after saving
;; TODO something is wrong here that sometimes prevents cleanup
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

(use-package project :demand t) ;; load project first
(use-package projectile
  :after (project) ;; so we ensure projectile is after it
  :ensure t
  :demand t
  :init
  (setq projectile-auto-discover t)
  (setq projectile-track-known-projects-automatically nil)
  ;; (setq projectile-globally-ignored-buffers )
  :bind (:map projectile-mode-map
              ("C-x p" . projectile-command-map)
              ("C-x p b" . consult-project-buffer))
  :config
  (projectile-mode))
(use-package consult-projectile
  :ensure t
  :demand t
  :after (consult projectile)
  :bind (:map projectile-mode-map
              ("C-x p p" . consult-projectile)))

(use-package ag
  :ensure t)

(use-package ligature
  :ensure t
  :config (ligature-set-ligatures
           t ;; all modes
           '("!!" "!=" "!==" "!!!" "!≡" "!≡≡" "!>" "!=<" "#("
     "#_" "#{" "#?" "#>" "##" "###" "#_(" "%=" "%>" "%>%" "%<%"
     "&%" "&&" "&*" "&+" "&-" "&/" "&=" "&&&" "&>" "$>"
     ;; "***"
     "*=" "*/" "*>" "++" "+++" "+=" "+>" "++=" "--"
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
     "~>>" "[[" "]]" "\">" "_|_"))
  (global-ligature-mode))

;; Use a readable, centered width and soft wrap for text-heavy modes
(use-package olivetti
  :ensure t
  :init
  (setq olivetti-style nil)
  :hook
  org-mode
  gfm-mode)

;; Best git interface
(use-package transient :ensure t)
(use-package magit :ensure t)

;; Show git status in the fringe
(use-package diff-hl
  :after (magit)
  :ensure t
  :demand t
  :init
  (setq diff-hl-draw-borders nil)
  :config
  (global-diff-hl-mode)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  (fontaine-set-preset . diff-hl-maybe-redefine-bitmaps))

;; Various small language modes that don't need config
(use-package fish-mode :ensure t :init (setq fish-indent-offset 2))
(use-package haml-mode :ensure t)
(use-package sass-mode :ensure t)
(use-package json-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package rspec-mode :ensure t) ;; this likely needs some config
(use-package lua-mode :ensure t :init (setq lua-indent-level 2))
(use-package swift-mode :ensure t :init (setq swift-mode:basic-offset 2))

;; Setting up rust
(use-package rust-mode :ensure t)
;; TODO These are no longer found for some reason?
;; (use-package lsp-mode :ensure t) ;; rustic needs it
;; (use-package rustic :ensure t
;;   :after (rust-mode)
;;   :init
;;   (setq rust-mode-treesitter-derive t))


(use-package markdown-mode
  :ensure t
    :mode
    (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode)))

(use-package ruby-mode
  :ensure nil
  :init
  (setq ruby-method-call-indent nil)
  :config
  (if (treesit-language-available-p 'ruby)
      (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))))

(use-package treesit-fold
  :ensure t)
;; enh-ruby-mode does some things better, but hoping ts will become configurable
;; enough to not need it

;; skipping elixir because I know ts situation changed

;; skip org for now because that need so much config! and that config needs
;; refactored badly

(use-package org
  :ensure nil
  :after (fontaine)
  :demand t
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
  :ensure t
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


(use-package outline
  :ensure nil
  :init
  (setq outline-blank-line t
        outline-minor-mode-use-buttons 'in-margins))

;; Some additional window management
(use-package emacs
  :ensure nil
  :preface
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
  :bind
  ("C-x w r" . 'eh/toggle-window-split)
  ("C-x w f" . 'toggle-frame-fullscreen))

;; TODO is there a ts-aware version of this?
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . 'er/expand-region))

;; allow highlighting hex codes without getting color names
(use-package rainbow-mode
  :ensure t
  :init
  (setq rainbow-ansi-colors nil
        rainbow-x-colors nil))

;; auto-close parens and such
(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode))

(use-package vterm
  :ensure t)

;; some other skipped things:
;; elpher, combobulate, eglot, company

;; don't silently delete buffers I've created and entered text in
;; (affects only quitting, not manually killing the buffer)
(use-package emacs
  :ensure nil
  :preface
  (defun eh/set-offer-save-in-created-buffers ()
    (unless (or buffer-file-name
                (string-match-p "^magit" (buffer-name))
                (string-match-p "^ " (buffer-name))
                (string-match-p "^*" (buffer-name)))
      (setq buffer-offer-save t)))
  :hook
  (first-change . eh/set-offer-save-in-created-buffers))

;; ;; highlight todo comments in prog modes
;; ;; TODO Default formatting until org is set up
(use-package hl-todo
  :ensure t
  :init
  (setq hl-todo-keyword-faces
        '(("TODO" . eh/org-keyword-todo)
          ("FIXME" . eh/org-keyword-halt)
          ("NOTE" . eh/org-keyword-read)
          ("HACK" . eh/org-keyword-idea)))
  :hook prog-mode)

;; bind this to something
(defun eh/kill-project-path-or-buffer-name ()
  "Add to the kill-ring the project-relative path of the current file, the full path of the current file, or the name of the current buffer"
  (interactive)
  (let* ((buffer-file (buffer-file-name))
         (project-dir (projectile-project-p buffer-file))
         (path (if (and buffer-file project-dir)
                   (file-relative-name buffer-file project-dir)
                 buffer-file)))
    (kill-new (or path (buffer-name)))))

;;; FINALLY

;; ensure a server is started (for opening from dock)
(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (server-start)))


;;; IDEAS
;; completion annotation buffer file path (project path?)
;; use `:custom' keyword instead of `setq' in `:config'
;;     (variable-name value "optional description string")

;; Local Variables:
;; eval: (outline-minor-mode)
;; End:
