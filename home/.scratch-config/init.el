;;; early-init.el -*- lexical-binding: t; no-byte-compile: t -*-

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(height . 50) default-frame-alist)
(push '(width . 100) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; DO NOT EDIT THIS FILE
;; This file is auto-generated from `README.org` so any changes made directly will be lost.

;; Lots of things stolen from https://www.lucacambiaghi.com/vanilla-emacs/readme.html

;; Bootstrap straight
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-recipes-gnu-elpa-use-mirror t)
(setq straight-check-for-modifications nil)
(setq use-package-always-defer t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(use-package emacs
  :init
  (setq inhibit-startup-screen t
        sentence-end-double-space nil
        )

  (setq user-full-name "Emily Hyland"
        user-mail-address "hello@duien.com")

  (set-face-attribute 'default nil :font "Cascadia Code" :weight 'semilight :height 150)
  (set-face-attribute 'bold nil :weight 'semibold)

  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  ;; always allow 'y' instead of 'yes'.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; default to utf-8 for all the things
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; write over selected text on input... like all modern editors do
  (delete-selection-mode t)

  ;; enable recent files mode.
  (recentf-mode t)
  (setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
                          ,(expand-file-name "eln-cache/" user-emacs-directory)
                          ,(expand-file-name "etc/" user-emacs-directory)
                          ,(expand-file-name "var/" user-emacs-directory)))

  ;; don't want ESC as a modifier
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Don't persist a custom file, this bites me more than it helps
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings

  ;; stop emacs from littering the file system with backup files
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  ;; follow symlinks 
  (setq vc-follow-symlinks t)

  ;; don't show any extra window chrome
  (when (window-system)
    (tool-bar-mode -1)
    (toggle-scroll-bar -1))

  ;; enable winner mode globally for undo/redo window layout changes
  (winner-mode t)

  (show-paren-mode t)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)


  ;; clean up the mode line
  (display-time-mode -1)
  (setq column-number-mode t)

  ;; use common convention for indentation by default
  (setq-default indent-tabs-mode nil) ;; never use tabs to indent 
  (setq-default tab-width 2)

  ;; Enable indentation+completion using the TAB key.
  ;; Completion is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Some evil stuff that needs to be set early
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

  ;; Enable horizontal scrolling
  (setq mouse-wheel-tilt-scroll t)

  )

;;; MAC THINGS

(use-package emacs
  :init
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    (setq mac-control-modifier 'control)
    )
  ;; when on emacs-mac 
  (when (fboundp 'mac-auto-operator-composition-mode)
    ;; disable * ligatures for org, enable w for www
    (setq mac-auto-operator-composition-characters "!\"#$%&'()+,-./:;<=>?@[\\]^_`{|}~w")
    (mac-auto-operator-composition-mode)   ;; enables font ligatures
    (global-set-key [(s c)] 'kill-ring-save)
    (global-set-key [(s v)] 'yank)
    (global-set-key [(s x)] 'kill-region)
    (global-set-key [(s q)] 'kill-emacs)
    )
  )

(defvar after-enable-theme-hook nil
   "Normal hook run after enabling a theme.")

(defun run-after-enable-theme-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'run-after-enable-theme-hook)
;; (defun eh/set-face-basics ()
;;   (set-face-attribute 'default nil :font "Cascadia Code" :weight 'semilight :height 150)
;;   (set-face-attribute 'bold nil :weight 'semibold)
;;   )
;; (eh/set-face-basics) ;; do it right now too
;; (use-package emacs
;;   :hook
;;   (after-enable-theme . eh/set-face-basics))

(use-package general
  :demand t
  :config
  (general-evil-setup)
  (general-create-definer eh/global-leader
    :states '(normal movement)
    :prefix "SPC")
  (eh/global-leader
    "SPC" 'consult-buffer
    "`" '((lambda () (interactive) (switch-to-buffer (other-buffer (current-buffer) 1))) :which-key "prev buffer")
    "<escape>" 'keyboard-escape-quit
    ":" '(execute-extended-command :which-key "execute command")
    ";" '(eval-expression :which-key "eval sexp")

    "b" '(:ignore t :which-key "buffer")
    "bd" 'kill-this-buffer
    "bz" 'bury-buffer

    "f" '(:ignore t :which-key "file")
    "fs" 'save-buffer
    "ff" 'project-find-file

    "w" '(:ignore t :which-key "window")
    "wd" 'delete-window 
    "ww" 'other-window
    "wm" '(:ignore t :which-key "maximize")
    "wmm" 'delete-other-windows
    "wmv" 'delete-other-windows-vertically
    ;; "wmh" '

    "q" '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal
    "qf" 'server-edit

    "t" '(:ignore t :which-key "toggle")
    "tl" 'display-line-numbers-mode
    "tr" 'rainbow-mode

    ;; "h" (general-simulate-key "C-h")
    "h" '(:ignore t :which-key "help")
    "hv" 'describe-variable
    "hk" 'describe-key
    "hf" 'describe-function
    "hF" 'describe-face
    "ha" 'apropros-command
    "hd" 'apropros-documentation
    "hm" 'describe-mode
    "hp" 'describe-package
    "ht" 'consult-theme
   ) 
  )

(use-package vertico
  :general
  (eh/global-leader
    "ff" 'find-file)
  :init
  (vertico-mode)
  )

(use-package orderless
  :config
  (defun flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))
  :init
  (setq orderless-matching-styles '(orderless-regexp)
        orderless-style-dispatchers '(without-if-bang flex-if-twiddle))
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; `consult' replaces `completing-read' with a nice interface
;; that we can extend as we want
(use-package consult
  :general

  )

(use-package marginalia
  :init
  (marginalia-mode))

(use-package which-key
  :config
  ;; this is the default
  (which-key-setup-side-window-bottom)
  :init
  (which-key-mode))

;; when using visual-line-mode, wrap to the `fill-column'
;; (use-package window-margin
;;   :hook
;;   (text-mode . 'turn-on-window-margin-mode)
;;   ;; (org-mode . 'turn-on-window-margin-mode)
;;   )

(use-package moody
  :demand
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package solaire-mode
  :init
  (solaire-global-mode 1))

(use-package minions
  :config
  (setq minions-mode-line-lighter "≡")
  :init (minions-mode 1))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package projectile
  :config
  (setq projectile-project-search-path
        '(("~/Code" . 3)
        ("~/.homesick/repos" . 1)))
  :init
  (projectile-mode +1)
  :general
  (eh/global-leader
    "p" '(:keymap projectile-command-map :package projectile :which-key "project")
  )
)

(use-package evil
  :config
  ;; Put cursor in new window after split
  (setq evil-respect-visual-line-mode t)
  (setq evil-vsplit-window-right t
        evil-split-window-below t
        )
  :general
  (eh/global-leader
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    )
  :init
  (evil-mode 1))

(use-package evil-commentary
  :init
  (evil-commentary-mode))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))
(use-package evil-surround
  :after evil
  :init
  (global-evil-surround-mode 1))

(use-package magit
  :general
  (eh/global-leader
    "g" '(:ignore t :which-key "git")
    "gg" 'magit-status

    )
  )
(use-package diff-hl
  :config

  :hook
  ;; (diff-hl-mode . diff-hl-flydiff-mode) ;; causing indent flicker in org
  (magit-pre-refresh  . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :init (global-diff-hl-mode)
  )

(use-package elixir-mode)
(use-package alchemist)

(use-package fish-mode)
(use-package rainbow-mode)

(use-package org
  :config
  (setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Org/"
        org-log-done t
        org-log-into-drawer t
        org-insert-heading-respect-content t
        org-cycle-separator-lines 2 ;; 2 blank lines to keep when collapsed
        org-hide-leading-stars t
        org-fontify-whole-heading-line t
        org-fontify-todo-headline t
        org-fontify-done-headline t)
  (setq org-src-preserve-indentation t)
  (setq org-todo-keywords
        '((sequence "WAIT(w)" "FLAG(f)" "TODO(t)" "BLOK(b)" "HOLD(h)" "|" "DONE(d!)" "KILL(k@)")
          (sequence "QSTN(q)" "|" "  OK(o)" " YES(y)" "  NO(n)" "ANSR(a@)")
          (type "IDEA(I)" " YAK(Y)" "|")
          )
        )
  ;; define faces to use for all org todo keywords

  ;; completed states
  (defface eh/org-keyword-done '((t :inherit org-done)) "Face used for the DONE keyword in Org")
  (defface eh/org-keyword-kill '((t :inherit org-done)) "Face used for the KILL keyword in Org")
  (defface eh/org-keyword-answer '((t :inherit org-done)) "Face used for the ANSR keyword in Org")
  (defface eh/org-keyword-ok '((t :inherit eh/org-keyword-answer)) "Face used for the OK keyword in Org")
  (defface eh/org-keyword-yes '((t :inherit eh/org-keyword-done)) "Face used for the YES keyword in Org")
  (defface eh/org-keyword-no '((t :inherit eh/org-keyword-kill)) "Face used for the NO keyword in Org")

  ;; incomplete states

  (defface eh/org-keyword-wait '((t :inherit org-done)) "Face used for the WAIT keyword in Org")
  (defface eh/org-keyword-flag '((t :inherit org-todo)) "Face used for the FLAG keyword in Org")
  (defface eh/org-keyword-todo '((t :inherit org-todo)) "Face used for the TODO keyword in Org")
  (defface eh/org-keyword-block '((t :inherit org-todo)) "Face used for the BLOK keyword in Org")
  (defface eh/org-keyword-hold '((t :inherit org-todo)) "Face used for the HOLD keyword in Org")
  (defface eh/org-keyword-question '((t :inherit org-todo)) "Face used for the QSTN keyword in Org")
  (defface eh/org-keyword-idea '((t :inherit org-todo)) "Face used for the IDEA keyword in Org")
  (defface eh/org-keyword-yak '((t :inherit org-todo)) "Face used for the YAK keyword in Org")

  (setq org-todo-keyword-faces
        `(("TODO" . eh/org-keyword-todo)
          ("FLAG" . eh/org-keyword-flag)
          ("DONE" . eh/org-keyword-done)
          ("HOLD" . eh/org-keyword-hold)
          ("BLOK" . eh/org-keyword-block)
          ("WAIT" . eh/org-keyword-wait)
          ("KILL" . eh/org-keyword-kill)
          ("QSTN" . eh/org-keyword-question)
          ("ANSR" . eh/org-keyword-answer)
          ("  OK" . eh/org-keyword-ok)
          (" YES" . eh/org-keyword-yes)
          ("  NO" . eh/org-keyword-no)
          ("IDEA" . eh/org-keyword-idea)
          (" YAK" . eh/org-keyword-yak)
          ))

  (defun eh/org-update-theme ()
    (set-face-attribute 'org-done nil
                        :weight (face-attribute 'default :weight))
    (set-face-attribute 'org-headline-todo nil
                        :foreground 'unspecified
                        :inherit 'default)
    (set-face-attribute 'org-headline-done nil
                        :inherit '(font-lock-comment-face default))
    )
  (defun eh/org-update-modus-theme ()
    (set-face-attribute 'eh/org-keyword-todo nil
                        :inherit '(modus-themes-refine-green org-todo))
    (set-face-attribute 'eh/org-keyword-flag nil
                        :inherit '(modus-themes-intense-green org-todo))
    (set-face-attribute 'eh/org-keyword-hold nil
                        :inherit '(modus-themes-refine-yellow org-todo))
    (set-face-attribute 'eh/org-keyword-block nil
                        :inherit '(modus-themes-intense-red org-todo))
    (set-face-attribute 'eh/org-keyword-wait nil
                        :inherit '(modus-themes-intense-neutral org-done))
    (set-face-attribute 'eh/org-keyword-question nil
                        :inherit '(modus-themes-refine-blue org-todo))
    (set-face-attribute 'eh/org-keyword-idea nil
                        :inherit '(modus-themes-refine-cyan org-todo))
    (set-face-attribute 'eh/org-keyword-yak nil
                        :inherit '(modus-themes-refine-magenta org-todo))
    (set-face-attribute 'eh/org-keyword-done nil
                        :inherit '(modus-themes-nuanced-green org-done)
                        :foreground (modus-themes-color 'green-faint))
    (set-face-attribute 'eh/org-keyword-kill nil
                        :inherit '(modus-themes-nuanced-red org-done)
                        :foreground (modus-themes-color 'red-faint))
    (set-face-attribute 'eh/org-keyword-answer nil
                        :inherit '(modus-themes-nuanced-blue org-done)
                        :foreground (modus-themes-color 'blue-faint))
    (set-face-attribute 'eh/org-keyword-ok nil
                        :inherit 'eh/org-keyword-answer
                        :foreground (modus-themes-color 'blue))
    (set-face-attribute 'eh/org-keyword-yes nil
                        :inherit 'eh/org-keyword-done
                        :foreground (modus-themes-color 'green))
    (set-face-attribute 'eh/org-keyword-no nil
                        :inherit 'eh/org-keyword-kill
                        :foreground (modus-themes-color 'red))
    )

  :hook (org-mode . (lambda()
                      (org-indent-mode 1)
                      (electric-indent-local-mode -1)
                      (visual-line-mode 1)
                      ))
  (after-enable-theme . eh/org-update-theme)
  (modus-themes-after-load-theme . eh/org-update-modus-theme)
  )

(use-package org-superstar 
  :config
  (setq org-superstar-cycle-headline-bullets nil
        org-superstar-special-todo-items t
        ;; org-superstar-leading-bullet "·"
        org-superstar-headline-bullets-list '("◎" "•")) ;;◌
  (setq org-superstar-todo-bullet-alist
        '(("TODO"     . ?▢) ;;⭘
          ("FLAG"     . ?▶) ;;◍
          ("DONE"     . ?✓)
          ("WAIT"     . ?◷) ;;⏾
          ("BLOK"     . ?▲)
          ("HOLD"     . ?≈)
          ("KILL"     . ?×)
          ("QSTN"     . ?◇) 
          ("ANSR"     . ?⬥)
          ("  OK"     . ?·)
          (" YES"     . ?·)
          ("  NO"     . ?·)
          ("IDEA"     . ?◦)
          (" YAK"     . ?◦)
          )
        org-superstar-prettify-item-bullets nil
        )
  (set-face-attribute 'org-superstar-header-bullet nil :weight (face-attribute 'default :weight))
  :hook (org-mode . org-superstar-mode)
  )

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

;; VISUALS AT THE END FOR SOME REASON

;; Set themes
(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-subtle-line-numbers t
        modus-themes-intense-markup t
        modus-themes-fringes nil ;; background of fringe area
        modus-themes-mode-line '(moody accented)
        modus-themes-syntax '(green-strings)
        modus-themes-org-blocks 'gray-background
        modus-themes-completions 'opinionated
        modus-themes-region '(bg-only accented)
        ;; modus-themes-headings
        ;; '((1 . (bold))
        ;;   (t . ()))
        )
  (defun eh/modus-customize ()
    ;; deal with git gutter faces? or maybe that's no longer an issue?
    ) 

  ;; load the theme files
  (modus-themes-load-themes)
  :hook (modus-themes-after-load-theme . eh/modus-customize)
  :config
  (modus-themes-load-operandi))
