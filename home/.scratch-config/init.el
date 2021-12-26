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

	;;; USING USE-PACKAGE

;; :init Code to run when `use-package' form evals.
;; :config Runs if and when package loads.


	;;; SANE DEFAULTS

(use-package emacs
	:init
	(setq inhibit-startup-screen t
				sentence-end-double-space nil
				)

	(setq user-full-name "Emily Hyland"
				user-mail-address "hello@duien.com")

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
	(setq-default indent-tabs-mode t)
	(setq-default tab-width 2)

	;; Enable indentation+completion using the TAB key.
	;; Completion is often bound to M-TAB.
	(setq tab-always-indent 'complete)

	;; Some evil stuff that needs to be set early
	(setq evil-want-integration t)
	(setq evil-want-keybinding nil)

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
		(mac-auto-operator-composition-mode)   ;; enables font ligatures
		(global-set-key [(s c)] 'kill-ring-save)
		(global-set-key [(s v)] 'yank)
		(global-set-key [(s x)] 'kill-region)
		(global-set-key [(s q)] 'kill-emacs)
		)
	)

	;;; BEGIN THE MAIN SETUP

;; Priorities

;; completion
;; keymap

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
		"bz" 'bury-buffer

		"f" '(:ignore t :which-key "file")
		"fs" 'save-buffer

		"w" '(:ignore t :which-key "window")
		"wd" 'delete-window 
		"ww" 'other-window
		"wm" 'delete-other-windows
		"wv" 'evil-window-vsplit
		"ws" 'evil-window-split
		"wh" 'evil-window-left
		"wj" 'evil-window-down
		"wk" 'evil-window-up
		"wl" 'evil-window-right

		"q" '(:ignore t :which-key "quit")
		"qq" 'save-buffers-kill-terminal
		"qf" 'server-edit

		"h" (general-simulate-key "C-h")
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
	:init
	(setq completion-styles '(orderless)
				completion-category-defaults nil
				completion-category-overrides '((file (styles partial-completion)))))

(use-package toc-org
	:hook (org-mode . toc-org-mode))

(use-package marginalia
	:init
	(marginalia-mode))

;; `consult' replaces `completing-read' with a nice interface
;; that we can extend as we want
(use-package consult
	:general

	)
(setq evil-respect-visual-line-mode t)

;; when using visual-line-mode, wrap to the `fill-column'
(use-package window-margin
	:init
	(add-hook 'text-mode 'turn-on-window-margin-mode)
	)

(use-package org-auto-tangle
	:defer t
	:hook (org-mode . org-auto-tangle-mode))

(use-package solaire-mode
	:init
	(solaire-global-mode 1))

(use-package evil
	:config
	:init
	(evil-mode 1))
(use-package evil-commentary
	:init
	(evil-commentary-mode))
(use-package evil-collection
	:after evil
	:init
	(evil-collection-init))

(use-package minions
	:demand
	:config (minions-mode 1))

(use-package moody
	:demand
	:config
	(setq x-underline-at-descent-line t)
	(moody-replace-mode-line-buffer-identification)
	(moody-replace-vc-mode)
	(moody-replace-eldoc-minibuffer-message-function))


(use-package ligature
	:straight (ligature :host github :repo "mickeynp/ligature.el")

	:config
	;; Enable the "www" ligature in every possible major mode
	(ligature-set-ligatures 't '("www"))
	;; Enable all Cascadia Code ligatures in programming modes
	(ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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
	;; NOTE disabling a few problematic ligatures: *** for different reasons
	;; ;; ->> -<<&& ^= ~~~@ ~=~> ~- *> ** */ || |] |} |> {|
	;; Enables ligature checks globally in all buffers. You can also do it
	;; per mode with `ligature-mode'.
	:init
	;; for some reason, the global mode isn't defined until you run the local mode
	(ligature-mode) 
	(global-ligature-mode t)
	)

(use-package which-key
	:config
	;; this is the default
	(which-key-setup-side-window-bottom)
	:init
	(which-key-mode))

(use-package vertico
	:init
	(vertico-mode))

(use-package fish-mode)

;; VERSION CONTROL

(use-package magit)

;; ORG-MODE

(use-package org
	:config
	(setq org-fontify-whole-heading-line t)
	(setq org-src-preserve-indentation t)
	:hook
	(org-mode . (electric-indent-local-mode -1))
	(org-mode . visual-line-mode))

;; VISUALS AT THE END FOR SOME REASON

;; Set themes
(use-package modus-themes
	:ensure
	:init
	(setq modus-themes-italic-constructs t
				modus-themes-bold-constructs t
				modus-themes-subtle-line-numbers t
				modus-themes-intense-markup t
				modus-themes-fringes nil
				modus-themes-mode-line '(moody accented)
				modus-themes-syntax '(green-strings)
				modus-themes-org-blocks 'gray-background
				modus-themes-completions 'opinionated
				modus-themes-region '(bg-only accented)
				modus-themes-headings
				'((1 . (rainbow background))
					(t . (monochrome)))
				)
	(defun eh/modus-customize ()
		;; deal with git gutter faces? or maybe that's no longer an issue?
		) 

	;; load the theme files
	(modus-themes-load-themes)
	:hook (modus-themes-after-load-theme . eh/modus-customize)
	:config
	(modus-themes-load-operandi))

(setq display-line-numbers-major-tick 10)

;; Some basic appearance stuff
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1) ;; not sure about this one ... would like dynamic, maybe?

(set-face-attribute 'default nil :font "Cascadia Code" :weight 'semilight :height 150)
(set-face-attribute 'bold nil :weight 'semibold)

;; this is probably the stupid and wrong way to do this
;; (server-start)
