;;; init.el --- Personal Config Setup -*- lexical-binding: t; no-byte-compile: t -*-

;; custom always seems to cause more problems than it solves
(setq custom-file (make-temp-file ""))

(with-eval-after-load 'bytecomp
  (delete 'docstrings-non-ascii-quotes byte-compile-warning-types))
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; (electric-pair-mode)
;; (repeat-mode)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; (recentf-mode)

(setq use-package-enable-imenu-support t)
(setq use-package-always-ensure t)
;; We delay startup of package.el because there was something weird going on,
;; possibly related to chemacs but who knows
;; (message "we've set package options and are ready to initialize")
;; (package-initialize t)
(package-initialize)
;; (message "we've now initialized, in theory")



(use-package emacs
  :init
  (setq-default fill-column 80)
  (setq completion-styles '(flex))
  (setq use-short-answers t)
  :config
  (repeat-mode)
  (recentf-mode)
  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . electric-pair-mode)
  )



;;; Install and configure packages

;; (use-package mode-line-bell
;;   :config
;;   (mode-line-bell-mode))
(use-package pulsar
  :config
  (setq ring-bell-function 'pulsar-pulse-line-red))


(use-package which-key
  :ensure
  :config
  (which-key-mode))

(use-package editorconfig
  :config
  (editorconfig-mode))
(use-package consult
  :init
  (setq consult-preview-key "M-.")
  (setq consult-narrow-key "<")
  :bind
  ("C-x b" . 'consult-buffer)
  ("C-x 4 b" . 'consult-buffer-other-window)
  ("C-x 5 b" . 'consult-buffer-other-frame)
  )
(use-package vertico
  :config
  (vertico-mode))
(use-package marginalia
  :config (marginalia-mode))
(use-package orderless
  :config
  (setq orderless-matching-styles '(orderless-regexp)
	;; orderless-style-dispatchers '(without-if-bang flex-if-twiddle)
	completion-styles '(orderless)
	)
  )
(use-package crux
  :bind
  ("C-k" . 'crux-smart-kill-line))
(use-package fontaine
  :init
  (setq fontaine-latest-state-file
	(locate-user-emacs-file "fontaine-latest-state.eld"))
  (setq fontaine-presets
	'((jetbrains
	   :default-family "JetBrains Mono"
	   :default-weight light)
	  (t
	   :default-height 140)))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'jetbrains)))

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	)
  (setq modus-themes-common-palette-overrides
	'((border-mode-line-active unspecified)
	  (border-mode-line-inactive unspecified)
	  (comment fg-dim))
	modus-vivendi-tinted-palette-overrides
	'((string cyan))
	modus-operandi-tinted-palette-overrides
	'((string green)))
  ;; (load-theme 'modus-vivendi-tinted :no-confirm)
  :config
  (load-theme 'modus-operandi-tinted :no-confirm)
  )




;; (use-package kaolin-themes)
;; ^^ so many errors!

(use-package imenu-list
  ;; :init
  ;; (add-to-list 'imenu-generic-expression
  ;;            '("Used Packages"
  ;;              "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))
  ;; TODO bind imenu-list-smart-toggle
  )

(setq imenu-sort-function 'imenu--sort-by-name) 
