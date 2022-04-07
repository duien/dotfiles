
;;; Bootstrap Straight
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

;; Install packages for packages
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


(set-face-attribute 'default nil :family "IBM Plex Mono" :height 160)
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      use-dialog-box nil
      pop-up-windows nil
      fill-column 80)
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(use-package nano-theme
  :straight
  '(nano-theme :type git :host github :repo "rougier/nano-theme")
  :config
  (nano-light)
  )
(use-package nano-modeline
  :straight
  '(nano-modeline :type git :host github :repo "rougier/nano-modeline")
  :config
  (nano-modeline-mode))
(use-package nano-splash
  :straight
  '(nano-splash :type git :host github :repo "rougier/nano-splash")
  :config
  (nano-splash))

(use-package vertico
  :init
  (vertico-mode))
(use-package orderless
  :init
  (setq orderless-matching-styles '(orderless-regexp))
(use-package marginalia)
(use-package mini-frame)
(use-package consult
  :bind
  ("C-x b" . 'consult-buffer))

;; this does not require its own dependencies. should be
;; added after vertico, marginalia, and mini-frame
(use-package nano-minibuffer
  :straight
  '(nano-minibuffer :type git :repo "https://gist.github.com/126e358464e12aa28fac5b4f3dd5eb9c.git"
		    :files ("nano-minibuffer.el"))
  :init
  ;; fix up problems
  (defface nano-face-faded
    '((t (:inherit 'nano-faded)))
    "This face has two names")
  :config
  (minibuffer-setup)
  )

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

;; Set variables about frames
(setq window-divider-default-right-width 12
      window-divider-default-bottom-width 1
      window-divider-default-places 'right-only)
(window-divider-mode 1)
(setq-default  default-frame-alist
	       (append (list
			'(internal-border-width . 12)
			'(left-fringe . 0)
			'(right-fringe . 0)
			'(tool-bar-lines . 0)
			'(menu-bar-lines . 0)
			'(vertical-scroll-bars . nil))))
(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)
(add-hook 'before-make-frame-hook #'window-divider-mode)
;; Set up fancy frame stuff
;; (use-package frame
;;   :straight '(frame :type built-in)
;;     :custom
;;   (window-divider-default-right-width 12)
;;   (window-divider-default-bottom-width 1)
;;   (window-divider-default-places 'right-only)
;;   (window-divider-mode t)
;;   :config
;;   (setq-default default-frame-alist
;;                 (append (list
;;                 '(internal-border-width . 12)
;;                 '(left-fringe    . 0)
;;                 '(right-fringe   . 0)
;;                 '(tool-bar-lines . 0)
;;                 '(menu-bar-lines . 0)
;;                 '(vertical-scroll-bars . nil))))
;;   (setq-default window-resize-pixelwise t)
;;   (setq-default frame-resize-pixelwise t)
;;   :hook
;;   (before-make-frame . 'window-divider-mode))


;; (use-package vertico
;;   :init
;;   (vertico-mode 1))

;; (use-package consult
;;   :config
  
;;   :bind
;;   ("C-x b" . consult-buffer))

;; (use-package orderless
;;   :init
;;   (setq completion-styles '(orderless basic))
;;   )

;; (use-package bespoke-theme
;;   :straight '(bespoke-theme :host github :repo "mclear-tools/bespoke-themes" :branch "main"))

;; (use-package marginalia)
;; (use-package mini-frame)
;; (use-package nano-emacs
;;   :straight
;;   '(nano-emacs :type git :host github :repo "rougier/nano-emacs")
;;   :init
;;   (setq nano-font-size 16)
;;   (setq nano-font-family-monospaced "IBM Plex Mono")
;;   (setq nano-font-family-proportional "iA Writer Quattro V")
;;   (require 'nano-layout)
;;   (require 'nano-faces)
;;   (nano-faces)
;;   (require 'nano-theme)
;;   (nano-theme)

;;   ;; hack to reset fonts from nano opinions
;;   ;; (set-face-attribute 'default nil :weight 'light)
;;   ;; (set-face-attribute'nano-face-salient nil :weight 'normal) 
;;   ;; (set-face-attribute 'nano-face-strong nil :weight 'bold)
;;   (set-face-attribute 'font-lock-comment-face nil :slant 'italic)

;;   (require 'nano-colors) ;; for minibuffer?
;;   (require 'nano-defaults)
;;   (require 'nano-session)
;;   (require 'nano-modeline)
;;   (require 'nano-writer) ;; org?
;;   ;; (require 'nano-bindings)
;;   (require 'nano-splash)
;;   (require 'nano-minibuffer)
;;   )


;; ;; (add-to-list 'load-path (concat user-emacs-directory "nano"))


;; ;; (use-package vertico
;; ;;   :init
;; ;;   (vertico-mode)
;; ;; (require 'nano-minibuffer)
;; ;; (require 'nano-command)
;; ;; (require 'nano-agenda)
;; ;; (require 'nano-writer)

;; (setq custom-file (concat user-emacs-directory "custom.el"))
;; (load custom-file)
