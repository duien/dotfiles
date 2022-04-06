
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

(use-package vertico
  :init
  (vertico-mode 1))

(use-package consult
  :config
  
  :bind
  ("C-x b" . consult-buffer))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  )

(use-package bespoke-theme
  :straight '(bespoke-theme :host github :repo "mclear-tools/bespoke-themes" :branch "main"))

(use-package marginalia)
(use-package mini-frame)
(use-package nano-emacs
  :straight
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs")
  :init
  (setq nano-font-size 16)
  (setq nano-font-family-monospaced "IBM Plex Mono")
  (setq nano-font-family-proportional "iA Writer Quattro V")
  (require 'nano-layout)
  (require 'nano-faces)
  (nano-faces)
  (require 'nano-theme)
  (nano-theme)

  ;; hack to reset fonts from nano opinions
  ;; (set-face-attribute 'default nil :weight 'light)
  ;; (set-face-attribute'nano-face-salient nil :weight 'normal) 
  ;; (set-face-attribute 'nano-face-strong nil :weight 'bold)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)

  (require 'nano-colors) ;; for minibuffer?
  (require 'nano-defaults)
  (require 'nano-session)
  (require 'nano-modeline)
  (require 'nano-writer) ;; org?
  ;; (require 'nano-bindings)
  (require 'nano-splash)
  (require 'nano-minibuffer)
  )


;; (add-to-list 'load-path (concat user-emacs-directory "nano"))


;; (use-package vertico
;;   :init
;;   (vertico-mode)
;; (require 'nano-minibuffer)
;; (require 'nano-command)
;; (require 'nano-agenda)
;; (require 'nano-writer)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
