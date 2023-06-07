;;; early-init.el -*- lexical-binding: t; no-byte-compile: t -*-

;; set modifiers immediately
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-control-modifier 'control))

;; (setq package-enable-at-startup nil)
;; (setq use-package-enable-imenu-support t)
;; (setq use-package-always-ensure t)

(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(width . 100) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)

(setq straight-repository-branch "develop")
