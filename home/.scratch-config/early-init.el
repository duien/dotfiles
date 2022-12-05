;;; early-init.el -*- lexical-binding: t; no-byte-compile: t -*-

;; set modifiers immediately
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-control-modifier 'control))

;; basic frame properties
(tool-bar-mode -1)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; prevent loading package.el and prepare for straight
(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)
