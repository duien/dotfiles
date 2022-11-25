;;; early-init.el -*- lexical-binding: t; no-byte-compile: t -*-

(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (push '(internal-border-width . 12) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize nil)

;; Disable GUI elements
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)
