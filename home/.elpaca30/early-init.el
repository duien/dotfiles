(setq package-enable-at-startup nil)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)
  (setq mac-control-modifier 'control))

(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(width . 200) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
