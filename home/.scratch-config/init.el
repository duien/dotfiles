;;; init.el --- Personal Config Setup -*- lexical-binding: t; no-byte-compile: t -*-

;; straight has been bootstrapped by chemacs
;; make sure we load the straight version of org when tangling
(straight-use-package 'org)
(straight-use-package 'use-package)
(let ()
  (setq org-confirm-babel-evaluate nil)
  (org-babel-load-file "~/.scratch-config/revision.org")
 ;; (org-babel-load-file "~/.scratch-config/README.org")
  )
