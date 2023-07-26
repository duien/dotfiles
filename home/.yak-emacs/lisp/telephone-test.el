(use-package telephone-line)

;; while testing: disable before configuring
(telephone-line-mode -1)

;; these faces somehow make the whole segment not display...
;; got them working now, not sure what changed
(defface eh/telephone-line-status-locked
  '((t :background "red"))
  "face for a read-only file-visiting buffer")
(defface eh/telephone-line-status-normal
  '((t :background "gray65"))
  "face for a buffer without anything special")
(defface eh/telephone-line-status-unsaved
  '((t :background "gold"))
  "face for a buffer that has changes")

(defun eh/buffer-status ()
  "return cons cell with symbol and status type"
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    ;; either the base buffer of an indirect buffer, or the current buffer
    ;; (let ((read-only buffer-read-only)
    ;;       (file-modified (and buffer-file-name (buffer-modified-p)))
    ;;       (modified (buffer-modified-p)))
    ;;   (prin1-to-string `(,read-only ,file-modified ,modified))
    ;;   " ✗ ✓ *"
    (cond (buffer-file-name ;; for file-visiting buffers
           (cond (buffer-read-only    '("×" . locked))
                 ((buffer-modified-p) '("*" . unsaved))
                 (t                   '("=" . normal))))
           ;; (cond (buffer-read-only    '("✗" . locked))
           ;;       ((buffer-modified-p) '("*" . unsaved))
           ;;       (t                   '("✓" . normal))))
          (t ;; for non-file-visiting
           (cond (buffer-read-only '("-" . normal))
                 (persistent-scratch-mode '("∞" . normal))
                 ;; TODO customize (shows as !)
                 ;; TODO dired (shows as - but would be cool if marks were incorporated)
                 (t '("!" . unsaved)))))))

(telephone-line-defsegment eh/buffer-status-segment ()
  (car (eh/buffer-status)))
(telephone-line-defsegment eh/bare-line-number ()
  "%l")
  ;; "%l")

(telephone-line-defsegment eh/lock-status-segment ()
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (cond (emacs-lock-mode "")
          ;; ("")
          )))

;; TODO This (and similar telephone line built-in) show nothing for elisp
(telephone-line-defsegment eh/simple-mode-segment ()
  "(%m)")

(telephone-line-defsegment eh/vc-branch ()
  (if vc-mode
      ;; this is a hack -- remove the "Git-" at beginning with substring
      ;; (concat "#" (substring-no-properties vc-mode 5))
      (concat " " (substring-no-properties vc-mode 5))
    nil))

(defun telephone-line-status-face (active)
  "Function used to determine which face to use for the buffer status segment"
  (cond ((not active) 'mode-line-inactive)
        (t
         (alist-get (cdr (eh/buffer-status))
                    '((locked  . eh/telephone-line-status-locked)
                      (normal  . eh/telephone-line-status-normal)
                      (unsaved . eh/telephone-line-status-unsaved))
                    'telephone-line-accent-active))))

(setq telephone-line-faces
      '((status . telephone-line-status-face)
        (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
        (nil    . (mode-line . mode-line-inactive))))

(setq telephone-line-primary-left-separator 'telephone-line-flat
      telephone-line-secondary-left-separator 'telephone-line-flat
      telephone-line-primary-right-separator 'telephone-line-flat
      telephone-line-secondary-right-separator 'telephone-line-flat)

(setq telephone-line-lhs
      '((status . (eh/buffer-status-segment))
        (nil    . (eh/lock-status-segment))
        (nil    . (telephone-line-buffer-name-segment))
        ;; (nil    . (telephone-line-vc-segment))
        (nil . (eh/vc-branch))
        )
      telephone-line-rhs
      '((nil . (telephone-line-simple-major-mode-segment))
        (nil . (eh/bare-line-number))))
(setq telephone-line-height 22) ;; needs to change with font size somehow

;; while testing: renable with new config
(telephone-line-mode 1)

;; what should the conditions be for the status segment?
;; - special handling for some modes
;;   (help/helpful, info, dired, persistent-scratch, imenu)
;; - for FILE VISITING
;;   - read-only OR modified OR read-write
;; - for NON-FILE-VISITING
;;   (this mostly overlaps the special ones, but there are potential exceptions)
;;   read-only (static?) OR unsaved (?)

;; show infinity for persistent scratch ∞

;; other things to show
;; - minions
;; - dedicated window
;; - lock-mode buffer
