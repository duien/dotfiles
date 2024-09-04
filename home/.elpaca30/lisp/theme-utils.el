;; A package to collect my theme utilities for use across configs
;; NOTE this is not actually a package at this point

;; Set up a variable and hook to automatically load a theme after another theme
;; is enabled (this allows for more robust customizations to themes than what
;; can be done with a second call to `custom-theme-set-faces')

(defvar linked-themes '() "Alist of theme to another theme to load automatically after it.")
(defun eh/load-linked-user-theme (theme)
  "Check `linked-themes' for THEME and load the associated theme if present."
  (let ((theme-to-load (cdr (assoc theme linked-themes))))
    (if theme-to-load
        (load-theme theme-to-load))))
(add-hook 'enable-theme-functions 'eh/load-linked-user-theme)

;; Also set automatic appearance-switching so that the title bar text stays visible.
;; This runs an excessive number of times, but that doesn't really seem to be a problem
(defun eh/set-ns-appearance-from-theme (theme)
  (let ((appearance (frame-parameter nil 'background-mode)))
    (modify-all-frames-parameters `((ns-appearance . ,appearance)))))
(add-hook 'enable-theme-functions 'eh/set-ns-appearance-from-theme)
(add-hook 'ns-appearance-change-functions 'eh/set-ns-appearance-from-theme)
