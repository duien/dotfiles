;; NOTE: In order to work with `emacsserver' utility, the server-name must be
;; the same as the profile name
(("scratch" . ((user-emacs-directory . "~/.scratch-config")
               (server-name . "scratch")
               (straight-p . t)))
 ("doom" . ((user-emacs-directory . "~/.doom")
	          (env . (("DOOMDIR" . "~/.doom-config")))
            (server-name . "doom")
            (straight-p)
            ))
 ("minimal" . ((user-emacs-directory . "~/.minimal-config")
               (server-name . "minimal")))
 )
