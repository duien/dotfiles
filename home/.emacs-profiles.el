;; NOTE: In order to work with `emacsserver' utility, the server-name must be
;; the same as the profile name, with `-server' appended
(("scratch" . ((user-emacs-directory . "~/.scratch-config")
               (server-name . "scratch-server")))
 ("doom" . ((user-emacs-directory . "~/.doom")
	          (env . (("DOOMDIR" . "~/.doom-config")))
            (server-name . "doom-server")
            (straight-p)
            ))
 ("minimal" . ((user-emacs-directory . "~/.minimal-config")
               (server-name . "minimal-server")))
 ("nano" . ((user-emacs-directory . "~/.nano-emacs")
            (server-name . "nano-server")))
 )
