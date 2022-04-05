;; NOTE: In order to work with `emacsserver' utility, the server-name must be
;; the same as the profile name, with `-server' appended
(("scratch" . ((user-emacs-directory . "~/.scratch-config")
               (server-name . "scratch-server")))
 ("doom" . ((user-emacs-directory . "~/.doom")
	       (env . (("DOOMDIR" . "~/.doom-config")))
               (server-name . "doom-server")
               (straight-p)
               ))
 ("castlemacs" . ((user-emacs-directory . "~/.castlemacs")
                  (server-name . "castlemacs-server")))
 ("minimal" . ((user-emacs-directory . "~/.minimal-config")
               (server-name . "minimal-server")))
 )
