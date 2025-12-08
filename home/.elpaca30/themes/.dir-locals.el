;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . (;;(mode . rainbow-mode)
         ;;(enable-local-eval . t)
         (eval . (when (fboundp 'rainbow-mode) (rainbow-mode 1)))
         (eval . (when (fboundp 'load-theme-if-enabled)
                   (add-hook 'after-save-hook #'load-theme-if-enabled nil t))))))
