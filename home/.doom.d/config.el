;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "Emily Hyland"
      user-mail-address "emily@duien.com")

;; NOTE Using a font with ligatures enabled (if it has a ~**~ or ~***~ ligature) will
;; cause the leading org bullets to disappear. Using ~org +pretty~ to get superstar-mode
;; seems to be an alternate fix
(setq doom-font (font-spec :family "MonoLisa" :size 14 :weight 'semi-light) )

(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

(setq doom-themes-treemacs-enable-variable-pitch nil)
;; (setq doom-themes-treemacs-theme "doom-colors")
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)
;; (setq doom-theme 'modus-operandi)
;; (setq doom-theme 'spacemacs-light)

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-string-face  :slant italic)
  )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq all-the-icons-scale-factor 1.0)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq emojify-emoji-set "twemoji-v2")
(setq which-key-idle-delay 0.5)


(use-package! projectile
  :config
  ;; (projectile-add-known-project "~/Code/doximity")
  ;; (projectile-add-known-project "~/.homesick/repos/dotfiles")
  (projectile-discover-projects-in-directory "~/Code/")
  (projectile-discover-projects-in-directory "~/.homesick/repos/")
  (projectile-add-known-project "~/.doom.d")
  )

;; (setq spacemacs-theme-comment-italic t
;;       spacemacs-theme-comment-bg nil
;;       spacemacs-theme-org-height nil)

;; (use-package! modus-themes
;;   :init
;;   (setq modus-themes-bold-constructs t
;;         modus-themes-slanted-constructs t
;;         modus-themes-syntax 'alt-synatx
;;         modus-themes-no-mixed-fonts t
;;         modus-themes-prompts 'intense-accented
;;         modus-themes-mode-line 'borderless-moody
;;         modus-themes-completions 'opinionated
;;         modus-themes-fringes 'intense
;;         modus-themes-lang-checkers 'colored-backgrounds
;;         ;; modus-themes-hl-line 'accented-background
;;         modus-themes-hl-line 'underline-only-neutral
;;         modus-themes-paren-match 'intense-bold
;;         modus-themes-region 'bg-only
;;         ;; modus-themes-scale-headings t
;;         modus-themes-headings
;;         '((1 . rainbow-highlight)
;;           (t . no-color-no-bold)))
;;   )

(setq mouse-wheel-tilt-scroll t) ;; horizontal scrolling

(setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

;; (custom-set-faces!
;;   '(org-superstar-leading :inherit org-hide))
