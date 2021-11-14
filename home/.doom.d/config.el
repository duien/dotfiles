;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "Emily Hyland"
      user-mail-address "emily@duien.com")

;; NOTE Using a font with ligatures enabled (if it has a ~**~ or ~***~ ligature) will
;; cause the leading org bullets to disappear. Using ~org +pretty~ to get superstar-mode
;; seems to be an alternate fix
;; (setq doom-font (font-spec :family "MonoLisa" :size 15 :weight 'semi-light) )
;; (setq doom-font (font-spec :family "Galix Mono" :size 16 :weight 'semi-light) )
;; (setq doom-font (font-spec :family "Rec Mono Custom" :size 15) )
;; (setq doom-font (font-spec :family "IBM Plex Mono" :size 16) )
;; (setq doom-font (font-spec :family "Cascadia Code PL" :size 15 :weight 'semi-light) )
(setq doom-font (font-spec :family "Cascadia Mono" :size 15 :weight 'semi-light) )

(setq line-spacing 0.1) ;; for Cascadia, add a little space between lines
;; (setq line-spacing nil)

(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

(setq doom-themes-treemacs-enable-variable-pitch nil)
(setq doom-themes-treemacs-theme "doom-colors")
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one-light)
(setq doom-theme 'doom-flatwhite)
;; (setq doom-theme 'modus-operandi)
;; (setq doom-theme 'spacemacs-light)

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-string-face  :slant italic)
  '(line-number :weight light)
  '(markdown-language-keyword-face :inherit markdown-pre-face :slant italic)
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

(add-to-list 'auto-mode-alist '("CODEOWNERS" . conf-mode))

(setq emojify-emoji-set "twemoji-v2")
(setq which-key-idle-delay 0.5)

(map! :leader :desc "Toggle file browser" :n "\\" #'+treemacs/toggle)

(use-package! projectile
  :config
  (projectile-discover-projects-in-directory "~/Code/")
  (projectile-discover-projects-in-directory "~/Code/gems/")
  (projectile-discover-projects-in-directory "~/Code/Forks/")
  (projectile-discover-projects-in-directory "~/Code/Other/")
  (projectile-discover-projects-in-directory "~/.homesick/repos/")
  )

;; configure various doom themes that I like
(setq modus-themes-bold-constructs t
      modus-themes-slanted-constructs t
      modus-themes-syntax '(faint alt-syntax green-strings) ;; yellow-comments)
      modus-themes-no-mixed-fonts t
      modus-themes-links '(background)
      modus-themes-prompts '(intense accented)
      modus-themes-mode-line '(borderless accented)
      modus-themes-completions '(moderate)
      modus-themes-fringes '(subtle)
      modus-themes-lang-checkers '(background)
      modus-themes-hl-line nil
      modus-themes-paren-match '(intense bold)
      modus-themes-region '(bg-only)
      modus-themes-tabs-accented t
      modus-themes-headings
      '((1 . rainbow-highlight)
        (t . no-color-no-bold)))

(setq doom-gruvbox-light-brighter-comments t
      doom-gruvbox-light-variant "hard"
      doom-gruvbox-light-brighter-modeline t)
(setq doom-flatwhite-brighter-modeline t)
(setq doom-nord-light-brighter-modeline t)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode) ;; don't turn on hl-line by default

(setq mouse-wheel-tilt-scroll t) ;; horizontal scrolling

;; ORDERLESS

(defun flex-if-twiddle (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun without-if-bang (pattern _index _total)
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))

(setq orderless-matching-styles '(orderless-regexp)
      orderless-style-dispatchers '(without-if-bang))

;; (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
;; (use-package orderless
;;   :ensure t
;;   :custom (completion-styles '(orderless)))

(add-hook! markdown-mode :append
           #'visual-line-mode)
;; (custom-set-faces!
;;   '(org-superstar-leading :inherit org-hide))

;; Attempt to configure Org
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Org/"
       org-log-done t
       org-log-into-drawer t
       org-cycle-separator-lines -1
       org-fontify-done-headline t
       org-ellipsis " ▼")
(add-hook! org-mode (electric-indent-local-mode -1))
(add-hook! org-mode :append
           #'visual-line-mode)
(setq org-superstar-cycle-headline-bullets nil
       org-superstar-special-todo-items t
       org-superstar-headline-bullets-list '("◌" "•"))

(setq org-todo-keywords
      '((sequence "WAIT(w)" "FLAG(f)" "TODO(t)" "BLOK(b)" "HOLD(h)" "|" "DONE(d!)" "KILL(k@)")
        (sequence "STORY(s)" "PR(p)" "REVIEWED(R)" "PASSED(P)" "|" "DEPLOYED(D)")
        (sequence "QUESTION(q)" "|" "OK(o)" "YES(y)" "NO(n)" "ANSWER(a@)")
        (type "IDEA(I)" "YAK(Y)" "|")
        ))
(setq org-superstar-todo-bullet-alist '(("TODO" . ?⭘) ;11096) ;
                                        ("FLAG" . ?◍)
                                        ("DONE" . ?·)
                                        ("WAIT" . ?⏾)
                                        ("BLOK" . ?▲)
                                        ("HOLD" . ?≈)
                                        ("KILL" . ?×)
                                        ("QUESTION" . ?◇)
                                        ("ANSWER" . ?◆)
                                        ("OK" . ?·)
                                        ("YES" . ?·)
                                        ("NO" . ?·)
                                        ))
(setq org-superstar-leading-bullet "·" ; totally hide leading bullets, but let them take up space
         org-superstar-prettify-item-bullets nil)
(custom-set-faces!
  '(org-level-1 :inherit markdown-header-face)
  '(org-level-2 :inherit default)
  '(org-level-3 :inherit default)
  '(org-level-4 :inherit default)
  '(org-level-5 :inherit default)
  '(org-level-6 :inherit default)
  '(org-level-7 :inherit default)
  '(org-level-8 :inherit default)
  )
