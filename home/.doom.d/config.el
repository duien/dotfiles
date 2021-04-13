;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(setq user-full-name "Emily Hyland"
      user-mail-address "emily@duien.com")

;; NOTE Using a font with ligatures enabled (if it has a ~**~ or ~***~ ligature) will
;; cause the leading org bullets to disappear. Using ~org +pretty~ to get superstar-mode
;; seems to be an alternate fix
(setq doom-font (font-spec :family "MonoLisa" :size 16 :weight 'semi-light) )

(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)

(setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Org/"
      org-log-done t
      org-log-into-drawer t
      org-cycle-separator-lines -1
      org-fontify-done-headline t
      org-ellipsis " ▼")

(add-hook! org-mode (electric-indent-local-mode -1))
(add-hook! org-mode :append
           #'visual-line-mode
           ;; #'variable-pitch-mode
           )

(after! org
  (setq org-default-notes-file (concat org-directory "inbox.org")
        org-default-log-file   (concat org-directory "logbook.org")
        org-hide-leading-stars nil
        )
  (setq org-todo-keywords
        '((sequence "WAIT(w)" "FLAG(f)" "TODO(t)" "BLOK(b)" "HOLD(h)" "|" "DONE(d!)" "KILL(k@)")
          (sequence "STORY(s)" "PR(p)" "REVIEWED(R)" "PASSED(P)" "|" "DEPLOYED(D)")
          (sequence "QUESTION(q)" "|" "OK(o)" "YES(y)" "NO(n)" "ANSWER(a@)")
          (type "IDEA(I)" "YAK(Y)" "|")
          ))
  (setq org-todo-keyword-faces
        '(
          ("WAIT" . modus-themes-special-warm)
          ("FLAG" . (:inherit modus-themes-intense-red :weight bold))
          ("TODO" . (:inherit modus-themes-intense-green :weight bold))
          ("BLOK" . (:inherit modus-themes-intense-yellow :weight bold))
          ("HOLD" . modus-themes-intense-neutral)
          ("DONE" . modus-themes-subtle-neutral)
          ("KILL" . modus-themes-special-calm)

          ("QUESTION" . modus-themes-intense-blue)
          ("OK" . modus-themes-intense-neutral)
          ("YES" . modus-themes-refine-green)
          ("NO" . modus-themes-refine-red)
          ("ANSWER" . modus-themes-special-cold)

          ("IDEA" . modus-themes-intense-magenta)
          ("YAK" . modus-themes-refine-magenta)

          ("STORY" . (:inherit modus-themes-special-warm :weight bold))
          ("PR" . modus-themes-refine-blue)
          ("REVIEWED" . modus-themes-refine-cyan)
          ("PASSED" . modus-themes-refine-green)
          ("DEPLOYED" . modus-themes-subtle-neutral)
          ))
  )

(setq org-superstar-cycle-headline-bullets nil
      org-superstar-special-todo-items t
      org-superstar-headline-bullets-list '("◌" "•"))

(after! org-superstar
  (setq org-superstar-leading-bullet "·" ; totally hide leading bullets, but let them take up space
        org-superstar-prettify-item-bullets nil)
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
                                          ("STORY" . ?⏽)
                                          ("PR" . ?⏽)
                                          ("REVIEWED" . ?⏽)
                                          ("PASSED" . ?⏽)
                                          ;; ("WORK" . ?⭘)
                                          ("DEPLOYED" . ?·)
                                          ))
  (org-superstar-restart))

(after! org-capture
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Inbox")
           "** TODO %?\nCAPTURED: %u %a\n%i")
          ("l" "Log to daybook" plain (file+datetree org-default-log-file)
           "%? (logged from [[%l][%f]])")))
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
  (projectile-add-known-project "~/Code/doximity")
  (projectile-add-known-project "~/.homesick/repos/dotfiles")
  (projectile-add-known-project "~/.doom.d")
  ;; (projectile-add-known-project "~/Org")
  (projectile-add-known-project org-directory)
  )

;; (after! org
;;   (setq org-log-done t)
;;   (setq org-catch-invisible-edits 'smart)
;;   )
;; (add-hook! org-mode (electric-indent-local-mode -1))
;; (add-hook! org-mode :append
;;            #'visual-line-mode
;;            #'variable-pitch-mode)

(use-package! modus-themes
  :init
  (setq modus-themes-bold-constructs t
        modus-themes-slanted-constructs t
        modus-themes-syntax 'alt-synatx
        modus-themes-no-mixed-fonts t
        modus-themes-prompts 'intense-accented
        modus-themes-mode-line 'borderless-moody
        modus-themes-completions 'opinionated
        modus-themes-fringes 'intense
        modus-themes-lang-checkers 'colored-backgrounds
        ;; modus-themes-hl-line 'accented-background
        modus-themes-hl-line 'underline-only-neutral
        modus-themes-paren-match 'intense-bold
        modus-themes-region 'bg-only
        ;; modus-themes-scale-headings t
        modus-themes-headings
        '((1 . rainbow-highlight)
          (t . no-color-no-bold)))
  )

;; (use-package! moody
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode))

;; (use-package! spaceline
;;   :config
;;   (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(custom-set-faces!
  '(org-superstar-leading :inherit org-hide))
