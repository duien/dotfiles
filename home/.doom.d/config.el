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
;; (setq doom-font (font-spec :family "IBM Plex Mono" :size 16 :weight 'normal) )
;; (setq doom-font (font-spec :family "Cascadia Code PL" :size 15 :weight 'semi-light) )
;; (setq doom-font (font-spec :family "Gintronic" :size 15 :weight 'semilight))
(setq doom-font (font-spec :family "Cascadia Code" :size 15 :weight 'semilight) )
(setq ns-use-thin-smoothing t)

(setq line-spacing 0.1) ;; for Cascadia, add a little space between lines
;; (setq line-spacing nil)

;; this does the stupid thing where it uses separate characters instead of ligatures
;; (setq +ligatures-in-modes nil)
;; (setq +ligatures-extras-in-modes nil)

(modus-themes-load-themes)

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

(setq doom-themes-treemacs-enable-variable-pitch nil)
(setq doom-themes-treemacs-theme "doom-atom")
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one-light)
;; (setq doom-theme 'doom-flatwhite)
;; (setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'modus-operandi)
;; (setq doom-theme 'spacemacs-light)

;; (setq doom-theme 'chinolor-light)

;; can I show persp list (w/ shortcuts) after SPC TAB
;; (setq header-line-format (+workspace--tabline))
;; (after! persp-mode (setq header-line-format (+workspace--tabline)))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-string-face  :slant italic)
  ;; `(outline-1 :foreground ,(doom-color 'red))
  '(markdown-language-keyword-face :inherit markdown-pre-face :slant italic)
  '(line-number :weight semilight)
  '(line-number-current-line :weight semibold)
  )

;; Stuff just for doom-solarized
;; (custom-set-faces!
;;   `(font-lock-constant-face :weight semi-light :foreground ,(doom-color 'cyan)) ;; solarized was making symbols bold -- only needed for solarized
;;   `(font-lock-variable-name-face :foreground ,(doom-color 'violet)) ;; only needed for solarized
;;   '(font-lock-type-face :slant normal)
;;   `(font-lock-function-name-face :foreground ,(doom-color 'blue))
;;   )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t
      all-the-icons-scale-factor 1.0
      scroll-margin 5
      )

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
(after! treemacs
  (treemacs-follow-mode t))

(use-package! projectile
  :config
  (projectile-discover-projects-in-directory "~/Code/")
  (projectile-discover-projects-in-directory "~/Code/gems/")
  (projectile-discover-projects-in-directory "~/Code/Forks/")
  (projectile-discover-projects-in-directory "~/Code/Other/")
  (projectile-discover-projects-in-directory "~/.homesick/repos/")
  )

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-subtle-line-numbers t
      modus-themes-intense-markup t
      modus-themes-fringes 'intense
      modus-themes-mode-line '(borderless)
      modus-themes-syntax '(green-strings)
      ;; modus-themes-syntax '(alt-syntax green-strings)
      modus-themes-prompts '(gray background intense)
      modus-themes-region '(bg-only accented)
      modus-themes-org-blocks 'gray-background
      modus-themes-headings
      '((1 . (rainbow background))
        (t . (monochrome regular)))
      )
;; (load-theme 'modus-operandi)

;; (modus-themes-load-operandi)
;; (load-theme 'modus-operandi t)
(defun my-modus-themes-custom-faces ()
  (modus-themes-load-themes)
  ;; doom specifies a bitmap for the fringe indicator, which ends up coloring
  ;; the whole thing with the foreground (this makes modus unreadable)

  ;; this stops the error, but it the face is apparently not defined in time
  (if (facep 'git-gutter-fr:added)
        (set-face-attribute 'git-gutter-fr:added nil
                            :background (modus-themes-color 'green-graph-0-bg)
                            :foreground (modus-themes-color 'green-graph-0-bg))
        )
  (if (facep 'git-gutter-fr:modified)
        (set-face-attribute 'git-gutter-fr:modified nil
                            :background (modus-themes-color 'blue-graph-0-bg)
                            :foreground (modus-themes-color 'blue-graph-0-bg))
        )
  )
;; TODO Figure out why this isn't getting called
;; maybe it _is_ getting called, but the faces aren't defined yet?
;; there was maybe an error, but it disappeared too fast
(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)
;; (my-modus-themes-custom-faces)

(defun my-load-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (modus-themes-load-operandi) )
    ('dark (modus-themes-load-vivendi) )))
(setq ns-system-appearance-change-functions #'my-load-theme)

;; configure various doom themes that I like
(setq doom-gruvbox-light-brighter-comments nil
      doom-gruvbox-light-variant "hard"
      doom-gruvbox-light-brighter-modeline t)
(setq doom-flatwhite-brighter-modeline t)
(setq doom-nord-light-brighter-modeline t)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode) ;; don't turn on hl-line by default
(remove-hook 'after-change-major-mode-hook #'+ligatures-init-buffer-h) ;; this disables too many ligatures - find a way to disable just prettyify-symbols-mode that doesn't break everything else

(setq mouse-wheel-tilt-scroll t) ;; horizontal scrolling

;; (setq frame-title-format
;;       '(""
;;         (:eval
;;          (if (s-contains-p org-roam-directory (or buffer-file-name ""))
;;              (replace-regexp-in-string
;;               ".*/[0-9]*-?" "☰ "
;;               (subst-char-in-string ?_ ?  buffer-file-name))
;;            "%b"))
;;         (:eval
;;          (let ((project-name (projectile-project-name)))
;;            (unless (string= "-" project-name)
;;              (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
;; (setq frame-title-format '("%b" " · " (:eval (doom-project-name))))

;; in the terminal:
;;   defaults write org.gnu.Emacs TransparentTitleBar LIGHT
;; (push '(ns-transparent-titlebar . t) default-frame-alist)
;; (push '(ns-appearance . t) default-frame-alist)
;; (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center)
;; (setq vertico-posframe-parameters
;;       '(
;;         (vertico-posframe-width . (* (window-font-width) 20))
;;         (left-fringe . (window-font-width))
;;         (right-fringe . (window-font-width))
;;         (background-color . "#f1ece4")
;;         ;; (background-color . (face-attribute 'solaire-default-face :background)) ;; taken from solaire-default-face :background
;;         ))

;; ;; (face-attribute 'solaire-default-face :background) ;; this returns a value, but if I try to use it in the parameters, vertico doesn't show up
;; ;; dig around in doom-real-buffer-p to see how to make vertico "unreal"
;; ;; vertico goes in the minibuffer, vs which-key which I think creates a buffer
;; ;; in the normal course of things, though, vertico does use the darker background :thinking-face:
;; (vertico-posframe-mode 1)
;; (which-key-posframe-mode 1)

;; (mini-frame-mode 1)
(setq mini-frame-show-parameters
      '((left . 0.5)
        (top . 0.3)
        (width . 0.7)
        (height . 10)
        )
      mini-frame-resize t
      )
;; overall, this is going better than posframe...
;; however, it somehow stops vertico from displaying suggestions until you start typing
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

;; Change where the new window goes when splitting
(setq evil-vsplit-window-right t
      evil-split-window-below t
      )
;; prompt for file for new split (same as SPC SPC)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (projectile-find-file))
;; (defadvice! prompt-for-buffer (&rest _)
;;   :after 'window-split (switch-to-buffer))

(add-hook! markdown-mode :append
           #'visual-line-mode)
;; (custom-set-faces!
;;   '(org-superstar-leading :inherit org-hide))

;; Attempt to configure Org
(setq org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/Org/"
      org-log-done t
      org-log-into-drawer t
      org-cycle-separator-lines -1
      org-fontify-whole-heading-line t
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
;; (custom-set-faces!
;;   '(org-level-1 :inherit markdown-header-face)
;;   '(org-level-2 :inherit default)
;;   '(org-level-3 :inherit default)
;;   '(org-level-4 :inherit default)
;;   '(org-level-5 :inherit default)
;;   '(org-level-6 :inherit default)
;;   '(org-level-7 :inherit default)
;;   '(org-level-8 :inherit default)
;;   )
