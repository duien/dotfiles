;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     ;; better-defaults
     emacs-lisp
     (git :variables
          git-magit-status-fullscreen t)
     markdown
     (org :variables
          org-enable-github-support t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'eshell)
     syntax-checking
     version-control
     osx

     ;; Things not in the original list
     ;; (colors :variables
     ;;         colors-enable-rainbow-identifiers t)
     colors
     dash
     emacs-lisp
     html
     javascript
     shell-scripts
     github
     rust
     themes-megapack
     emoji
     react
     ruby-on-rails
     dockerfile
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(groovy-mode)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(toxi-theme)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;; dotspacemacs-themes '(solarized-light
   ;;                       solarized-dark
   ;;                       spacemacs-light
   ;;                       spacemacs-dark
   ;;                       leuven
   ;;                       monokai
   ;;                       zenburn)
   ;; dotspacemacs-themes '(gruvbox)
   dotspacemacs-themes '(spacemacs-light)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   ;; TODO Find some way to make this work across macs
   dotspacemacs-default-font '("Fantasque Sans Mono" ;; M+ 2m"
                               :size 20
                               :weight light
                               :width normal
                               :powerline-scale 1.3)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; (default nil)
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here

  ;; This just seems to break everything
  ;; (when (memq window-system '(mac ns))
  ;;   (exec-path-from-shell-initialize))

  (add-hook 'text-mode-hook
            ;; (lambda () (toggle-truncate-lines -1))
            (lambda () (visual-line-mode t)))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; Turn off the `swipe-left' and `swipe-right' gestures that used to
  ;; annoyingly switch between files when I tried to scroll
  ;;
  ;; I could see turning them back on if there were an indicator (maybe
  ;; additional modeline at the top of the screen?) of what files they'd jump
  ;; too. And maybe if I could turn the sensitivity down, but that might not be
  ;; doable.
  (global-unset-key [swipe-left])
  (global-unset-key [swipe-right])

  ;; -------------------------------------------------------
  ;;  Lots of setup for getting `js' and `jsx' passable
  ;; -------------------------------------------------------

  (setq-default
   js2-basic-offset 2
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-attr-indent-offset 2)

  ;; ;; Use js2-mode for `.js' and `.jsx' files, since it handles code without
  ;; ;; semi-colons. 
  ;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))

  ;; ;; When loading web-mode in a js/jsx file, use the jsx content type.
  ;; (setq web-mode-content-types-alist
  ;;       '(("jsx" . "\\.js[x]?\\'")))

  ;; ;; Custom binding to switch between `web-mode' and `js2-mode' depending on
  ;; ;; what I'm mostly working on in a React file
  ;; (evil-leader/set-key-for-mode 'web-mode "or" 'js2-mode)
  ;; (evil-leader/set-key-for-mode 'js2-mode "or" 'web-mode)

  ;; It's OK to leave semicolons out of JavaScript.
  (setq js2-strict-missing-semi-warning nil)

  ;; -------------------------------------------------------
  ;; Various toggles and defaults and settings and things
  ;; -------------------------------------------------------

  ;; Allow toggling line numbers in just the current buffer
  (spacemacs|add-toggle local-line-numbers
                        :status linum-mode
                        :on (linum-mode)
                        :off (linum-mode -1)
                        :documentation "Show the line numbers in this buffer."
                        :evil-leader "tN")

  ;; Add the (probably inadvisable for updates) keybinding "hh"
  (evil-leader/set-key "hh" 'helm-apropos)

  ;; TODO Hmm... what is `ort'? It is found in SPC C menu
  (evil-leader/set-key "os" 'org-store-link)

  (setq vc-follow-symlinks t)
  (toggle-word-wrap 1) ;; Wrap at word boundaries instead of end of breaking inside words

  ;; Show enabled minor modes in plain ASCII so it's easier to tell the correct key to toggle
  (setq dotspacemacs-mode-line-unicode-symbols nil)

  (setq neo-theme 'nerd)
  (setq powerline-default-separator 'wave)
  (spacemacs/toggle-visual-line-navigation-on) ;; up/down within wrapped lines
  (spacemacs/toggle-highlight-current-line-globally-off)
  (spacemacs/toggle-fill-column-indicator-on)

  ;; Attempt to set up auto-indent in a sane way. This is surprisingly difficult.
  (setq indent-tabs-mode nil) ;; Always indent with spaces
  (setq standard-indent 2)
  (setq tab-width 2)
  (defvaralias 'c-basic-offset 'tab-width)
  (defvaralias 'cperl-indent-level 'tab-width)
  (setq evil-shift-width 2)

  ;; Assorted config for org-mode
  (setq org-fontify-done-headline t)
  (setq org-hide-leading-stars t)
  (setq org-bullets-bullet-list '("*"))
  (setq org-startup-folded nil)
  (setq org-cycle-level-faces nil)

  ;; More advanced config for org-mode
  (setq org-directory "~/Org/")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-agenda-files (append
                          (file-expand-wildcards (concat org-directory "*.org"))
                          (file-expand-wildcards (concat org-directory "**/*.org"))))
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))))
  (setq org-insert-heading-respect-content t)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Inbox")
           "** TODO %?\nCAPTURED: %u %a\n%i")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "LATER(l)" "|" "DONE(d)" "CANCEL(c)")
          (sequence "QUESTION(q)" "|" "ANSWER(a)")))
  (setq org-todo-keyword-faces
        '(
          ("TODO" . (:inherit org-todo :inverse-video t))
          ("QUESTION" . (:inherit org-todo :foreground "#268bd2" :inverse-video t))
          ("ANSWER" . (:inherit org-todo :foreground "#268bd2"  :inverse-video nil))
          ("LATER" . (:inherit org-todo :foreground "#b58900" :inverse-video nil))))
  (setq org-agenda-custom-commands
        '(("w" "Agenda and work tasks"
           ((agenda "" ((org-agenda-ndays 1) (org-deadline-warning-days 7)))
            ;; (agenda "" ((org-agenda-time-grid nil)
            ;;             (org-agenda-ndays 0)
            ;;             (org-deadline-warning-days 7)
            ;;             (org-agenda-entry-types '(:deadline))
            ;;             (org-agenda-overriding-header "Upcoming deadlines")
            ;;             ))
            (tags-todo "WORK+TODO=\"TODO\""
                       ((org-agenda-overriding-header "Work Tasks")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
            (tags-todo "WORK+TODO=\"QUESTION\"|INBOX+TODO=\"QUESTION\"" ((org-agenda-overriding-header "Questions")))
            (tags-todo "INBOX" ((org-agenda-overriding-header "Inbox")))
            (tags-todo "WORK+TODO=\"LATER\""
                       ((org-agenda-overriding-header "Work Tasks : Later")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
            ))))



  ;; -------------------------------------------------------
  ;; Some WIP things
  ;; -------------------------------------------------------

  ;; TODO How to I set the background for a default part of the modeline?
  ;; Ok, working on powerline, here's some info:
  ;;
  ;; Here's the link to the init, where most everything can be found
  ;;    [[file:~/.emacs.d/spacemacs/packages.el::(defun%20spacemacs/init-powerline%20()]]
  ;;
  ;; What I'd like to accomplish
  ;; - [ ] Only highlight the `TODO' keyword when it's actually at the start of a comment
  ;; - [ ] Set the background of the powerline to the `state-face' in the center fill area
  ;; - [ ] Get more useful git status info (changed/etc. with color, maybe
  ;;       commits ahead/behind instead of just the branch, which is actually not
  ;;       super-useful)
  ;; - [ ] ^^ Get org-mode to lay out that nicely
  ;;
  ;; I'm curious about the "literate programming" sort of thing a lot of folks
  ;; seem to do with their emacs config files. I ... feel like maybe this says
  ;; something about how hard it is to understand emacs lisp, and also something
  ;; about how very /deeply/ people customize emacs

  (setq spacemacs-mode-line-left
        '(
          ((workspace-number window-number)
            :fallback state-tag
            :separator "|"
            :face state-face)
          (buffer-modified buffer-id)
          (major-mode
            :face state-face)
          ((flycheck-errors flycheck-warnings flycheck-infos)
            :when active)
          ))
  (setq spacemacs-mode-line-right
        '(
          ;; (selection-info) ;; seems to not really work?
          (version-control :when active)
          (line-column :when active :face state-face)
          ((global-mode new-version) :when active)
          ))





  (setq dotspacemacs-persistent-server t)

  ;; In any programming mode, highlight the isolated string TODO BUG XXX or FIXME (which mysteriously stopped working again?)
  ;; Ideally, we'd want this to happen only when the keyword appears at the beginning of a comment, but that's harder
  (add-hook 'prog-mode-hook
            (lambda ()
              (defvar todo-comment-regexp "\\<\\(FIXME\\|TODO\\|BUG\\|XXX+\\):?\\>")
              (defvar-local todo-comment-graphic '(:inherit font-lock-comment-face :foreground "#715ab1" :weight bold :underline t))
              (defvar-local todo-comment-term    '(:inherit font-lock-comment-face :foreground "#af5fd7" :weight bold :underline t))

              (font-lock-add-keywords
               nil
               `((,todo-comment-regexp 1 ',(if (display-graphic-p) todo-comment-graphic todo-comment-term) t
                                      )))
              ))

  ;; If an org headline also contains a TODO (or similar) keyword, then remove all the fancy color and size styling
  ;; and fall back to `org-default' which is the same as notes
  ;; TODO I think maybe we want to do a little something with the face, rather than just default (darker?) but this
  ;; at least gets rid of the stuff I don't want
  (add-hook 'org-mode-hook
            (lambda ()
              (defvar alternate-heading-keyword-regexp-format "^\\**\\(\\*\\)\\(?: +%s\\)\\(?: +\\(.*?\\)\\)?[ 	]*$")
              (defvar org-keyword-headline-regexp (format alternate-heading-keyword-regexp-format org-todo-regexp))
              (font-lock-add-keywords
               'org-mode
               `((,org-keyword-headline-regexp
                  (1 'org-default t)
                  (2 'org-default t)
                  ))
              )))


  ;; Here's a wrapper for make-frame-command that allows you to customize how
  ;; emacs starts up between GUI and terminal environments, while sharing a
  ;; server.
  ;; From http://emacs.stackexchange.com/a/92
  ;; (defadvice make-frame-command (after make-frame-change-background-color last activate)
  ;;   "Adjusts the background color for different frame types. 
  ;; Graphical (X) frames should have the theme color, while terminal frames should match the terminal color (which matches the theme color...but terminal frames can't directly render this color)"
  ;;   (if (display-graphic-p)
  ;;       (set-background-color "#202020")
  ;;     (set-background-color "black")))
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(evil-shift-width 2)
 '(neo-vc-integration nil)
 '(org-inlinetask-show-first-star t)
 '(ring-bell-function (quote ignore) t)
 '(sp-highlight-pair-overlay nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(font-lock-comment-face ((t (:slant italic :foreground "#9f8fbd" :background "#f1eeed"))))
 '(org-ellipsis ((t (:background "azure2" :foreground "#1f71ab")))))
