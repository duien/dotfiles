;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
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
     colors
     deft
     elixir
     emacs-lisp
     emoji
     eyebrowse
     git
     markdown
     org
     osx
     ;; ranger
     react
     (ruby :variables
           ruby-enable-enh-ruby-mode nil)
     shell-scripts
     themes-megapack
     theming
     (shell :variables
            shell-default-height 30
            shell-default-shell 'eshell
            shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     version-control
     yaml
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(groovy-mode ruby-guard)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(org-bullets window-numbering)
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
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
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
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         ;; gruvbox
                         ;; light-soap
                         ;; brin
                         ;; omtose-phellack
                         spacemacs-light
                         ;; darktooth
                         )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Operator Mono SSm"
                               :size 16
                               :weight light
                               :width normal
                               :powerline-scale 1.4)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
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
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ))


;; Things to Investigate
;; https://github.com/syl20bnr/spacemacs/tree/master/layers/theming
;; https://github.com/d125q/gruvbox-dark-emacs

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."

  ;; This seems to require being in `init'
  ;; But then it loses its value if I switch themes
  ;; (setq spacemacs-theme-comment-bg nil)
  ;; (setq spacemacs-theme-org-height nil)

  ;; Experiment with theme tweaking using `theming' layer
  ;; This seems to require being in `init' rather than config
  (setq theming-modifications
        '(
          (omtose-phellack
           (font-lock-string-face  :slant italic)
           (font-lock-comment-face :slant italic)
           (org-todo :weight bold  :inverse-video t)
           (org-done :weight bold  :slant italic :foreground "#868691")
           (org-headline-done      :slant italic :foreground "#73767d")
           (markdown-header-face   :inherit font-lock-function-name-face :weight bold :slant italic)
           (helm-selection         :background "#BB4EB8")
           (helm-selection-line    :background "#BB4EB8")
           )
          (darktooth
           (font-lock-string-face  :slant italic)
           (font-lock-comment-face :slant italic))
          (spacemacs-light
           (font-lock-string-face  :slant italic)
           (font-lock-comment-face :slant italic :foreground "#B8AEB6")
           (font-lock-doc-face     :slant italic)
           )
          (gruvbox
           (font-lock-comment-face :slant italic)
           (font-lock-string-face  :slant italic)
           (org-done    :foreground "#928374" :slant italic :weight bold)
           (org-todo    :inverse-video t :weight bold)
           (org-level-1 :inherit org-default :foreground "#FB4933") ;; red
           (org-level-2 :inherit org-default :foreground "#FE8019") ;; orange
           (org-level-3 :inherit org-default :foreground "#FABD2F") ;; yellow
           (org-level-4 :inherit org-default :foreground "#B8BB26") ;; green
           (org-level-5 :inherit org-default :foreground "#8EC07C") ;; aqua
           (org-level-6 :inherit org-default :foreground "#83A598") ;; blue
           (org-level-7 :inherit org-default :foreground "#D3869B") ;; purple
           (org-level-8 :inherit org-default)
           (org-headline-done :foreground "#928374" :slant italic)
           )
          )
        )

  ;; This crazy hook here allows removing the coloring from org-mode headlines that
  ;; have a TODO keyword, which keeps things much cleaner-looking
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

  (add-hook 'text-mode-hook 'spacemacs/toggle-truncate-lines-on)
  (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

  (add-hook 'prog-mode-hook 'spacemacs/toggle-truncate-lines-off)
  (add-hook 'prog-mode-hook 'spacemacs/toggle-visual-line-navigation-off)

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  (setq fci-rule-color "#928374")
  (setq deft-directory "~/Dropbox/PlainText")

  (prefer-coding-system 'utf-8-unix)
  (setq powerline-default-separator 'slant)
  (setq vc-follow-symlinks t)
  ;; TODO The word-wrap, truncate-lines, fill-column mess is just *not* working
  ;; (spacemacs/toggle-truncate-lines-on)
  ;; (spacemacs/toggle-visual-line-navigation-on)
  (spacemacs/toggle-highlight-current-line-globally-off)
  (spacemacs/toggle-vi-tilde-fringe-off)
  (spacemacs/toggle-fill-column-indicator-on)

  ;; Set up indentation (suprisingly complicated)
  ;; IDEA Would this work well for a (large) contribution to spacemacs?
  (setq indent-tabs-mode nil)
  (setq standard-indent 2)
  (setq tab-width 2)
  (defvaralias 'c-basic-offset 'tab-width)
  (defvaralias 'cperl-indent-level 'tab-width)
  (setq evil-shift-width 2)


  ;; ------------------------ ;;
  ;;         ORG MODE         ;;
  ;; ------------------------ ;;
  (with-eval-after-load 'org

    ;; Set up basic org-mode options
    (setq org-fontify-whole-heading-line t)
    (setq org-fontify-done-headline t)
    (setq org-hide-leading-stars t)
    (setq org-startup-folded nil)
    (setq org-cycle-level-faces nil)
    (setq org-insert-heading-respect-content t)

    ;; Tell org where to find files
    (setq org-directory "~/Org/")
    (setq org-default-notes-file (concat org-directory "/inbox.org"))
    (setq org-default-log-file   (concat org-directory "/logbook.org"))
    (setq org-agenda-files (append
                            (file-expand-wildcards (concat org-directory "*.org"))
                            (file-expand-wildcards (concat org-directory "**/*.org"))))
    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 9))))

    ;; Set up org capture templates
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-default-notes-file "Inbox")
             "** TODO %?\nCAPTURED: %u %a\n%i")
            ("l" "Log to daybook" plain (file+datetree org-default-log-file)
             "%? (logged from [[%l][%f]])")
            ))

    ;; Set up org keywords
    (setq org-todo-keywords
          '((sequence "TODO(t)" "WAIT(w)" "LATER(l)" "|" "DONE(d)" "CANCEL(c)")
            (sequence "QUESTION(q)" "|" "ANSWER(a)")
            (sequence "IDEA")))

    ;; Set up faces for keywords
    ;; TODO try to find a way to incorporate this better into a syntax theme
    (setq org-todo-keyword-faces
          '(
            ;; Colors of Omtose Phellack theme
            ("TODO" .     (:inherit org-todo :foreground "#BB4EB8"))
            ("WAIT" .     (:inherit org-todo :foreground "#868691" :inverse-video nil))
            ("LATER" .    (:inherit org-todo :foreground "#BB4EB8" :inverse-video nil))

            ("DONE" .    (:inherit org-done))
            ("CANCEL" .  (:inherit org-done))

            ("QUESTION" . (:inherit org-todo :foreground "#5D809E"))
            ("ANSWER" .   (:inherit org-done :foreground "#5D809E"))

            ("IDEA" .    (:inherit org-todo :foreground "yellow"))

            ;; Colors for Gruvbox theme
            ;; ("TODO" .     (:inherit org-todo :foreground "#B8BB26"))
            ;; ("QUESTION" . (:inherit org-todo :foreground "#83A598"))
            ;; ("ANSWER" .   (:inherit org-done :foreground "#83A598"))
            ;; ("WAIT" .     (:inherit org-todo :foreground "#B16286" :inverse-video nil))
            ;; ("LATER" .    (:inherit org-todo :foreground "#FABD2F" :inverse-video nil))
            ))

    ) ;; end org-mode section


  ;; ------------------------ ;;
  ;;         BAD IDEAS        ;;
  ;; ------------------------ ;;

  ;; don't get any ... big ideas ... never ... gonna happen

  ;; working on a custom `spaceline' theme
  ;; see: https://github.com/TheBB/spaceline

  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-line-column-off)
  (spaceline-toggle-buffer-encoding-abbrev-off)


  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-org-height nil)
)
