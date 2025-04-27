;; Organize the configuration of org-mode

;; define functions for different sections of org config
;; in use-package invocation, load this file and call the functions

;; this must be loaded after fontaine

;;; hook function to make C-c C-c open source block for editing instead of exec

(defun eh/hook-edit-src-block ()
  (cond ((org-in-src-block-p) (org-edit-src-code))))

;;; collapse done entries

(defun eh/org-get-folded-state ()
    (cond
        ((not (or (org-at-item-p) (org-at-heading-p)))
            'not-at-node)
        ((org-before-first-heading-p)
            'not-at-node)
        (t
            (let (eoh eol eos has-children children-skipped struct)
                ;; First, determine end of headline (EOH), end of subtree or item
                ;; (EOS), and if item or heading has children (HAS-CHILDREN).
                (save-excursion
                    (if (org-at-item-p)
                        (progn
                            (beginning-of-line)
                            (setq struct (org-list-struct))
                            (setq eoh (point-at-eol))
                            (setq eos (org-list-get-item-end-before-blank (point) struct))
                            (setq has-children (org-list-has-child-p (point) struct)))
                        (org-back-to-heading)
                        (setq eoh (save-excursion (outline-end-of-heading) (point)))
                        (setq eos (save-excursion (org-end-of-subtree t t)
                                      (when (bolp) (backward-char)) (point)))
                        (setq has-children
                            (or (save-excursion
                                    (let ((level (funcall outline-level)))
                                        (outline-next-heading)
                                        (and (org-at-heading-p t)
                                            (> (funcall outline-level) level))))
                                (save-excursion
                                    (org-list-search-forward (org-item-beginning-re) eos t)))))
                    ;; Determine end invisible part of buffer (EOL)
                    (beginning-of-line 2)
                    (while (and (not (eobp)) ;; this is like `next-line'
                               (get-char-property (1- (point)) 'invisible))
                        (goto-char (next-single-char-property-change (point) 'invisible))
                        (and (eolp) (beginning-of-line 2)))
                    (setq eol (point)))
                (cond
                    ((= eos eoh)
                        'empty-node)
                    ((or (>= eol eos)
                         (not (string-match "\\S-" (buffer-substring eol eos))))
                        'folded)
                    (t
                        'not-folded))))))

(defun eh/org-tree-can-fold-p ()
    (not (member (eh/org-get-folded-state) (list 'folded 'empty-node))))

(defun eh/org-cycle-until-folded ()
    (while (eh/org-tree-can-fold-p)
        (org-cycle)))

(defun eh/org-hide-done-entries-in-range (start end)
    (save-excursion
        (goto-char end)
        (while (and (outline-previous-heading) (> (point) start))
            (when (org-entry-is-done-p)
                (eh/org-cycle-until-folded)))))

(defun eh/org-hide-done-entries-in-region (start end)
    (interactive "r")
    (eh/org-hide-done-entries-in-range start end))

(defun eh/org-hide-done-entries-in-buffer ()
    (interactive)
    (eh/org-hide-done-entries-in-range (point-min) (point-max)))

;;; keyword management

(cl-defun eh/org-register-keyword (&key keywords face symbol)
  (let ((symbol-for-face (if (listp symbol)
                             ;; with a list, car is a fontaine preset or `t' for fallback
                             (or (cdr (or (assoc fontaine-current-preset symbol)
                                          (assoc t symbol)))
                                 ?·)
                           symbol)))
    (dolist (key keywords)
      (add-to-list 'org-todo-keyword-faces `(,key . ,face))
      (add-to-list 'org-superstar-todo-bullet-alist `(,key . ,symbol-for-face)))))

(defun eh/define-org-keywords ()
  ;; Register all variations of keywords with faces and symbols
  ;; Only the ones included in `org-todo-keywords' will be
  ;; activated by default, but the others will still be styled
  ;; if activated by buffer properties

  ;; First, clear out (and register) the two lists we're building up
  (setq org-todo-keyword-faces '()
        org-superstar-todo-bullet-alist '())

  (eh/org-register-keyword
   :keywords '("DONE")
   ;; ✓✔︎√
   :symbol '((comic-code . ?·)
             (belinsky   . ?·)
             (antikor    . ?·)
             (vctr       . ?·) ; vctr has a private-use checkbox, but width is wrong
             ;; (belinsky   . ?∕)
             ;; (belinsky   . ?¬)
             ;; (belinsky   . ?⁄)
             (operator   . ?)
             (dank       . ?·)
             (pragmata   . ?√) ;; weirdly, this is square root which is usually bad
             (dolph      . ?·)
             (t          . ?✓))
   :face (defface eh/org-keyword-done '((t :inherit org-done))
           "Face used for the DONE keyword in Org"))

  (eh/org-register-keyword
   :keywords '("KILL" "CANCEL")
   ;; :symbol ?× ;; ✗ ;;× ×
   :symbol '((belinsky . ?·)
             (antikor  . ?·)
             ;; (monolisa . ?✗)
             (t        . ?×))
   :face (defface eh/org-keyword-kill '((t :inherit org-done))
           "Face used for the KILL keyword in Org"))

  (eh/org-register-keyword
   :keywords '("ANSWER" "ANSR")
   :symbol ?·
   :face (defface eh/org-keyword-answer '((t :inherit org-done))
           "Face used for the ANSR keywork in Org"))

  (eh/org-register-keyword
   :keywords '("MEH" "OK")
   :symbol ?·
   :face (defface eh/org-keyword-meh '((t :inherit org-done))
           "Face used for the OK keyword in Org"))

  (eh/org-register-keyword
   :keywords '("YES")
   :symbol ?·
   :face (defface eh/org-keyword-yes '((t :inherit eh/org-keyword-done))
           "Face used for the YES keyword in Org"))

  (eh/org-register-keyword
   :keywords '("NO")
   :symbol ?·
   :face (defface eh/org-keyword-no '((t :inherit eh/org-keyword-kill))
           "Face used for the NO keyword in Org"))

  (eh/org-register-keyword
   :keywords '("RODE")
   :symbol ?·
   :face (defface eh/org-keyword-rode '((t :inherit 'org-done))
           "Face used for RODE keyword in Org"))

  (eh/org-register-keyword
   :keywords '("BURY" "WAIT" "HOLD" "LATER")
   :symbol ?~
   :face (defface eh/org-keyword-bury '((t :inherit org-todo))
           "Face used for the WAIT keyword in Org"))

  (eh/org-register-keyword
   :keywords '("NEXT" "FLAG")
   :symbol '((operator . ?#)
             ;; (belinsky . ?#)
             (belinsky . ?∆)
             (antikor  . ?∆)
             (plex     . ?∆)
             (monolisa . ?◉)
             (vctr     . ?#)
             (md-io    . ?#)
             (dank     . ?△)
             (t        . ?■))
   ;;◯ ;;☐ ;; ?◦●
   :face (defface eh/org-keyword-next '((t :inherit org-todo))
           "Face used for the FLAG keyword in Org"))

  (eh/org-register-keyword
   :keywords '("TODO")
   :symbol '((comic-code . ?○)
             (cascadia   . ?○)
             (input      . ?○)
             (codelia    . ?○)
             (victor     . ?○)
             (recursive  . ?▷)
             ;; (belinsky   . ?•)
             (belinsky   . ?∕)
             (antikor    . ?∕)
             (operator   . ?•)
             (vctr       . ?•)
             (apple-sf   . ?○)
             (pragmata   . ?○)
             (monolisa   . ?○)
             (md-io      . ?○)
             (plex       . ?∕)
             (berkeley   . ?•)
             (dank       . ?○)
             (dolph      . ?○)
             (t          . ?◯))
   ;;☐ ;; ?◦ ○
   :face (defface eh/org-keyword-todo '((t :inherit org-todo))
           "Face used for the TODO keyword in Org"))

  (eh/org-register-keyword
   :keywords '("HALT" "BLOK" "BLOCK")
   :symbol '((operator . ?!)
             (vctr     . ?!)
             (md-io    . ?!)
             ;; (belinsky . ?!)
             (belinsky . ?∆)
             (antikor  . ?∆)
             (plex     . ?∆)
             (t        . ?▲)) ;;△ ;;◊▲
   :face (defface eh/org-keyword-halt '((t :inherit org-todo))
           "Face used for the BLOK keyword in Org"))

  (eh/org-register-keyword
   :keywords '("QUEST" "QSTN" "QUESTION")
   :symbol '((comic-code . ?◊)
             (input      . ?◊)
             (codelia    . ?◊)
             (operator   . ??)
             ;; (belinsky   . ??)
             ;; (belinsky   . ?•)
             (vctr       . ??)
             (md-io      . ??)
             (belinsky   . ?∕)
             (antikor    . ?∕)
             (apple-sf   . ?◊)
             (plex       . ?∕)
             (berkeley   . ?◊)
             (t          . ?◇))
   :face (defface eh/org-keyword-question '((t :inherit org-todo))
           "Face used for the QSTN keyword in Org"))

  (eh/org-register-keyword
   :keywords '("IDEA" "YAKS" "YAK")
   :symbol '((jetbrains . ?◌)
             (cascadia  . ?◌)
             (pragmata  . ?◌)
             (monolisa  . ?◌)
             (berkeley  . ?◌)
             (operator  . ?•)
             (vctr      . ?•)
             (md-io     . ?•)
             ;; (belinsky . ?•)
             (belinsky  . ?∕)
             (antikor   . ?∕)
             ;; (belinsky  . ?∞)
             (plex      . ?∕)
             (dolph     . ?⁄)
             (t         . ?¤))
   ;; ∞ ҩ ¤ φ ♡
   :face (defface eh/org-keyword-idea '((t :inherit org-todo))
           "Face used for the IDEA keyword in Org"))

  (eh/org-register-keyword
   :keywords '("READ")
   :symbol '((operator . ?•)
             (vctr     . ?•)
             (md-io    . ?•)
             ;; (belinsky . ?•)
             ;; (belinsky  . ?÷)
             (belinsky   . ?∕)
             (antikor    . ?∕)
             (plex       . ?∕)
             (dank       . ?·)
             (t        . ?□))  ;;◊ ;;◇□
   :face (defface eh/org-keyword-read '((t :inherit org-todo))
           "Face used for the READ keyword in Org"))

  ;; hack for now: move other symbol-setting into this function
  ;; for easier reset after font change
  ;; (setq org-ellipsis (if (eq fontaine-current-preset 'jetbrains)
  ;;                        " ⋯" " ↓"))
  (setq org-ellipsis (pcase fontaine-current-preset
                       ('jetbrains " ⋯")
                       ('pragmata  " ⋯")
                       ('operator  " >")
                       ('belinsky  " ¶")
                       ('antikor   " ↩")
                       ('plex      " ↲")
                       ('monolisa  " ↲")
                       ('md-io     " ⤶")
                       (_          " ↓")))
  ;; ↵ ⏎ ¶ ⌄ ▶ § ⋱ ◁ ◀ ∷ ⋯ ≡
  ;; ⤵ ⬎ [+] ▼ ↯
  ;; • ▶ » ¶ § → ■ ● available in SF
  (setq org-superstar-headline-bullets-list
        (pcase fontaine-current-preset
          ('comic-code '("♦" "•"))
          ('codelia '("●" "•"))
          ;; ('belinsky '("◊" "•"))
          ('belinsky '("§" "•"))
          ('antikor  '("§" "•"))
          ('operator '("•"))
          ('apple-sf '("●" "•"))
          ('md-io    '("●" "•"))
          ('plex     '("§" "•"))
          ('vctr     '("§" "•"))
          ('input    '("●" "•"))
          (_  '("◆" "•")))))
;; (eh/define-org-keywords)

(defun eh/org-skip-subtree-if-bury ()
  "If this entry has the BURY keyword, skip it and its children"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-get-todo-state) "BURY")
        subtree-end
      nil)))
(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
         ((agenda "")
          (alltodo "")))
        ("g" "Grouped agenda task list"
         ((agenda "Calendar"
                  ((org-agenda-span 1)
                   (org-agenda-overriding-header "Today")
                   (org-agenda-show-log t)
                   (org-agenda-use-time-grid nil)))
          (todo "NEXT|HALT" ((org-agenda-overriding-header "Look here first")))
          (todo ""
                ((org-agenda-overriding-header "Get things done")
                 (org-agenda-sorting-strategy '(priority-down))
                 (org-agenda-todo-ignore-with-date t)
                 (org-agenda-skip-function
                  '(or
                    (eh/org-skip-subtree-if-bury)
                    (org-agenda-skip-entry-if 'todo '("NEXT" "READ" "IDEA" "BURY" "GOAL"))
                    ))
                 ))
          (todo "READ|IDEA" ((org-agenda-overriding-header "Investigate")))
          (todo "BURY" ((org-agenda-overriding-header "Burried tasks"))))
         (;(org-agenda-files (file-expand-wildcards "~/Org/dox-*.org"))
          (org-agenda-tag-filter-preset '("-meta" "-test"))
          ;; (org-agenda-compact-blocks t)
          (org-agenda-prefix-format "  %?s"))
         )
        ("w" "Work task list"
         ((agenda "Calendar"
                  ((org-agenda-span 1)
                   (org-agenda-overriding-header "Today")
                   (org-agenda-show-log t)
                   (org-agenda-use-time-grid nil)))
          (todo "NEXT|HALT" ((org-agenda-overriding-header "Look here first")))
          (todo ""
                ((org-agenda-overriding-header "Get things done")
                 (org-agenda-sorting-strategy '(priority-down))
                 (org-agenda-todo-ignore-with-date t)
                 (org-agenda-skip-function
                  '(or
                    (eh/org-skip-subtree-if-bury)
                    (org-agenda-skip-entry-if 'todo '("NEXT" "READ" "IDEA" "BURY" "GOAL"))
                    ))
                 ))
          (todo "READ|IDEA" ((org-agenda-overriding-header "Investigate")))
          (todo "BURY" ((org-agenda-overriding-header "Burried tasks"))))
         (;(org-agenda-files (file-expand-wildcards "~/Org/dox-*.org"))
          (org-agenda-files
           (append
            (file-expand-wildcards "~/Org/skylight/*.org")
            '("~/Org/skylight.org")))
          (org-agenda-tag-filter-preset '("-meta" "-test"))
          ;; (org-agenda-compact-blocks t)
          (org-agenda-prefix-format "  %?s"))
         )
        ("y" "Yak-shaving task list"
         ((agenda "Calendar"
                  ((org-agenda-span 1)
                   (org-agenda-overriding-header "Today")
                   (org-agenda-show-log t)
                   (org-agenda-use-time-grid nil)))
          (todo "NEXT|HALT" ((org-agenda-overriding-header "Look here first")))
          (todo ""
                ((org-agenda-overriding-header "Get things done")
                 (org-agenda-sorting-strategy '(priority-down))
                 (org-agenda-todo-ignore-with-date t)
                 (org-agenda-skip-function
                  '(or
                    (eh/org-skip-subtree-if-bury)
                    (org-agenda-skip-entry-if 'todo '("NEXT" "READ" "IDEA" "BURY" "GOAL"))
                    ))
                 ))
          (todo "READ|IDEA" ((org-agenda-overriding-header "Investigate")))
          (todo "BURY" ((org-agenda-overriding-header "Burried tasks"))))
         (;(org-agenda-files (file-expand-wildcards "~/Org/dox-*.org"))
          (org-agenda-tag-filter-preset '("-test"))
          ;; (org-agenda-compact-blocks t)
          (org-agenda-prefix-format "  %?s"))
         )))
