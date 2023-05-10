;; Organize the configuration of org-mode

;; define functions for different sections of org config
;; in use-package invocation, load this file and call the functions

;; this must be loaded after fontaine

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
   ;; ✓✔︎
   :symbol '((comic-code . ?·)
             (belinsky   . ?·)
             (operator   . ?)
             (t          . ?✓))
   :face (defface eh/org-keyword-done '((t :inherit org-done))
           "Face used for the DONE keyword in Org"))

  (eh/org-register-keyword
   :keywords '("KILL" "CANCEL")
   :symbol ?× ;; ✗ ;;× ×
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
             (operator   . ?*)
             (t          . ?◯))
   ;;☐ ;; ?◦ ○
   :face (defface eh/org-keyword-todo '((t :inherit org-todo))
           "Face used for the TODO keyword in Org"))

  (eh/org-register-keyword
   :keywords '("HALT" "BLOK" "BLOCK")
   :symbol '((operator . ?!)
             (t        . ?▲)) ;;△ ;;◊▲
   :face (defface eh/org-keyword-halt '((t :inherit org-todo))
           "Face used for the BLOK keyword in Org"))

  (eh/org-register-keyword
   :keywords '("QUEST" "QSTN" "QUESTION")
   :symbol '((comic-code . ?◊)
             (input      . ?◊)
             (codelia    . ?◊)
             (operator   . ??)
             (t          . ?◇))
   :face (defface eh/org-keyword-question '((t :inherit org-todo))
           "Face used for the QSTN keyword in Org"))

  (eh/org-register-keyword
   :keywords '("IDEA" "YAKS" "YAK")
   :symbol '((jetbrains-mono . ?◌)
             (cascadia . ?◌)
             (operator . ?*)
             (t         . ?¤))
   ;; ∞ ҩ ¤ φ ♡
   :face (defface eh/org-keyword-idea '((t :inherit org-todo))
           "Face used for the IDEA keyword in Org"))

  (eh/org-register-keyword
   :keywords '("READ")
   :symbol '((operator . ?*)
             (t        . ?□))  ;;◊ ;;◇□
   :face (defface eh/org-keyword-read '((t :inherit org-todo))
           "Face used for the READ keyword in Org"))

  ;; hack for now: move other symbol-setting into this function
  ;; for easier reset after font change
  ;; (setq org-ellipsis (if (eq fontaine-current-preset 'jetbrains)
  ;;                        " ⋯" " ↓"))
  (setq org-ellipsis (pcase fontaine-current-preset
                       ('jetbrains " ⋯")
                       ('operator  " >")
                       ('belinsky  " ¶")
                       (t          " ↓")))
  ;; ↵ ⏎ ¶ ⌄ ▶ § ⋱ ◁ ◀ ∷ ⋯ ≡
  ;; ⤵ ⬎ [+] ▼ ↯
  (setq org-superstar-headline-bullets-list
        (pcase fontaine-current-preset
          ('comic-code '("♦" "•"))
          ('codelia '("●" "•"))
          ('belinsky '("•"))
          ('operator '("•"))
          (t  '("◆" "•"))))
        ;; (if (eq fontaine-current-preset 'comic-code)
        ;;     '("♦" "•")
        ;;   '("◆" "•")))
  )
;; (let () (eh/define-org-keywords) (org-mode-restart) (org-superstar-restart))
;; □ ☐

;; (set-face-attribute 'org-todo nil
;;                     :inverse-video t)
;; (set-face-attribute 'org-headline-todo nil
;;                     :weight 'reset
;;                     :inherit nil)

;; (set-face-attribute 'org-done nil
;;                     :slant 'italic)
;; (set-face-attribute 'org-headline-done nil
;;                     :weight 'reset
;;                     :inherit 'font-lock-comment-face)

;; (set-face-attribute 'org-superstar-header-bullet nil
;;                     :weight 'reset)
