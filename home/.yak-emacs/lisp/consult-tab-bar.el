;; https://github.com/minad/consult/wiki#completing-tabs-from-tab-bar-with-preview
;; https://github.com/minad/consult/wiki#dwim-function-that-i-took-from-prot-and-adapted

(defun +tab-bar-dwim (&optional arg)
  "Do-What-I-Mean function for tabs.
If optional prefix argument is specified, then switch to `ARG'th tab.

If no other tab exists, create one and switch to it.

If there is one other tab (two in total), switch to it.

If there are more than two tabs, select tab with `+consult-tab'."
  (interactive "P")
  (if arg
      (tab-bar-select-tab arg)
    (pcase (length (tab-bar-tabs))
      (1 (tab-bar-new-tab))
      (2 (tab-bar-switch-to-next-tab))
      (_ (+consult-tab)))))




(defun +tab-bar--make-completion-list (tab-list)
  "Return completion list of strings formatted from TAB-LIST."
  (mapcar (lambda (tab)
            (let ((index (1+ (tab-bar--tab-index tab)))
                  (name (alist-get 'name tab)))
              (format "%d %s" index name)))
          tab-list))

(defun +tab-bar--completion-list-recent ()
  "Return completion list of recent tabs (current not included)."
  (+tab-bar--make-completion-list (tab-bar--tabs-recent)))

(defun +tab-bar--index-from-candidate (cand)
  "Return prefix index of CAND."
  (let ((match (string-match "^[[:digit:]]+" cand)))
    (when match
      (string-to-number (match-string match cand)))))

(defun +tab-bar--tab-from-index (index)
  "Return tab from `(tab-bar-tabs)' by index of CAND."
  (when index
    (nth (1- index) (tab-bar-tabs))))

(defun +consult--tab-preview ()
  "Preview function for tabs."
  (let ((orig-wc (current-window-configuration)))
    (lambda (action cand)
      (if (eq action 'exit)
          (set-window-configuration orig-wc nil t)
        (when cand
          (let* ((index (+tab-bar--index-from-candidate cand))
                 (tab (+tab-bar--tab-from-index index)))
            (when tab
              (if (eq (car tab) 'current-tab)
                  (set-window-configuration orig-wc nil t)
                (set-window-configuration (alist-get 'wc tab) nil t)))))))))

(defun +consult--tab-annotate (cand)
  "Annotate current tab."
  (when (equal (car (+tab-bar--tab-from-index (+tab-bar--index-from-candidate cand))) 'current-tab)
    "Current"))

(defun +consult--tab-action-select (cand)
  "Select tab from CAND."
  (tab-bar-select-tab (+tab-bar--index-from-candidate cand)))

(defvar +consult--tab-history
  "History of tab completion selections.")

(defvar +consult--source-tab-recent
  (list :name "Tab"
        :category 'tab
        :narrow ?t
        :default t
        :history '+consult--tab-history
        :items #'+tab-bar--completion-list-recent
        :annotate #'+consult--tab-annotate
        :action #'+consult--tab-action-select
        :state  #'+consult--tab-preview))

(defun +consult-tab ()
  "Select tab with completion and preview."
  (interactive)
  (consult--multi '(+consult--source-tab-recent) :prompt "Select tab: "))

(defun +consult-tab-close ()
  "Select tab to close it."
  (interactive)
  (tab-bar-close-tab (+tab-bar--index-from-candidate (car (consult--multi '(+consult--source-tab-recent) :prompt "Close tab: ")))))
