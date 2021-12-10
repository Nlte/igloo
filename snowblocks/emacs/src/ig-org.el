;;; ig-org.el --- -*- lexical-binding: t -*-

(require 'org)
(require 'major-mode-hydra)

;; Lib -------------------------------------------------------------------------

;;;###autoload
(defun igloo-org-parse-file (filename)
  "Return the org parse tree for FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (org-element-parse-buffer)))

;;;###autoload
(defun igloo-org-parse-current-buffer ()
  "Return the org parse tree for the current buffer."
  (with-current-buffer
      (org-element-parse-buffer)))

;;;###autoload
(defun igloo-org-yield-headlines (tree)
  "Return the headlines for org parse tree TREE."
  (org-element-map tree 'headline
    (lambda (head) head)))

;;;###autoload
(defun igloo-org-yield-first-level-headlines (tree)
  "Return the first headlines of the org parse tree TREE."
    (org-element-map tree 'headline
    (lambda (head)
        (let ((parent (org-element-property :parent head)))
        (if (not (eq (org-element-type parent) 'headline))
            head)))))

;;;###autoload
(defun igloo-org-yield-first-level-headlines-names (tree)
  "Return the first headlines of the org parse tree TREE."
    (org-element-map tree 'headline
    (lambda (head)
        (let ((parent (org-element-property :parent head)))
        (if (not (eq (org-element-type parent) 'headline))
            (org-element-property :raw-value head))))))

;;;###autoload
(defun igloo-org-yield-headlines-by-name (tree name)
  "Return the headlines of the org parse tree TREE matching the name NAME."
  (org-element-map tree 'headline
    (lambda (head)
      (if (equal (org-element-property :raw-value head) name)
       head))))

(defun igloo-org-get-keyword-key-value (kwd)
     (let ((data (cadr kwd)))
       (list (plist-get data :key)
             (plist-get data :value))))

(defun igloo-org-current-buffer-get-pay ()
    (nth 1
    (assoc "PAY"
    (org-element-map (org-element-parse-buffer 'greater-element)
        '(keyword)
        #'igloo-org-get-keyword-key-value))))

(defun igloo-org-file-get-pay (file)
    (with-current-buffer (find-file-noselect file)
            (igloo-org-current-buffer-get-pay)))

(defun igloo-org-file-get-rent (file)
  (with-current-buffer (find-file-noselect file)
    (igloo-org-current-buffer-get-rent)))

(defun igloo-org-current-buffer-get-rent ()
  (nth 1
   (assoc "RENT"
          (org-element-map (org-element-parse-buffer 'greater-element)
              '(keyword)
            #'igloo-org-get-keyword-key-value))))

(defun igloo-org-get-property-by-name (name)
  (org-entry-get (point) name))

;;;###autoload
(defun +org-cycle-only-current-subtree-h (&optional arg)
  "Toggle the local fold at the point.
`org-cycle's standard behavior is to cycle between three levels: collapsed,
subtree and whole document."
  (interactive "P")
  (unless (eq this-command 'org-shifttab)
    (save-excursion
      (org-beginning-of-line)
      (let (invisible-p)
        (when (and (org-at-heading-p)
                   (or org-cycle-open-archived-trees
                       (not (member org-archive-tag (org-get-tags))))
                   (or (not arg)
                       (setq invisible-p (outline-invisible-p (line-end-position)))))
          (unless invisible-p
            (setq org-cycle-subtree-status 'subtree))
          (org-cycle-internal-local)
          t)))))

;; ;;;###autoload
;; (defun +org-cycle-only-current-subtree-h (&optional arg)
;;   "Toggle the local fold at the point.
;; `org-cycle's standard behavior is to cycle between three levels: collapsed,
;; subtree and whole document."
;;   (interactive "P")
;;   (unless (eq this-command 'org-shifttab)
;;     (save-excursion
;;       (org-beginning-of-line)
;;       (let (invisible-p)
;;         (when (and (org-at-heading-p)
;;                    (or org-cycle-open-archived-trees
;;                        (not (member org-archive-tag (org-get-tags))))
;;                    (or (not arg)
;;                        (setq invisible-p (outline-invisible-p (line-end-position)))))
;;           (unless invisible-p
;;             (setq org-cycle-subtree-status 'subtree))
;;           (org-cycle-internal-local)
;;           t)
;;         (when (org-at-property-block-p)
;;           (message "at property")
;;           (unless invisible-p
;;             (setq org-cycle-subtree-status 'subtree))
;;           (org-cycle-hide-drawers)
;;           t)
;;         ))))

;; Config ----------------------------------------------------------------------

;; Faces
(custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
(custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")

;; UI
(setq org-startup-folded t
      org-hide-leading-stars t
      org-startup-indented t
      org-enforce-todo-dependencies t
      org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

;; Environment
(setq org-directory "~/org"
      org-agenda-files (list "inbox.org" "agenda.org"))

;; Keywords
(setq org-todo-keywords
      '((sequence
         "TODO(t)"  ; A task that needs doing & is ready to do
         "PROJ(p)"  ; A project, which usually contains other tasks
         "LOOP(r)"  ; A recurring task
         "STRT(s)"  ; A task that is in progress
         "WAIT(w)"  ; Something external is holding up this task
         "HOLD(h)"  ; This task is paused/on hold because of me
         "IDEA(i)"  ; An unconfirmed and unapproved task or notion
         "|"
         "DONE(d)"  ; Task successfully completed
         "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
        (sequence
         "[ ](T)"   ; A task that needs doing
         "[-](S)"   ; Task is in progress
         "[?](W)"   ; Task is being held up or paused
         "|"
         "[X](D)")  ; Task was completed
        (sequence
         "|"
         "OKAY(o)"
         "YES(y)"
         "NO(n)"))
      org-todo-keyword-faces
      '(("[-]"  . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("[?]"  . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)
        ("NO"   . +org-todo-cancel)
        ("KILL" . +org-todo-cancel)))



(pretty-hydra-define ig-hydra-org-table 
  (:idle 0.3
    :color blue
    :body-pre (ig-hydra-reset)
    :quit-key ("q" "<escape>")
    :inherit (ig-base/heads)
    :separator " ")
  ("Table"
   (("t" org-table-create "Create table")
    ("e" org-table-edit-formulas "Edit formulas")
    ("c" org-table-insert-column "Insert column")
    ("r" org-table-insert-row "Insert row")
    ("l" org-table-move-column-right "Move column right")
    ("h" org-table-move-column-left "Move column left"))))


(major-mode-hydra-define org-mode
  (:idle 0.3
    :color blue
    :body-pre (ig-hydra-reset)
    :quit-key ("q" "<escape>")
    :inherit (ig-base/heads)
    :separator " ")
  ("Org"
   (("c" org-ctrl-c-ctrl-c "C-c C-c")
    ("t" (ig-open-hydra ig-hydra-org-table/body) "Table")
    ("e" org-set-effort "Set effort")
    ("p" org-set-property "Set property")
    ("r" org-refile "Refile"))))


;; Refile ----------------------------------------------------------------------
(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)


;; Change org-cycle to only cycle in subtrees
;; (with-eval-after-load 'org
;;   (defalias 'org-cycle-internal-local '+org-cycle-only-current-subtree-h))


;; Org agenda ------------------------------------------------------------------
(setq org-agenda-hide-tags-regexp ".")
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " %i %-12:c")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

;; Org capture
(require 'ig-org-capture-templates)


(provide 'ig-org)
;;; ig-org.el ends here
