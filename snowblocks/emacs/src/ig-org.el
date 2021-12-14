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
      (org-element-parse-buffer))

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


;;;###autoload
(defun igloo-org-cycle (&optional arg)
  (interactive "P")
  (if (org-at-heading-p)
      (+org-cycle-only-current-subtree-h)
    (org-cycle)))


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
      org-return-follows-link t
      org-enforce-todo-dependencies t
      org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

;; (setq org-goto-interface 'outline-path-completion
;;       org-goto-max-level 10)

;; Environment
(setq org-directory "~/org")

;; Keywords
(setq org-todo-keywords
      '(
        ;; Main task states
        (sequence
         "TODO(t)"  ; Task that needs doing & is ready to do
         "NEXT(n)"  ; Next task to be completed
         "STRT(s)"  ; A task that is in progress
         "HOLD(h)"  ; Task is paused/on hold
         "|"
         "DONE(d)"  ; Task successfully completed
         )
        ;; Additional task states
         (sequence
         "PROJ(p)"  ; Project contains other tasks cf. jira epic
         "IDEA(i)"  ; An unconfirmed and unapproved task or notion
         "KILL(k)") ; Task cancelled or is no longer applicable
        )
      org-todo-keyword-faces
      '(("STRT" . +org-todo-active)
        ("HOLD" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)
        ("KILL" . +org-todo-cancel)))

;; Log -----------------------------------------------------------------------
(defun igloo-org-log-todo-next-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "STRT")))
    (org-entry-put nil "STRT" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'igloo-org-log-todo-next-date)

(setq org-log-done 'time)

;; Hydra ----------------------------------------------------------------------
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
    )
   " "
    (("e" org-set-effort "Set effort")
     ("p" org-set-property "Set property")
     ("d" org-deadline "Set deadline")
     ("s" org-schedule "Set schedule"))
   " "
   (("o" org-open-at-point "Open at point")
    ("r" org-refile "Refile")
    ("j" counsel-outline "Jump to heading"))))


(major-mode-hydra-define org-agenda-mode
  (:idle 0.3
         :color blue
         :body-pre (ig-hydra-reset)
         :quit-key ("q" "<escape>")
         :inherit (ig-base/heads)
         :separator " ")
  ("Org"
   (("c" org-ctrl-c-ctrl-c "C-c C-c")
    ("r" org-refile "Refile"))))


;; Org agenda ------------------------------------------------------------------
(setq org-agenda-files (list "inbox.org" "agenda.org" "projects.org"))
(setq org-agenda-hide-tags-regexp ".")
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " %i %-12:c")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))


(defun igloo-org-save-agenda-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda () 
                         (when (member (buffer-file-name) org-agenda-files) 
                           t)))
  (message "Saving org-agenda-files buffers... done"))

;; Add it after refile
(advice-add 'org-refile :after
            (lambda (&rest _)
              (igloo-org-save-agenda-buffers)))

(setq org-agenda-custom-commands
      '(("g" "GTD"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))


;; Refile ----------------------------------------------------------------------
(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
;; (setq org-refile-use-cache t)


;; Org capture
;; Make org capture open in full window and restore previous arrangement when done.
(add-hook 'org-capture-mode-hook 'delete-other-windows)
(require 'ig-org-capture-templates)

;; Keymap ----------------------------------------------------------------------
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'igloo-org-cycle)


(provide 'ig-org)
;;; ig-org.el ends here
