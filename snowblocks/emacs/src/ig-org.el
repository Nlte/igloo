;;; ig-org.el --- -*- lexical-binding: t -*-

(require 'org)
(require 'major-mode-hydra)

;; Definitions -----------------------------------------------------------------
(defvar igloo-org-projects-file "~/org/projects.org"
  "Projects org file.")

;; Lib -------------------------------------------------------------------------

(defun org-fast-tag-selection (current inherited table &optional todo-table)
  "Fast tag selection with single keys.
CURRENT is the current list of tags in the headline, INHERITED is the
list of inherited tags, and TABLE is an alist of tags and corresponding keys,
possibly with grouping information.  TODO-TABLE is a similar table with
TODO keywords, should these have keys assigned to them.
If the keys are nil, a-z are automatically assigned.
Returns the new tags string, or nil to not change the current settings."
  (let* ((fulltable (append table todo-table))
	 (maxlen (if (null fulltable) 0
		   (apply #'max
			  (mapcar (lambda (x)
				    (if (stringp (car x)) (string-width (car x))
				      0))
				  fulltable))))
	 (buf (current-buffer))
	 (expert (eq org-fast-tag-selection-single-key 'expert))
	 (tab-tags nil)
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 (i-face 'org-done)
	 (c-face 'org-todo)
	 tg cnt e c char c1 c2 ntable tbl rtn
	 ov-start ov-end ov-prefix
	 (exit-after-next org-fast-tag-selection-single-key)
	 (done-keywords org-done-keywords)
	 groups ingroup intaggroup)
    (save-excursion
      (beginning-of-line)
      (if (looking-at org-tag-line-re)
	  (setq ov-start (match-beginning 1)
		ov-end (match-end 1)
		ov-prefix "")
	(setq ov-start (1- (point-at-eol))
	      ov-end (1+ ov-start))
	(skip-chars-forward "^\n\r")
	(setq ov-prefix
	      (concat
	       (buffer-substring (1- (point)) (point))
	       (if (> (current-column) org-tags-column)
		   " "
		 (make-string (- org-tags-column (current-column)) ?\ ))))))
    (move-overlay org-tags-overlay ov-start ov-end)
    (save-excursion
      (save-window-excursion
	(if expert
	    (set-buffer (get-buffer-create " *Org tags*"))
	  (delete-other-windows)
	  (set-window-buffer (split-window-vertically) (get-buffer-create " *Org tags*"))
	  (org-switch-to-buffer-other-window " *Org tags*"))
	(erase-buffer)
	(setq-local org-done-keywords done-keywords)
	(org-fast-tag-insert "Inherited" inherited i-face "\n")
	(org-fast-tag-insert "Current" current c-face "\n\n")
	(org-fast-tag-show-exit exit-after-next)
	(org-set-current-tags-overlay current ov-prefix)
	(setq tbl fulltable char ?a cnt 0)
	(while (setq e (pop tbl))
	  (cond
	   ((eq (car e) :startgroup)
	    (push '() groups) (setq ingroup t)
	    (unless (zerop cnt)
	      (setq cnt 0)
	      (insert "\n"))
	    (insert (if (cdr e) (format "%s: " (cdr e)) "") "{ "))
	   ((eq (car e) :endgroup)
	    (setq ingroup nil cnt 0)
	    (insert "}" (if (cdr e) (format " (%s) " (cdr e)) "") "\n"))
	   ((eq (car e) :startgrouptag)
	    (setq intaggroup t)
	    (unless (zerop cnt)
	      (setq cnt 0)
	      (insert "\n"))
	    (insert "[ "))
	   ((eq (car e) :endgrouptag)
	    (setq intaggroup nil cnt 0)
	    (insert "]\n"))
	   ((equal e '(:newline))
	    (unless (zerop cnt)
	      (setq cnt 0)
	      (insert "\n")
	      (setq e (car tbl))
	      (while (equal (car tbl) '(:newline))
		(insert "\n")
		(setq tbl (cdr tbl)))))
	   ((equal e '(:grouptags)) (insert " : "))
	   (t
	    (setq tg (copy-sequence (car e)) c2 nil)
	    (if (cdr e)
		(setq c (cdr e))
	      ;; automatically assign a character.
	      (setq c1 (string-to-char
			(downcase (substring
				   tg (if (= (string-to-char tg) ?@) 1 0)))))
	      (if (or (rassoc c1 ntable) (rassoc c1 table))
		  (while (or (rassoc char ntable) (rassoc char table))
		    (setq char (1+ char)))
		(setq c2 c1))
	      (setq c (or c2 char)))
	    (when ingroup (push tg (car groups)))
	    (setq tg (org-add-props tg nil 'face
				    (cond
				     ((not (assoc tg table))
				      (org-get-todo-face tg))
				     ((member tg current) c-face)
				     ((member tg inherited) i-face))))
	    (when (equal (caar tbl) :grouptags)
	      (org-add-props tg nil 'face 'org-tag-group))
	    (when (and (zerop cnt) (not ingroup) (not intaggroup)) (insert "  "))
	    (insert "[" c "] " tg (make-string
				   (- fwidth 4 (length tg)) ?\ ))
	    (push (cons tg c) ntable)
	    (when (= (cl-incf cnt) ncol)
	      (unless (memq (caar tbl) '(:endgroup :endgrouptag))
		(insert "\n")
		(when (or ingroup intaggroup) (insert "  ")))
	      (setq cnt 0)))))
	(setq ntable (nreverse ntable))
	(insert "\n")
	(goto-char (point-min))
	(unless expert (org-fit-window-to-buffer))
	(setq rtn
	      (catch 'exit
		(while t
		  (message "[a-z..]:toggle [SPC]:clear [RET]:accept [TAB]:edit [!] %sgroups%s"
			   (if (not groups) "no " "")
			   (if expert " [C-c]:window" (if exit-after-next " [C-c]:single" " [C-c]:multi")))
		  (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
		  (setq org-last-tag-selection-key c)
		  (cond
		   ((= c ?\r) (throw 'exit t))
		   ((= c ?!)
		    (setq groups (not groups))
		    (goto-char (point-min))
		    (while (re-search-forward "[{}]" nil t) (replace-match " ")))
		   ((= c ?\C-c)
		    (if (not expert)
			(org-fast-tag-show-exit
			 (setq exit-after-next (not exit-after-next)))
		      (setq expert nil)
		      (delete-other-windows)
		      (set-window-buffer (split-window-vertically) " *Org tags*")
		      (org-switch-to-buffer-other-window " *Org tags*")
		      (org-fit-window-to-buffer)))
		   ((or (= c ?\C-g)
			(and (= c ?q) (not (rassoc c ntable))))
		    (delete-overlay org-tags-overlay)
		    (setq quit-flag t))
		   ((= c ?\ )
		    (setq current nil)
		    (when exit-after-next (setq exit-after-next 'now)))
		   ((= c ?\t)
                    (condition-case nil
                        (unless tab-tags
                          (setq tab-tags
                                (delq nil
                                      (mapcar (lambda (x)
                                                (let ((item (car-safe x)))
                                                  (and (stringp item)
                                                       (list item))))
                                              (org--tag-add-to-alist
                                               (with-current-buffer buf
                                                 (org-get-buffer-tags))
                                               table))))))
                    (setq tg (completing-read "Tag: " tab-tags))
		    (when (string-match "\\S-" tg)
		      (cl-pushnew (list tg) tab-tags :test #'equal)
		      (if (member tg current)
			  (setq current (delete tg current))
			(push tg current)))
		    (when exit-after-next (setq exit-after-next 'now)))
		   ((setq e (rassoc c todo-table) tg (car e))
		    (with-current-buffer buf
		      (save-excursion (org-todo tg)))
		    (when exit-after-next (setq exit-after-next 'now)))
		   ((setq e (rassoc c ntable) tg (car e))
		    (if (member tg current)
			(setq current (delete tg current))
		      (cl-loop for g in groups do
			       (when (member tg g)
				 (dolist (x g) (setq current (delete x current)))))
		      (push tg current))
		    (when exit-after-next (setq exit-after-next 'now))))

		  ;; Create a sorted list
		  (setq current
			(sort current
			      (lambda (a b)
				(assoc b (cdr (memq (assoc a ntable) ntable))))))
		  (when (eq exit-after-next 'now) (throw 'exit t))
		  (goto-char (point-min))
		  (beginning-of-line 2)
		  (delete-region (point) (point-at-eol))
		  (org-fast-tag-insert "Current" current c-face)
		  (org-set-current-tags-overlay current ov-prefix)
		  (let ((tag-re (concat "\\[.\\] \\(" org-tag-re "\\)")))
		    (while (re-search-forward tag-re nil t)
		      (let ((tag (match-string 1)))
			(add-text-properties
			 (match-beginning 1) (match-end 1)
			 (list 'face
			       (cond
				((member tag current) c-face)
				((member tag inherited) i-face)
				(t (get-text-property (match-beginning 1) '
						      face))))))))
		  (goto-char (point-min)))))
	(delete-overlay org-tags-overlay)
	(if rtn
	    (mapconcat 'identity current ":")
	  nil)))))

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

(defun igloo-org-open-projects ()
  (interactive)
  (if (not (file-exists-p igloo-org-projects-file))
      (error (format "File not found: %s" igloo-org-projects-file)))
  (find-file igloo-org-projects-file))

;;;###autoload
(defun igloo-org-recalculate-table ()
  (interactive)
  ;; Call org recalc twice to refresh the entire table.
  (org-table-recalculate)
  (org-table-recalculate))


;; Config ----------------------------------------------------------------------

;; Faces
(custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
(custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
(custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) "")
(custom-declare-face '+org-todo-project '((t (:inherit (bold igloo-string)))) "")

;; UI
(setq org-startup-folded t
      org-hide-leading-stars t
      org-startup-indented t
      org-return-follows-link t
      org-enforce-todo-dependencies t
      org-edit-src-content-indentation 0
      org-fast-tag-selection-single-key 'expert
      org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

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
  "Log STRT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "STRT")
             (not (org-entry-get nil "STRT")))
    (org-entry-put nil "STRT" (format-time-string "[%Y-%m-%d %H:%M:%S]"))))
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
    ("u" igloo-org-recalculate-table "Recalculate"))
   ""
   (("c" org-table-insert-column "Insert column")
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


;; Org refile ------------------------------------------------------------------
(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
;; (setq org-refile-use-cache t)


;; Org capture -----------------------------------------------------------------
;; Make org capture open in full window and restore previous arrangement when done.
(add-hook 'org-capture-mode-hook 'delete-other-windows)

(require 'ig-org-capture-templates)

;; Org keymap ------------------------------------------------------------------
;; Override TAB key to use custom org-cycle function
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'igloo-org-cycle)


;; Org babel -------------------------------------------------------------------
(use-package org-babel
  :defer t
  :init
  ;; Don't ask to eval code in SRC blocks.
  (setq org-confirm-babel-evaluate nil))

;; Avoid `org-babel-do-load-languages' since it does an eager require.

;; Part of org stdlib
(use-package ob-python
  :defer t
  :commands
  (org-babel-execute:python))

;; Part of org stdlib
(use-package ob-shell
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

;; Part of org stdlib
(use-package ob-C
  :defer t
  :commands
  (org-babel-execute:C
   org-babel-expand-body:C))


(provide 'ig-org)
;;; ig-org.el ends here
