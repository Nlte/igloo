;;; ig-lang-org.el --- -*- lexical-binding: t -*-

;; Custom lib
(require 'ig-org-lib)

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


;; Org capture
(require 'ig-org-capture-templates)
;; (setq 'org-capture-templates
;;       '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
;;          "* [ ] %?\n  %i\n  %a")
;;         ("j" "Journal" entry (file+datetree "~/org/journal.org")
;;          "* %?\nEntered on %U\n  %i\n  %a")
;;         ("a" "Archive" entry (file "~/org/archive.org")
;;          "* %U %?\n")))

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
   (("t" (ig-open-hydra ig-hydra-org-table/body) "Table"))))


(use-package org-web-tools
  :straight t)

(provide 'ig-lang-org)
;;; ig-lang-org.el ends here
