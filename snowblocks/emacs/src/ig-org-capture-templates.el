;;; ig-org-capture-templates.el --- summary -*- lexical-binding: t -*-

(defconst *ig-ledger-accounts* '("CE" "Bunq" "Revolut"))
(defconst *ig-ledger-currencies* '("EUR"))
(defconst *ig-ledger-expense-categories* '(""))
(defconst *ig-ledger-file* (format-time-string "~/org/budget/ledger-%Y.dat"))

(string-join '("one" "two" "three") ", ")

(defun ig-ledger-account-string ()
  (string-join (append '("Account") *ig-ledger-accounts*) "|"))

(defun ig-ledger-currency-string ()
  (string-join (append '("Currency") *ig-ledger-currencies*) "|"))


(defun ig-org-template-ledger-expense-cash ()
  "Template for cash withdrawal"
  (format "%%(org-read-date) * %%^{Merchant}
    Expenses:Cash                                           %%^{Amount} %%^{%s}
    Assets:%%^{%s}" (ig-ledger-currency-string) (ig-ledger-account-string)))

(defun ig-org-template-ledger-expense-cash ()
  "Template for cash withdrawal"
  (format "%%(org-read-date) * Cash Withdrawal
    Expenses:Cash                                           %%^{Amount} %%^{%s}
    Assets:%%^{%s}" (ig-ledger-currency-string) (ig-ledger-account-string)))


(setq org-capture-templates
'(
;; Tasks
("t" "Task")

;; TODO
("tt" "TODO" entry (file "~/org/todo.org")
"* TODO %?
:DEADLINE:
:NOTE:"
:empty-lines 1)


;; Archive
("a" "Archive")

("aa" "Archive" entry (file "~/org/archive.org")
 "* %t  %?"
 :empty-lines 0)


;; Ledger

("l" "Ledger")

;; (defconst ig-ledger-accounts
;;   '("CE"))

("le" "Expenses" plain
 (file *ig-ledger-file*)
 (function ig-org-template-ledger-expense-card))

("lc" "Cash" plain
 (file *ig-ledger-file*)
 (function ig-org-template-ledger-expense-cash))



))


(provide 'ig-org-capture-templates)

;;; ig-org-capture-templates.el ends here
