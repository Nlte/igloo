;;; ig-org-capture-templates.el --- summary -*- lexical-binding: t -*-

(defconst *ig-ledger-accounts* '("CE" "Bunq" "Revolut" "Boursorama"))
(defconst *ig-ledger-currencies* '("EUR"))

(defconst *ig-ledger-expense-categories*
  '(
    "Housing"
    "Housing:Rent"
    "Housing:Taxes"
    "Housing:Repairs"
    "Transportation"
    "Transportation:Parkingfees"
    "Transportation:Travelcard"
    "Food"
    "Food:Groceries"
    "Food:Restaurants"
    "Utilities"
    "Utilities:Electricity"
    "Utilities:Gaz"
    "Utilities:Water"
    "Utilities:Garbage"
    "Utilities:Phones"
    "Utilities:Internet"
    "Utilities:Banking"
    "Clothing"
    "Medical"
    "Medical:Primary care"
    "Medical:Medications"
    "Insurance"
    "Insurance:Health"
    "Insurance:Home"
    "Insurance:Life"
    "Supplies"
    "Supplies:Cleaning"
    "Supplies:Tools"
    "Personal"
    "Personal:Gym"
    "Personal:Hairsalon"
    "Personal:Cosmetics"
    "Personal:Subscriptions"
    "Personal:Purchases"
    "Education"
    "Education:Books"
    "Gifts"
    "Gifts:Birthday"
    "Gifts:Christmas"
    "Entertainment"
    "Entertainment:Subscriptions"
    "Entertainment:Movies"
    "Entertainment:Games"
    "Entertainment:Bars"
    "Entertainment:Concerts"
    "Entertainment:Vacations"))

(defconst *ig-ledger-file* (format-time-string "~/org/budget/ledger-%Y.dat"))

(defun ig-ledger-account-string ()
  (string-join (append '("Account") *ig-ledger-accounts*) "|"))

(defun ig-ledger-currency-string ()
  (string-join (append '("Currency") *ig-ledger-currencies*) "|"))

(defun ig-ledger-expenses-category-string ()
  (string-join (append '("Category") *ig-ledger-expense-categories*) "|"))

(defun ig-org-template-ledger-expense-normal ()
  "Template for cash withdrawal"
  (format "%%(org-read-date) * %%^{Merchant}
    ; :Note: %%^{Note}
    Expenses:%%^{%s}                                           %%^{Amount} %%^{%s}
    Assets:%%^{%s}"
          (ig-ledger-expenses-category-string)
          (ig-ledger-currency-string)
          (ig-ledger-account-string)))

(defun ig-org-template-ledger-expense-cash ()
  "Template for expenses withdrawal"
  (format "%%(org-read-date) * Cash Withdrawal
    Expenses:Cash                                           %%^{Amount} %%^{%s}
    Assets:%%^{%s}" (ig-ledger-currency-string) (ig-ledger-account-string)))

(defun ig-org-template-ledger-bank-transfer ()
  "Template for bank transfers"
  (format "%%(org-read-date) * Bank Transfer
    Assets:%%^{%s}                                           %%^{Amount} %%^{%s}
    Assets:%%^{%s}"
          ((lambda () (string-join (append '("To") *ig-ledger-accounts*) "|")))
          (ig-ledger-currency-string)
          ((lambda () (string-join (append '("From") *ig-ledger-accounts*) "|")))))

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
 (function ig-org-template-ledger-expense-normal))

("lt" "Transfer" plain
 (file *ig-ledger-file*)
 (function ig-org-template-ledger-bank-transfer))

("lc" "Cash" plain
 (file *ig-ledger-file*)
 (function ig-org-template-ledger-expense-cash))



))


(provide 'ig-org-capture-templates)

;;; ig-org-capture-templates.el ends here
