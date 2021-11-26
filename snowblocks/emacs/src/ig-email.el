;;; ig-email.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(defconst email-index-command "mu"
  "Command line tool to manage email indexing.")

(defconst email-sync-command "offlineimap"
  "Command line tool to manage email sync.")

(if (not (executable-find email-index-command))
    (warn (format "ig-email.el: %s command not found" email-index-command)))

(if (not (executable-find email-sync-command))
    (warn (format "ig-email.el: %s command not found" email-sync-command)))

;; (use-package mu4e
;;   :straight t)

(provide 'ig-email)

;;; ig-email.el ends here
