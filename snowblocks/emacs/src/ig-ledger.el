;;; ledger.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package ledger-mode
  :straight t
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :config
  (setq ledger-post-amount-alignment-at :decimal))

(provide 'ledger)

;;; ledger.el ends here
