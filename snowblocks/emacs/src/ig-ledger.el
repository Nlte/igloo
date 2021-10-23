;;; ig-ledger.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(use-package ledger-mode
  :straight t
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :config
  (setq ledger-post-amount-alignment-at :decimal))

(provide 'ig-ledger)

;;; ig-ledger.el ends here
