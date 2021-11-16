;;; ig-ledger.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:


(use-package ledger-mode
  :straight t
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :config
  (setq ledger-post-amount-alignment-at :decimal)
  :mode-hydra
  (ledger-mode
   (:title "Ledger mode" :color teal :idle 0.5 :quit-key ("q" "<escape>") :separator " ")
   ("Report"
    (("r" ledger-report "ledger-report")))))

;; ledger-report-mode does not derive from ledger-mode
;; hence the need to duplicate the major hydra
(use-package ledger-report-mode
  :config
  :mode-hydra
  (ledger-report-mode
   (:title "Ledger mode" :color teal :idle 0.5 :quit-key ("q" "<escape>") :separator " ")
   ("Report"
    (("r" ledger-report "ledger-report")))))

(provide 'ig-ledger)

;;; ig-ledger.el ends here
