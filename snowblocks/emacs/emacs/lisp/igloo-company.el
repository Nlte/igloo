;;; igloo-company.el --- Evil config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package company
  :hook
  (prog-mode . global-company-mode)
  :init
  (setq-default
    company-global-modes '(not org-mode shell-mode eshell-mode))
  :custom
  (company-backends '(company-capf))
  (company-minimum-prefix-length 1)
  (company-idle-delay 0))

;; (use-package company-box
;;   :hook
;;   (company-mode . company-box-mode)
;;   :custom
;;   (company-box-enable-icon nil)
;;   (company-box-max-candidates 50)
;;   (company-box-scrollbar nil)
;;   (company-box-show-single-candidate 'always))

(provide 'igloo-company)
;;; igloo-company.el ends here
