;;; ig-lsp.el --- -*- lexical-binding: t -*-

(use-package lsp-mode
  :straight t
  :init
  :hook ((python-mode . lsp))
  :config
  (setq lsp-completion-provider :company-capf
        ;; lsp-ui nil
        lsp-idle-delay 0.1
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-doc-show-with-mouse nil))

(use-package company
  :straight t
  :defer 2
  :diminish
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-format-margin-function nil))

(use-package company-box
  :straight t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(provide 'ig-lsp)
;;; ig-lsp.el ends here
