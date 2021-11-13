;;; ig-lsp.el --- -*- lexical-binding: t -*-

(use-package lsp-mode
  :straight t
  :init
  :hook ((python-mode . lsp))
  :config
  (setq lsp-completion-provider :company-capf
        ;; lsp-ui nil
        lsp-idle-delay 0.1
        lsp-keep-workspace-alive nil
        lsp-enable-file-watchers nil
        lsp-prefer-flymake nil
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-doc-show-with-mouse nil))

(use-package company
  :straight t
  :defer 2
  :diminish
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-format-margin-function nil
        company-idle-delay 0.1
        ))

(use-package company-box
  :straight t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(provide 'ig-lsp)
;;; ig-lsp.el ends here
