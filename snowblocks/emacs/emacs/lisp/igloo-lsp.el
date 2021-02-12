;;; igloo-lsp.el --- LSP config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(use-package lsp-mode
  :hook (prog-mode . lsp)
  :init
  (setq-default
   lsp-idle-delay 0.200 ;; to tune (how often lsp refresh)
   lsp-completion-provider :capf
   lsp-headerline-breadcrumb-enable nil
   lsp-lens-enable nil
   lsp-ui-sideline-enable nil
   lsp-ui-doc-enable nil
   ))

(use-package lsp-ui)

(provide 'igloo-lsp)
;;; igloo-lsp.el ends here
