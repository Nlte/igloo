;;; lsp.el --- -*- lexical-binding: t -*-

;; (use-package lsp-mode
;;   :straight t
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook ((python-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

(use-package eglot
  :straight t)


(provide 'lsp)
;;; lsp.el ends here
