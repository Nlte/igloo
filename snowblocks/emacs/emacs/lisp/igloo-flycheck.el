;;; igloo-flycheck.el --- Flycheck config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :hook
  (emacs-lisp-mode . flycheck-mode)
  (python-mode . flycheck-mode)
  ;; :custom
  ;; (flycheck-check-syntax-automatically '(idle-buffer-switch idle-change save))
  ;; (flycheck-display-errors-delay .01)
  ;; (flycheck-emacs-lisp-load-path 'inherit)
  ;; (flycheck-indication-mode 'right-fringe)
  :config
  ;; (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow [255] nil nil '(center t))
  )

(provide 'igloo-flycheck)

;;; igloo-flycheck.el ends here
