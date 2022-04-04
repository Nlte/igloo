;; ig-editor.el --- Editing in emacs -*- lexical-binding: t; -*-

;;; Aggressive indent mode
(use-package aggressive-indent
  :straight t
  :config
  (global-aggressive-indent-mode 1))

;; Expand region
;; Auto hilighting with smart rules
(use-package expand-region
  :straight t
  :config 
  (global-set-key (kbd "S-SPC") #'er/expand-region))

(use-package company
             :straight t
             :config
             (company-mode))


(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(provide 'ig-editor)

;;; ig-editor.el ends here
