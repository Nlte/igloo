;;; ig-snippets.el --- -*- lexical-binding: t -*-
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(provide 'ig-snippets)
;;; ig-snippets.el ends here
