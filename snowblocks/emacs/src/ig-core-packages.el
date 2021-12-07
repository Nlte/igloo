;;; ig-core-packages.el --- Core packages -*- lexical-binding: t -*-

(straight-use-package 'use-package)

(use-package gcmh
  :straight t
  :config
  (gcmh-mode 1))

(use-package exec-path-from-shell
  :straight t
  :init
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)))

(use-package interaction-log
  :straight t)

(use-package which-key
  :straight t)

(provide 'ig-core-packages)
;;; ig-core-packages.el ends here
