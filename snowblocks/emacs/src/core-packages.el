;;; core-packages.el --- Core packages -*- lexical-binding: t -*-

(straight-use-package 'use-package)

(use-package gcmh
  :straight t
  :config
  (gcmh-mode 1))

(provide 'core-packages)
;;; core-packages.el ends here
