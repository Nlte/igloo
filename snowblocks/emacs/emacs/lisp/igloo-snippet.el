;;; igloo-snippet.el --- Snippet config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :custom
  (yas-verbosity 2)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets)

(provide 'igloo-snippet)

;;; igloo-snippet.el ends here
