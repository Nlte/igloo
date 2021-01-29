;;; igloo-snippet.el --- Snippet config -*- lexical-binding: t -*-
(use-package yasnippet
  :bind
  (:map yas-minor-mode-map
   ("TAB" . nil)
   ([tab] . nil))
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :custom
  (yas-verbosity 2)
  :config
  (yas-reload-all))

(provide 'igloo-snippet)
;;; igloo-snippet.el ends here
