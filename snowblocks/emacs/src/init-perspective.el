;;; init-perspective.el --- -*- lexical-binding: t -*-

;;
;; Filename: init-perspective.el
;; Description: Configure perspective management
(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)   ; or use a nicer switcher, see below
  :config
  (persp-mode))

(provide 'init-perspective)
;;; init-perspective.el ends here
