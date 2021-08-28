;;; init-git.el --- -*- lexical-binding: t -*-

;;
;; Filename: init-git.el
;; Description: Configure Git

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
	;; cursor appearance
	evil-normal-state-cursor 'box
	evil-insert-state-cursor 'bar
	evil-visual-state-cursor 'hollow
	;; Search
	evil-symbol-word-search t)
  :config
  (evil-mode 1))


(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


(provide 'init-git)
;;; init-git.el ends here
