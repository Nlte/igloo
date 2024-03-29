;;; ig-evil.el --- Evil mode -*- lexical-binding: t -*-

(require 'use-package)

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil
	;; cursor appearance
	evil-normal-state-cursor 'box
	evil-insert-state-cursor 'bar
	evil-visual-state-cursor 'hollow
	;; Navigation
	evil-want-C-u-scroll t
	;; Search
	evil-symbol-word-search t)
  :config
  (evil-mode 1))


(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-commentary 
  :straight t
  ; :quelpa (evil-commentary :fetcher github :repo "linktohack/evil-commentary")
  :config
  (evil-commentary-mode 1))


(provide 'ig-evil)
;;; ig-evil.el ends here
