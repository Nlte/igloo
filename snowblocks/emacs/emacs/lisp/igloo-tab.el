;;; igloo-tab.el --- Tab config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
 (use-package centaur-tabs
   :after (evil)
   :config
   (centaur-tabs-mode t)
   (centaur-tabs-group-by-projectile-project)
   (setq centaur-tabs-style "bar"
	  centaur-tabs-set-bar 'under
	  x-underline-at-descent-line t)
   (centaur-tabs-headline-match)
   ;; (setq centaur-tabs-gray-out-icons 'buffer)
   ;; (centaur-tabs-enable-buffer-reordering)
   ;; (setq centaur-tabs-adjust-buffer-order t)
   (setq uniquify-separator "/")
   (setq uniquify-buffer-name-style 'forward)
   :hook
   ;; register non centaur tab mode here with centaur-tabs-local-mode
   ;; ie. pytest comint etc.
   ;; to prevent loosing the buffer excursion when closing the pytest buffer
   (python-pytest-mode . centaur-tabs-local-mode)
   (dashboard-mode . centaur-tabs-local-mode)
   (term-mode . centaur-tabs-local-mode)
   (calendar-mode . centaur-tabs-local-mode)
   (org-agenda-mode . centaur-tabs-local-mode)
   (helpful-mode . centaur-tabs-local-mode)
   :bind
   ("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)
   ("C-c t s" . centaur-tabs-counsel-switch-group)
   ("C-c t p" . centaur-tabs-group-by-projectile-project)
   ("C-c t g" . centaur-tabs-group-buffer-groups)
   (:map evil-normal-state-map
	  ("g t" . centaur-tabs-forward)
	  ("g T" . centaur-tabs-backward)))


(defun igloo-ivy/centaur-tabs-switch-group ()
  "Display a list of current buffer groups using Counsel."
  (interactive)
  (when (featurep 'ivy)
    (require 'ivy)
    (ivy-read
     "Switch to group:"
     (centaur-tabs-get-groups)
     :action #'centaur-tabs-switch-group
     :caller 'centaur-tabs-counsel-switch-group)))


(provide 'igloo-tab)

;; (require 'centaur-tabs-functions)
;; (message (centaur-tabs-get-groups))
;;; igloo-tab.el ends here
