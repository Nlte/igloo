(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c C-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/Github")
    (setq projectile-project-search-path '("~/Documents/Github"))))

(use-package counsel-projectile)


(provide 'nano-projectile)
