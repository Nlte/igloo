(require 'igloo-cache)

(use-package projectile
    :init
    (setq-default
      projectile-cache-file (concat igloo-cache-dir "projectile.cache")
      projectile-known-projects-file (concat igloo-cache-dir "projectile.projects"))
    :custom
    (projectile-completion-system 'ivy)
    (projectile-dynamic-mode-line nil)
    (projectile-enable-caching t)
    ;; (projectile-track-known-projects-automatically nil)
    :config
    (projectile-mode 1))

(use-package counsel-projectile
    :ensure t)


(provide 'igloo-projectile)
