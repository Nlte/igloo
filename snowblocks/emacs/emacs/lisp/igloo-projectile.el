(require 'igloo-cache)

(use-package projectile
    :hook
    (after-init . projectile-mode)
    :init
    (setq-default
      projectile-cache-file (ig/cache-concat "projectile/cache")
      projectile-known-projects-file (ig/cache-concat "projectile/projects.eld"))
    :custom
    (projectile-completion-system 'ivy)
    (projectile-dynamic-mode-line nil)
    (projectile-enable-caching t)
    (projectile-track-known-projects-automatically nil)
    :config
    (projectile-mode 1))

(use-package counsel-projectile
    :ensure t)


(provide 'igloo-projectile)
