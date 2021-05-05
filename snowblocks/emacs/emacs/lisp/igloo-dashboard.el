(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Igloo emacs"
	dashboard-center-content t
	dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 5)
			(bookmarks . 5)
			(projects . 5)
			(agenda . 5)
			(registers . 5))))

(provide 'igloo-dashboard)
