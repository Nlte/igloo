(require 'package)
(setq package-list '(
                   projectile
                   counsel-projectile
                   vterm
                   centaur-tabs
                   company
                   company-box
                   dashboard
                   evil
                   evil-collection
                   evil-nerd-commenter
                   lsp-python-ms
		   python-mode
                   python-pytest
                   neotree
                   direnv
                   yasnippet
                   yasnippet-snippets
                   lsp-mode
                   lsp-ui
                   flycheck
                   smex
                   counsel
                   ivy
                   ivy-rich
                   persp-mode
                   csv-mode
                   centaur-tabs
                   shell-pop
                   major-mode-hydra
                   magit
                   yaml-mode
                   general
                   doom-themes
                   flucui-themes
                   org-jira
                   ))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


