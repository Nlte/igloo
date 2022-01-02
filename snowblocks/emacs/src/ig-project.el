;;; ig-project.el --- Project management -*- lexical-binding: t -*-

;; Constants and lib -----------------------------------------------------------
(defvar igloo-projectile-cache-limit 10000)

(defvar igloo-projectile-cache-blacklist '("~" "/tmp" "/"))

(defvar igloo-projectile-fd-binary
  (cl-find-if #'executable-find (list "fdfind" "fd"))
  "Find binary (fdfind on ubuntu, debian) and fd on other distribution")

;; Compilation follows stdout
(setq compilation-scroll-output t)

;;;###autoload
(defun igloo-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))


;;;###autoload
(defun igloo-project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (igloo-project-root dir)
       t))


;; Projectile ------------------------------------------------------------------
(use-package projectile
  :straight t
  :init
  (setq projectile-cache-file (concat igloo-cache-dir "projectile.cache")
	    projectile-auto-discover nil
	    projectile-globally-ignored-files '(".DS_Store" "TAGS")
	    projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
	    projectile-known-projects-file (concat igloo-cache-dir "projectile.projects")
	    projectile-project-search-path '("~/github/" "~/")
	    projectile-ignored-projects '("~/")

        ;; The original `projectile-default-mode-line' can be expensive over
        ;; TRAMP, so we gimp it in remote buffers.
        projectile-mode-line-function
        (lambda ()
          (if (file-remote-p default-directory) ""
            (projectile-default-mode-line))))
  :config
  (projectile-mode)
  ;; In the interest of performance, we reduce the number of project root marker
  ;; files/directories projectile searches for when resolving the project root.
  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"  ; projectile's root marker
                  ".git")        ; Git VCS root dir
                (when (executable-find "hg")
                  '(".hg")))      ; Mercurial VCS root dir
        ;; This will be filled by other modules. We build this list manually so
        ;; projectile doesn't perform so many file checks every time it resolves
        ;; a project's root -- particularly when a file has no project.
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))
  )


(use-package counsel-projectile
  :straight t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (defun igloo/counsel-rg-project-at-point ()
    (interactive)
    (counsel-rg (thing-at-point 'symbol) (projectile-project-root)))
  (defun igloo/counsel-rg-project ()
    (interactive)
    (counsel-rg "" (projectile-project-root))))


;; Makefile --------------------------------------------------------------------
(use-package makefile-executor
  :straight t
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(defun ig-make-run ()
  (interactive)
  (makefile-executor-execute-target))

(defun ig-make-run-last ()
  (interactive)
  (makefile-executor-execute-last))


;; Envrc -----------------------------------------------------------------------
(defconst direnv-command "direnv"
  "Direnv command.")

(if (not (executable-find direnv-command))
    (warn (format "ig-project.el: %s command not found" direnv-command)))

(use-package envrc
  :straight t
  :config
  (envrc-global-mode)
  (advice-add 'lsp :before (lambda (&optional n) (envrc--update))))


(provide 'ig-project)
;;; ig-project.el ends here
