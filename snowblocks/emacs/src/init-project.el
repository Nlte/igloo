;;; init-project.el --- -*- lexical-binding: t -*-

;;
;; Filename: init-project.el
;; Description: Configure Projectile

(defvar igloo-projectile-cache-limit 10000)

(defvar igloo-projectile-cache-blacklist '("~" "/tmp" "/"))

(defvar igloo-projectile-fd-binary
  (cl-find-if #'executable-find (list "fdfind" "fd"))
  "Find binary (fdfind on ubuntu, debian) and fd on other distribution")


(use-package projectile
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
  (projectile-mode 1)
  ;; In the interest of performance, we reduce the number of project root marker
  ;; files/directories projectile searches for when resolving the project root.
  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"  ; projectile's root marker
                  ".project"     ; doom project marker
                  ".git")        ; Git VCS root dir
                (when (executable-find "hg")
                  '(".hg"))      ; Mercurial VCS root dir
                (when (executable-find "bzr")
                  '(".bzr")))    ; Bazaar VCS root dir
        ;; This will be filled by other modules. We build this list manually so
        ;; projectile doesn't perform so many file checks every time it resolves
        ;; a project's root -- particularly when a file has no project.
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))
)


(use-package counsel-projectile)
(use-package persp-projectile)


(provide 'init-project)
;;; init-project.el ends here
