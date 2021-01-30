;;; igloo-env.el --- Set up exec-path and env  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set default directory to $HOME
(defun igloo--setup-default-dir ()
  "Configure the default emacs directory."
    (setq default-directory "~")
    ; (cd (getenv "HOME"))
    ) ;; for windows

(defun igloo--setup-env ()
  (igloo--setup-default-dir)
  )

(use-package exec-path-from-shell)

(use-package direnv
 :config
 (direnv-mode))

(provide 'igloo-env)
;;; igloo-env.el ends here
