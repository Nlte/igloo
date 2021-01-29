;;; igloo-env.el --- Set up exec-path and env  -*- lexical-binding: t -*-

;; Set default directory to $HOME
(defun igloo--setup-default-dir ()
    (setq default-directory "~")
    ; (cd (getenv "HOME"))
    ) ;; for windows

(defun igloo--setup-env ()
  (igloo--setup-default-dir)
  )

(add-to-list 'exec-path "/usr/local/bin")

(use-package direnv
 :config
 (direnv-mode))

(provide 'igloo-env)
;;; igloo-env.el ends here
