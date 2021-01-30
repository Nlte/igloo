;;; igloo-config.el --- Emacs config dir and extra -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar igloo-config-dir "~/.emacs.d/")


(defun ig/open-private-config ()
  (interactive)
  (counsel-find-file igloo-config-dir))

(provide 'igloo-config)

;;; igloo-config.el ends here
