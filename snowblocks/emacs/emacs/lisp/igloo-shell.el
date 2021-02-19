;;; igloo-shell.el --- Shell config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package shell-pop
;;   :config
;;   (setq shell-pop-window-position "bottom"
;;         shell-pop-window-size 30))


;; TODO if projectile mode then "projectile-run-shell" else shell
(defun igloo-pop-shell (arg)
  "Pop a shell in a side window.
Pass arg to ‘shell’."
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'shell))
      (current-buffer))
    '((side . bottom)))))

(provide 'igloo-shell)

;;; igloo-python.el ends here
