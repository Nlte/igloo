;;; igloo-tree.el --- Tree config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons)
(use-package neotree)

(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

(provide 'igloo-tree)
;;; igloo-tree.el ends here
