(defconst ig/cache-directory
  (expand-file-name "~/.local/share/emacs/")
  "Directory where all cache files should be saved")

(setq make-backup-files nil
      create-lockfiles nil)

(defun ig/cache-concat (name)
  "Return the absolute path of NAME under `ig/cache-directory'."
  (let* ((directory (file-name-as-directory ig/cache-directory))
         (path (convert-standard-filename (concat directory name))))
    (make-directory (file-name-directory path) t)
    path))


(provide 'igloo-cache)
