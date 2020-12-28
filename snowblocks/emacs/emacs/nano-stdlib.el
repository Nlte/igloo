(defun nano/fill-to-end ()
  (interactive)
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char ?-))))

;;;###autoload
(defun nano-project-find-file ())

;;;###autoload
(defun nano/open-config ()
  "Open the config directory."
  (interactive)
  (nano-project-find-file nano-config))

(defun nano--path (&rest segments)
  (let ((segments (delq nil segments))
        dir)
    (while segments
      (setq dir (expand-file-name (car segments) dir)
            segments (cdr segments)))
    dir))

;;;###autoload
(defun nano-glob (&rest segments)
  "Construct a path from SEGMENTS and expand glob patterns.
Returns nil if the path doesn't exist.
Ignores `nil' elements in SEGMENTS."
  (let* (case-fold-search
         (dir (apply #'nano--path segments)))
    (if (string-match-p "[[*?]" dir)
        (file-expand-wildcards dir t)
      (if (file-exists-p dir)
          dir))))


(provide 'nano-stdlib)
