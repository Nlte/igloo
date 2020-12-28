(defun nano/fill-to-end ()
  (interactive)
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char ?-))))

;;;###autoload
(defun nano::project-find-file ()
  )

;;;###autoload
(defun nano/open-config ()
  "Open the config directory."
  (interactive)
  (nano::project-find-file nano-config))


(provide 'nano-stdlib)
