;;; ig-lib.el --- summary -*- lexical-binding: t -*-

(defun igloo/arrayify (start end quote)
    "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
    (interactive "r\nMQuote: ")
    (let ((insertion
           (mapconcat
            (lambda (x) (format "%s%s%s" quote x quote))
            (split-string (buffer-substring start end)) ", ")))
      (delete-region start end)
      (insert insertion)))

(defun igloo/fill-to-end ()
  (interactive)
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char ?-))))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun igloo/org-days-between (start end)
  "Number of days between START and END.
This includes START and END."
  (1+ (- (calendar-absolute-from-gregorian (org-date-to-gregorian end))
         (calendar-absolute-from-gregorian (org-date-to-gregorian start)))))



(provide 'ig-lib)

;;; ig-lib.el ends here
