;;; ig-web.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'url-parse)

(defun ig-web-url-fetch-as-string (url)
  "Retrieve synchronously the URL content as a string."
  (with-temp-buffer
    (url-insert-file-contents url)
    (buffer-string)))

(defun ig-web-url-fetch (url)
    (let ((dom (with-temp-buffer 
          (url-insert-file-contents url)
	      (libxml-parse-html-region (point-min) (point-max)))))
     ))

(defun ig-web-parse-html (url)
  (with-temp-buffer
    (url-insert-file-contents url)
    (libxml-parse-html-region (point-min) (point-max))))

(defun ig-web-scrape-dom-by-id (dom value)
  (dom-by-id dom value))

(defun ig-web-scrape-dom-by-class (dom value)
  (dom-by-class dom value))

(defun ig-web-scrape-dom (dom by value)
  (cond
    ((equal by "id") (ig-web-scrape-dom-by-id dom value))
    ((equal by "class") (ig-web-scrape-dom-by-class dom value))
    (t (error "Unkown BY, should be 'id' or 'class'"))))


(provide 'ig-web)

;;; ig-web.el ends here
