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

(defun normalize-price (price-string)
  (string-to-number (replace-regexp-in-string "[^0-9.]" "" price-string)))

(defun ig-web-openrent-parse-price (url)
  (normalize-price
    (dom-text
        (ig-web-scrape-dom
            (ig-web-parse-html url) 
            "class"
            "perMonthPrice price-title"))))

(defun ig-web-rightmove-parse-price (url)
  (normalize-price
    (dom-text
        (nth 0 (dom-children
                (ig-web-scrape-dom
                    (ig-web-parse-html url) 
                    "class"
                    "_1gfnqJ3Vtd1z40MlC0MzXu"))))))

(defun ig-web-realestate-parse-price (url)
    (let ((host (url-host (url-generic-parse-url url))))
      (cond ((equal host "www.rightmove.co.uk")
             (ig-web-rightmove-parse-price url))
            ((equal host "www.openrent.co.uk")
             (ig-web-openrent-parse-price url))
            (t (error (format "unkown host %s" host))))))

;; (ig-web-realestate-parse-price "https://www.rightmove.co.uk/properties/116804210#/?channel=RES_LET")
;; (ig-web-rightmove-parse-price "https://www.rightmove.co.uk/properties/116804210#/?channel=RES_LET")
;; (ig-web-realestate-parse-price "https://www.openrent.co.uk/property-to-rent/london/1-bed-flat-woodstock-road-n4/1263533")

;; (normalize-price "112.4$")

(defun ig-appartment-insert-cost-table (x)
(interactive "URL:")
  (message x))

(defun ig-appartment-insert-cost-table (url)
  "Prompt user to enter a string, with input history support."
  (interactive
   (list
    (read-string "URL:")))
  (if (= (length url) 0)
      (insert ig-appartment-cost-table)
    (message "fetching the price from url")))



(provide 'ig-web)

;;; ig-web.el ends here
