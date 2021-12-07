;;; ig-org-lib.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'org-element) ;; stdlib

;;;###autoload
(defun igloo-org-parse-file (filename)
  "Return the org parse tree for FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (org-element-parse-buffer)))

;;;###autoload
(defun igloo-org-parse-current-buffer ()
  "Return the org parse tree for the current buffer."
  (with-current-buffer
      (org-element-parse-buffer)))

;;;###autoload
(defun igloo-org-yield-headlines (tree)
  "Return the headlines for org parse tree TREE."
  (org-element-map tree 'headline
    (lambda (head) head)))

;;;###autoload
(defun igloo-org-yield-first-level-headlines (tree)
  "Return the first headlines of the org parse tree TREE."
    (org-element-map tree 'headline
    (lambda (head)
        (let ((parent (org-element-property :parent head)))
        (if (not (eq (org-element-type parent) 'headline))
            head)))))

;;;###autoload
(defun igloo-org-yield-first-level-headlines-names (tree)
  "Return the first headlines of the org parse tree TREE."
    (org-element-map tree 'headline
    (lambda (head)
        (let ((parent (org-element-property :parent head)))
        (if (not (eq (org-element-type parent) 'headline))
            (org-element-property :raw-value head))))))

;;;###autoload
(defun igloo-org-yield-headlines-by-name (tree name)
  "Return the headlines of the org parse tree TREE matching the name NAME."
  (org-element-map tree 'headline
    (lambda (head)
      (if (equal (org-element-property :raw-value head) name)
       head))))

;;;###autoload
(defun igloo-org-get-drawer-item-by-name (drawer name))

(igloo-org-yield-headlines-by-name (igloo-org-parse-file "~/org/appartments.org") "Table")
(org-element-map (igloo-org-parse-file "~/org/appartments.org") 'property-drawer
  (lambda (x) x))

(igloo-org-yield-headlines-by-name (igloo-org-parse-file "~/org/appartments.org") "Template table")

(defun igloo-org-get-keyword-key-value (kwd)
     (let ((data (cadr kwd)))
       (list (plist-get data :key)
             (plist-get data :value))))

(defun igloo-org-current-buffer-get-pay ()
    (nth 1
    (assoc "PAY"
    (org-element-map (org-element-parse-buffer 'greater-element)
        '(keyword)
        #'igloo-org-get-keyword-key-value))))

(defun igloo-org-file-get-pay (file)
    (with-current-buffer (find-file-noselect file)
            (igloo-org-current-buffer-get-pay)))

(defun igloo-org-file-get-rent (file)
  (with-current-buffer (find-file-noselect file)
    (igloo-org-current-buffer-get-rent)))

(defun igloo-org-current-buffer-get-rent ()
  (nth 1
   (assoc "RENT"
          (org-element-map (org-element-parse-buffer 'greater-element)
              '(keyword)
            #'igloo-org-get-keyword-key-value))))

(defun igloo-org-get-property-by-name (name)
  (org-entry-get (point) name))


(igloo-org-file-get-rent "~/org/appartments.org")

(provide 'ig-org-lib)
;;; ig-org-lib.el ends here
