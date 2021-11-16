;;; ig-modeline.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;; Reference:
;; Nano modeline
;; https://github.com/rougier/nano-emacs/blob/master/nano-modeline.el

;;; Code:


(defun ig-modeline-vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun ig-modeline-mode-name ()
  (if (listp mode-name) (car mode-name) mode-name))

;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))


(defun ig-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (space-up       +0.15)
         (space-down     -0.20)
	 (prefix (cond ((string= status "RO")
			        (propertize (if (window-dedicated-p)" -- " " RO ")
                                'face 'igloo-popout))
                   ((string= status "**")
			        (propertize (if (window-dedicated-p)" -- " " ** ")
                                'face 'igloo-critical))
                   ((string= status "RW")
			        (propertize (if (window-dedicated-p)" -- " " RW ")
                                'face 'igloo-faded))
                   (t (propertize status 'face 'igloo-popout))))
         (left (concat
                (propertize " "  'face 'igloo-default
			    'display `(raise ,space-up))
                (propertize name 'face 'igloo-strong)
                (propertize " "  'face 'igloo-default
			    'display `(raise ,space-down))
		(propertize primary 'face 'igloo-default)))
         (right (concat secondary " "))
         (available-width (- (window-total-width) 
			     (length prefix) (length left) (length right)
			     (/ (window-right-divider-width) char-width)))
	 (available-width (max 1 available-width)))
    (concat prefix
	    left
	    (propertize (make-string available-width ?\ )
                        'face 'igloo-default)
	    (propertize right 'face `(:inherit igloo-default
                                      :foreground ,igloo-faded)))))


(defun ig-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))



;; prog-mode -------------------------------------------------------------------
(defun ig-modeline-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun ig-modeline-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun ig-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (ig-modeline-mode-name))
          (branch      (ig-modeline-vc-branch))
          (position    (format-mode-line "%l:%c")))
      (ig-modeline-compose (ig-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             position)))


(defun ig-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)
  (setq-default bottom-line-format
  '((:eval
     (cond ((ig-modeline-prog-mode-p)            (ig-modeline-default-mode))
           ((ig-modeline-text-mode-p)            (ig-modeline-default-mode))
           (t                                    (ig-modeline-default-mode))))))
  (setq-default mode-line-format bottom-line-format))


(ig-modeline-default-mode)

(provide 'ig-modeline)

;;; ig-modeline.el ends here
