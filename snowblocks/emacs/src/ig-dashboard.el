;;; ig-dashboard.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'org)

;;;###autoload
(defmacro igloo-dashboard-define-minor-mode (name)
  "Macro to define minor modes for dashboards."
  `(define-minor-mode ,name
     ,(format "%s" name)
     :init nil
     :lighter ,(format "%s" name)
     :keymap (make-sparse-keymap)
     (when ,name
       (read-only-mode))))


;;;###autoload
(defmacro igloo-dashboard-define-key (key action)
  "Macro to define a key for the dashboard."
  (if (bound-and-true-p evil-mode)
      `(evil-local-set-key
        'normal
        (kbd ,key)
        (eval (car (read-from-string
                    (format "(lambda () (interactive) (%s))" ,action)))))
    ))


;;;###autoload
(defun igloo-dashboard-parse-modename ()
  "Parse an org file for the minor mode name."
  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (k)
      (when (string= (org-element-property :key k) "MODE")
        (org-element-property :value k)))))


;;;###autoload
(defun igloo-dashboard-parse-and-set-keymap ()
  "Parse an org file for keybindings.
  eg. #+KEYMAP: q | kill-current-buffer
"

  (local-set-key (kbd "<return>") #'org-open-at-point)

  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (k)
      (when (string= (org-element-property :key k) "KEYMAP")
        (let* ((value (org-element-property :value k))
               (key   (string-trim (nth 0 (split-string value "|"))))
               (call  (string-trim (nth 1 (split-string value "|")))))
          (local-set-key
           (kbd key)
           (eval (car (read-from-string
                       (format "(lambda () (interactive) (%s))" call)))))
          (message
           "mu4e-dashboard: binding %s to %s"
           key
           (format "(lambda () (interactive) (%s))" call)))))))

;;;###autoload
(defun igloo-dashboard-parse-keymap ()
  "Parse an org file for keybindings."
  (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (k)
      (when (string= (org-element-property :key k) "KEYMAP")
        (let* ((value (org-element-property :value k))
               (key   (string-trim (nth 0 (split-string value "|"))))
               (call  (string-trim (nth 1 (split-string value "|")))))
          (list key call))))))

;;;###autoload
(defun igloo-dashboard-open (filename)
  "Opens a dashboard file and configures mode and keybindings."
  (find-file filename)
  (let* ((modename (intern (nth 0 (igloo-dashboard-parse-modename))))
         (keymap (igloo-dashboard-parse-keymap)))
    (if (not modename)
        (error (format "+MODE: undefined in dashboard '%s'" filename)))
    (message (format "Creating dashboard minor mode: %s" modename))
    (igloo-dashboard-define-minor-mode modename)
    (modename)
    (dolist (x keymap)
      (let* ((key (nth 0 x))
             (action (nth 1 x)))
        (message (format "ig-dashboard.el: binding %s to %s" key action))
        (igloo-dashboard-define-key key action))
      )
    ))

;;;###autoload
(defun igloo-dashboard-close ()
  "Closes a dashboard (kill buffer and close window)."
  (kill-current-buffer)
  (delete-window))


;; Config ----------------------------------------------------------------------
(defun igloo-dashboard-open-email ()
  "Open email dashboard."
  (interactive)
  (igloo-dashboard-open "~/org/mu4e-dashboard-main.org"))

(defun igloo-dashboard-open-test ()
  "Open test dashboard."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (igloo-dashboard-open "~/org/dashboard-test.org"))


(provide 'ig-dashboard)

;;; ig-dashboard.el ends here
