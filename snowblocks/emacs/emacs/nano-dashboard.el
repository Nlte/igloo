(defvar +nano-dashboard-name "*nano*"
  "The name to use for the dashboard buffer.")

;;;###autoload
(defun nano-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `nano-fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create nano-fallback-buffer-name)))


(defun +nano-dashboard-init-h ()
  "Initialise Dashboard."
  (unless noninteractive
    ;; Ensure the dashboard becomes Emac's default buffer when no other buffer
    (setq nano-fallback-buffer-name +nano-dashboard-name
          initial-buffer-choice #'nano-fallback-buffer)
    (when (equal (buffer-name) "*scratch*")
      (set-window-buffer nil (nano-fallback-buffer)))))

(provide 'nano-dashboard)
