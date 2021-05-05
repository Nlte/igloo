;;; igloo-buffer.el --- Buffer lib -*- lexical-binding: t -*-
;;; Commentary:
;;; Code: 

(defvar +igloo-dashboard-name "*dashboard*"
  "The name to use for the dashboard buffer.")
(defvar igloo-fallback-buffer-name +igloo-dashboard-name
  "The name of the fallback buffer.")


;; Lib 

;;;###autoload
(defun igloo-fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `igloo-fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create igloo-fallback-buffer-name)))


(defun +ivy--switch-buffer (workspace other)
  (let ((current (not other))
        prompt action filter update unwind)
    (cond ((and workspace current)
           (setq prompt "Switch to workspace buffer: "
                 action #'ivy--switch-buffer-action
                 filter #'+ivy--is-workspace-other-buffer-p))
          (workspace
           (setq prompt "Switch to workspace buffer in other window: "
                 action #'ivy--switch-buffer-other-window-action
                 filter #'+ivy--is-workspace-buffer-p))
          (current
           (setq prompt "Switch to buffer: "
                 action #'ivy--switch-buffer-action))
          ((setq prompt "Switch to buffer in other window: "
                 action #'ivy--switch-buffer-other-window-action)))
    (ivy-read prompt 'internal-complete-buffer
              :action action
              :predicate filter
              :update-fn update
              :unwind unwind
              :preselect (buffer-name (other-buffer (current-buffer)))
              :matcher #'ivy--switch-buffer-matcher
              :keymap ivy-switch-buffer-map
              ;; NOTE A clever disguise, needed for virtual buffers.
              :caller #'ivy-switch-buffer)))

;;;###autoload
(defun +ivy/switch-workspace-buffer (&optional arg)
  "Switch to another buffer within the current workspace.
If ARG (universal argument), open selection in other-window."
  (interactive "P")
  (+ivy--switch-buffer t arg))

;;;###autoload
(defun +ivy/switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (+ivy--switch-buffer nil nil))


(provide 'igloo-buffer)


;;; igloo-buffer.el ends here
