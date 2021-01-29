;; Lib -------------------------------------------------------------------------

(defun +ivy--is-workspace-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (+workspace-contains-buffer-p buffer)))

(defun +ivy--is-workspace-other-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (and (not (eq buffer (current-buffer)))
         (+workspace-contains-buffer-p buffer))))

(defun +ivy--switch-buffer (workspace other)
  (message "%s" other)
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

;; Interactive -----------------------------------------------------------------

;;;###autoload
(defun +ivy/switch-workspace-buffer (&optional arg)
  "Switch to another buffer within the current workspace.
   If ARG (universal argument), open selection in other-window."
  (interactive "P")
  (+ivy--switch-buffer t arg))

;; Config ----------------------------------------------------------------------

(use-package smex)

(use-package counsel)

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
  ;; is way too big (30,000). Turn it down so big repos affect project
  ;; navigation less.
  (setq ivy-sort-max-size 7500)

  (setq ivy-height 17
        ivy-count-format ""
        ivy-initial-inputs-alist nil
        ivy-fixed-height-minibuffer t
        ivy-read-action-function #'ivy-hydra-read-action
        ivy-read-action-format-function #'ivy-read-action-format-columns
        projectile-completion-system 'ivy
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


(provide 'igloo-ivy)
