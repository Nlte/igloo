;;; ig-workspaces.el --- -*- lexical-binding: t -*-

;;;###autoload
(defface +workspace-tab-selected-face '((t (:inherit highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

;;;###autoload
(defface +workspace-tab-face '((t (:inherit default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be deleted.")

(defvar +workspaces-fallback-buffer "*scratch*"
  "Buffer to go to on empty perspectives.")


(defun +workspace--protected-p (name)
  (equal name persp-nil-name))

(defun +workspace--generate-id ()
  (or (cl-loop for name in (+workspace-list-names)
               when (string-match-p "^#[0-9]+$" name)
               maximize (string-to-number (substring name 1)) into max
               finally return (if max (1+ max)))
      1))

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (member name (+workspace-list-names)))

;;;###autoload
(defun +workspace-switch (name &optional auto-create-p)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE-P is non-nil, create the workspace if it doesn't exist, otherwise
throws an error."
  (unless (+workspace-exists-p name)
    (if auto-create-p
        (+workspace-new name)
      (error "%s is not an available workspace" name)))
  (let ((old-name (+workspace-current-name)))
    (unless (equal old-name name)
      (setq +workspace--last
            (or (and (not (string= old-name persp-nil-name))
                     old-name)
                +workspaces-main))
      (persp-frame-switch name))
    (equal (+workspace-current-name) name)))

;; Getters
;;;###autoload
(defalias #'+workspace-current #'get-current-persp
  "Return the currently active workspace.")

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Return a workspace named NAME. Unless NOERROR is non-nil, this throws an
error if NAME doesn't exist."
  (cl-check-type name string)
  (when-let (persp (persp-get-by-name name))
    (unless noerror
      (error "No workspace called '%s' was found" name))
    persp))

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the current workspace."
  (safe-persp-name (+workspace-current)))

;;;###autoload
(defun +workspace-list-names ()
  "Return the list of names of open workspaces."
  persp-names-cache)

;;; Tabs display in minibuffer
(defun +workspace--tabline (&optional names)
  (let ((names (or names (+workspace-list-names)))
        (current-name (+workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (1+ i) name)
                          'face (if (equal current-name name)
                                    '+workspace-tab-selected-face
                                  '+workspace-tab-face)))
     " ")))

(defun +workspace--message-body (message &optional type)
  (concat (+workspace--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))

;;;###autoload
(defun +workspace-message (message &optional type)
  "Show an 'elegant' message in the echo area next to a listing of workspaces."
  (message "%s" (+workspace--message-body message type)))

;;;###autoload
(defun +workspace-error (message &optional noerror)
  "Show an 'elegant' error in the echo area next to a listing of workspaces."
  (funcall (if noerror #'message #'error)
           "%s" (+workspace--message-body message 'error)))

;;;###autoload
(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (+workspace--tabline))))

;;;###autoload
(defun +workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (let ((persp (persp-add-new name))
        (+popup--inhibit-transient t))
    (save-window-excursion
      (let ((ignore-window-parameters t)
            (+popup--inhibit-transient t))
        (persp-delete-other-windows))
      (switch-to-buffer (+workspaces-fallback-buffer))
      (setf (persp-window-conf persp)
            (funcall persp-window-state-get-function (selected-frame))))
    persp))
;;;###autoload
(defun +workspace-delete (name &optional _dont-confirm-p)
  "Delete a workspace named NAME."
  (let* ((current-name (+workspace-current-name))
         (persp (get-current-persp))
         (frame (selected-frame)))
    (when (equal name current-name)
      ;; Switch away first
      (let ((names (delq name (+workspace-list-names))))
        (if names
            (+workspace-switch (car names))
          (+workspace-switch +workspaces-main t))))
    (persp-kill (persp-get-by-name name))))

;;;###autoload
(defun +workspace/delete (name)
  "Delete this workspace. If called with C-u, prompts you for the name of the
workspace to delete."
  (interactive
   (let ((current-name (+workspace-current-name)))
     (list
      (if current-prefix-arg
          (completing-read (format "Delete workspace (default: %s): " current-name)
                           (+workspace-list-names)
                           nil nil nil nil current-name)
        current-name))))
  (condition-case-unless-debug ex
      (let ((workspaces (+workspace-list-names)))
        (if (not (member name workspaces))
            (+workspace-message (format "'%s' workspace doesn't exist" name) 'warn)
          ;; Workspace exists, proceed with deletion
          (if (equal (+workspace-current-name) name)
              ;; Currently in this workspace, switch away first
              (let ((other-workspaces (delq name workspaces)))
                (if other-workspaces
                    (progn
                      (+workspace-delete name)
                      (+workspace-switch (car other-workspaces))
                      (+workspace-message (format "Deleted '%s' workspace" name) 'success))
                  ;; No other workspaces, switch to main
                  (progn
                    (+workspace-delete name)
                    (+workspace-switch +workspaces-main t)
                    (+workspace-message (format "Deleted '%s' workspace" name) 'success))))
            ;; Not in this workspace, just delete it
            (progn
              (+workspace-delete name)
              (+workspace-message (format "Deleted '%s' workspace" name) 'success)))))
    (error (+workspace-error ex t))))

(defun +workspaces-init-first-workspace-h (&rest _)
  "Ensure main workspace exists."
  (when persp-mode
     (when (equal (car persp-names-cache) persp-nil-name)
       (message "popping names cache")
       (pop persp-names-cache))
       (message "adding main persp")
       (persp-add-new +workspaces-main))
        ;; Display the Warnings buffer after main init if exists
        (when-let (warnings (get-buffer "*Warnings*"))
          (save-excursion
            (display-buffer-in-side-window
             warnings '((window-height . shrink-window-if-larger-than-buffer))))))

;;;###autoload
(defun +workspace/new (&optional name clone-p)
  "Create a new workspace named NAME. If CLONE-P is non-nil, clone the current
workspace, otherwise the new workspace is blank."
  (interactive (list nil current-prefix-arg))
  (unless name
    (setq name (format "#%s" (+workspace--generate-id))))
  (condition-case e
      (cond ((+workspace-exists-p name)
             (error "%s already exists" name))
            (clone-p (persp-copy name t))
            (t
             (+workspace-switch name t)
             (+workspace/display)))
    (error (+workspace-error (cadr e) t))))

;;;###autoload
(defun +workspace-rename (name)
  "Rename the current workspace to NAME."
  (interactive
   (let ((current-name (+workspace-current-name)))
     (list
      (read-string (format "Rename workspace (current: %s) to: " current-name)
                   nil nil current-name))))
  (let ((current-name (+workspace-current-name)))
    (if (equal current-name name)
        (+workspace-message "No change to workspace name" 'warn)
      (condition-case ex
          (if (or (equal name persp-nil-name)
                  (+workspace-exists-p name))
              (+workspace-error (format "'%s' is not a valid workspace name" name))
            (persp-rename name (get-current-persp))
            (+workspace-message (format "Renamed '%s' to '%s'" current-name name) 'success))
        (error (+workspace-error (cadr ex) t))))))


(use-package persp-mode
  :straight t
  :init
  (persp-mode 1)
  (+workspaces-init-first-workspace-h)
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-reset-windows-on-nil-window-conf nil
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-save-dir (concat igloo-etc-dir "workspaces/")
        persp-set-last-persp-for-new-frames t
        persp-switch-to-added-buffer nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1 ; Don't auto-load on startup
        persp-auto-save-opt (if noninteractive 0 1)) ; auto-save on kill
  )

(provide 'ig-workspaces)
;;; ig-workspaces.el ends here
