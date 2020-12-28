;; Workspace module
;; Source: "Doom workspace"
;; https://github.com/hlissner/doom-emacs/blob/master/modules/feature/workspaces/autoload/workspaces.el


;; Lib ------------------------------------------------------------------------

(defvar +workspace-main "main"
  "The name of the primary and initial workspace, which cannot be deleted.")

(defface +workspace-tab-selected-face '((t (:inherit 'highlight)))
  "The face for selected tabs displayed by `+workspace/display'")

(defface +workspace-tab-face '((t (:inherit 'default)))
  "The face for selected tabs displayed by `+workspace/display'")

(defun +workspace--generate-id ()
  (or (cl-loop for name in (+workspace-list-names)
               when (string-match-p "^#[0-9]+$" name)
               maximize (string-to-number (substring name 1)) into max
               finally return (if max (1+ max)))
      1))

;;;###autoload
(defun +workspace/switch-to-0 () (interactive) (+workspace/switch-to 0))
;;;###autoload
(defun +workspace/switch-to-1 () (interactive) (+workspace/switch-to 1))
;;;###autoload
(defun +workspace/switch-to-2 () (interactive) (+workspace/switch-to 2))
;;;###autoload
(defun +workspace/switch-to-3 () (interactive) (+workspace/switch-to 3))
;;;###autoload
(defun +workspace/switch-to-4 () (interactive) (+workspace/switch-to 4))
;;;###autoload
(defun +workspace/switch-to-5 () (interactive) (+workspace/switch-to 5))
;;;###autoload
(defun +workspace/switch-to-6 () (interactive) (+workspace/switch-to 6))
;;;###autoload
(defun +workspace/switch-to-7 () (interactive) (+workspace/switch-to 7))
;;;###autoload
(defun +workspace/switch-to-8 () (interactive) (+workspace/switch-to 8))


(defun +workspace--protected-p (name)
  (equal name persp-nil-name))

;;;###autoload
(defalias #'+workspace-contains-buffer-p #'persp-contain-buffer-p
  "Return non-nil if BUFFER is in WORKSPACE (defaults to current workspace).")

(defun +workspace-init-first-workspace-h (&rest _)
  "Ensure a main workspace exists."
  (when persp-mode
    (let (persp-before-switch-functions)
      ;; The default perspective persp-mode creates is special and doesn't
      ;; represent a real persp object, so buffers can't really be assigned
      ;; to it, among other quirks, so we get rid of it...
      (when (equal (car persp-names-cache) persp-nil-name)
        (pop persp-names-cache))
      ;; ...and create a *real* main workspace to fill this role.
      (if (and (not (gethash +workspace-main *persp-hash*))
                    (< (hash-table-count *persp-hash*) 2))
          (persp-add-new +workspace-main))
      ;; HACK Fix #319: the warnings buffer gets swallowed when creating
      ;;      `+workspace-main', so display it ourselves, if it exists.
      ; (when-let (warnings (get-buffer "*Warnings*"))
      ;   (save-excursion
      ;     (display-buffer-in-side-window
      ;      warnings '((window-height . shrink-window-if-larger-than-buffer)))))
      )))

(defun +workspace--message-body (message &optional  type)
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
  (funcall (if noerror #'message #'error) "%s" (+workspace--message-body message 'error)))


;;;###autoload
(defun +workspace-list-names ()
  "Return a list of workspace names (strings)."
  (delete persp-nil-name (persp-names-current-frame-fast-ordered)))

;;;###autoload
(defun +workspace-current-name ()
  "Get the name of the current workspace."
  (safe-persp-name (get-current-persp)))

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (cl-assert (stringp name) t)
  (member name (+workspace-list-names)))

;;;###autoload
(defalias #'+workspace-p #'persp-p "Return t if OBJ is a perspective hash table.")

;;;###autoload
(defun +workspace-get (name &optional noerror)
  "Returns a workspace (perspective hash table) named NAME."
  (when-let* ((persp (persp-get-by-name name)))
    (cond ((+workspace-p persp) persp)
          ((not noerror) (error "'%s' is an invalid workspace" name)))))

;;;###autoload
(defalias '+workspace-current #'get-current-persp)

;;;###autoload
(defun +workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise."
  (when (+workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (+workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (and (persp-add-new name) t))

(defun +workspace--tabline (&optional names)
  "Returns a string with all the workpaces."
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

;;;###autoload
(defun +workspace-switch (name &optional auto-create-p)
  "Switch to another workspace."
  (unless (+workspace-exists-p name)
    (if auto-create-p
        (+workspace-new name)
      (error "%s is not an available workspace" name)))
  (let ((old-name (+workspace-current-name)))
    (setq +workspace--last
          (or (and (not (string= old-name persp-nil-name))
                   old-name)
              +workspace-main)))
  (persp-frame-switch name))

;;;###autoload
(defun +workspace-delete (name &optional inhibit-kill-p)
  "Delete the workspace denoted by NAME, which can be the name of a perspective
or its hash table. If INHIBIT-KILL-P is non-nil, don't kill this workspace's
buffers."
  (when (+workspace--protected-p name)
    (error "Can't delete '%s' workspace" name))
  (+workspace-get name) ; error checking
  (persp-kill name inhibit-kill-p)
  (not (+workspace-exists-p name)))


;;; Interactive ----------------------------------------------------------------

;;;###autoload
(defun +workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (message "%s" (+workspace--tabline)))

;;;###autoload
(defun +workspace/switch-to (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (completing-read "Switch to workspace: " (+workspace-list-names)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case ex
      (let ((names (+workspace-list-names))
            (old-name (+workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (+workspace-switch dest)))
              ((stringp index)
               (unless (member index names)
                 (error "No workspace named %s" index))
               (+workspace-switch index))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (+workspace-current-name) old-name)
              (+workspace-message (format "Already in %s" old-name) 'warn)
            (+workspace/display))))
    ('error (+workspace-error (cadr ex) t))))


;;;###autoload
(defun +workspace/cycle (n)
  "Cycle n workspaces to the right (default) or left."
  (interactive (list 1))
  (let ((current-name (+workspace-current-name)))
    (if (equal current-name persp-nil-name)
        (+workspace-switch +workspace-main t)
      (condition-case ex
          (let* ((persps (+workspace-list-names))
                 (perspc (length persps))
                 (index (cl-position current-name persps)))
            (when (= perspc 1)
              (user-error "No other workspaces"))
            (+workspace/switch-to (% (+ index n) perspc))
            (unless (called-interactively-p 'interactive)
              (+workspace/display)))
        ('user-error (+workspace-error (cadr ex) t))
        ('error (+workspace-error ex t))))))

;;;###autoload
(defun +workspace/switch-left ()  (interactive) (+workspace/cycle -1))

;;;###autoload
(defun +workspace/switch-right () (interactive) (+workspace/cycle +1))

;;;###autoload
(defun +workspace/new (&optional name clone-p)
  "Create a new workspace named NAME. If OVERWRITE-P is non-nil, clear any
pre-existing workspace."
  (interactive "iP")
  (unless name
    (setq name (format "#%s" (+workspace--generate-id))))
  (condition-case ex
      (let ((exists-p (+workspace-exists-p name)))
        (if exists-p
            (error "%s already exists" name)
          (+workspace-switch name t)
          (if clone-p
              (dolist (window (window-list))
                (persp-add-buffer (window-buffer window) persp nil))
            (delete-other-windows-internal)
            (switch-to-buffer (get-buffer-create "*splash*")))
          (+workspace/display)))
    ('error (+workspace-error (cadr ex) t))))

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
                           nil nil current-name)
        current-name))))
  (condition-case ex
      (+workspace-message
       (let ((workspaces (length (+workspace-list-names))))
         (cond ((> workspaces 1)
                (+workspace-delete name)
                (+workspace-switch
                 (if (+workspace-exists-p +workspace--last)
                     +workspace--last
                   (car (+workspace-list-names))))
                (format "Deleted '%s' workspace" name))
               ((= workspaces 1)
                (format "Can't delete the last workspace"))
               (t
                (+workspace-delete name)
                (+workspace-switch +workspaces-main t)
                (switch-to-buffer (get-buffer-create "*splash*"))
                (format "No workspaces detected. Auto-creating '%s' workspace" +workspaces-main))))
       'success)
    ('error (+workspace-error (cadr ex) t))))


;;; Persp-mode config ----------------------------------------------------------

(use-package persp-mode
  :commands persp-mode
  :init
  (add-hook 'persp-mode-hook #'+workspace-init-first-workspace-h)
  (persp-mode 1)
  :config
  (setq persp-nil-hidden t ; hide the nil perspective
        persp-auto-resume-time -1 ; Don't auto-load on startup
        persp-autokill-buffer-on-remove t
        persp-auto-save-persps-to-their-file-before-kill nil ; do not autosave
        persp-auto-save-persps-to-their-file nil
        persp-mode-auto-save-opt 0))


(provide 'nano-workspace)
