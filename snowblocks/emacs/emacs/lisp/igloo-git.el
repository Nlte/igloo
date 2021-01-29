;;; igloo-git.el --- Git config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar +magit-open-windows-in-direction 'right
  "What direction to open new windows from the status buffer (left, right, up, down).")


;;;###autoload
(defun +magit-display-buffer-fn (buffer)
  "Same as `magit-display-buffer-traditional', except...
   - If opened from a commit window, it will open below it.
   - Magit process windows are always opened in small windows below the current.
   - Everything else will reuse the same window."
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond
             ((and (eq buffer-mode 'magit-status-mode)
                   (get-buffer-window buffer))
              '(display-buffer-reuse-window))
             ;; Any magit buffers opened from a commit window should open below
             ;; it. Also open magit process windows below.
             ((or (bound-and-true-p git-commit-mode)
                  (eq buffer-mode 'magit-process-mode))
              (let ((size (if (eq buffer-mode 'magit-process-mode)
                              0.35
                            0.7)))
                `(display-buffer-below-selected
                  . ((window-height . ,(truncate (* (window-height) size)))))))

             ;; Everything else should reuse the current window.
             ((or (not (derived-mode-p 'magit-mode))
                  (not (memq (with-current-buffer buffer major-mode)
                             '(magit-process-mode
                               magit-revision-mode
                               magit-diff-mode
                               magit-stash-mode
                               magit-status-mode))))
              '(display-buffer-same-window))

             ('(+magit--display-buffer-in-direction))))))

(defun +magit--display-buffer-in-direction (buffer alist)
  "`display-buffer-alist' handler that opens BUFFER in a direction.
This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
  (let ((direction (or (alist-get 'direction alist)
                       +magit-open-windows-in-direction))
        (origin-window (selected-window)))
    (if-let (window (window-in-direction direction))
        (unless magit-display-buffer-noselect
          (select-window window))
      (if-let (window (and (not (one-window-p))
                           (window-in-direction
                            (pcase direction
                              (`right 'left)
                              (`left 'right)
                              ((or `up `above) 'down)
                              ((or `down `below) 'up)))))
          (unless magit-display-buffer-noselect
            (select-window window))
        (let ((window (split-window nil nil direction)))
          (when (and (not magit-display-buffer-noselect)
                     (memq direction '(right down below)))
            (select-window window))
          (display-buffer-record-window 'reuse window buffer)
          (set-window-buffer window buffer)
          (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
          (set-window-prev-buffers window nil))))
    (unless magit-display-buffer-noselect
      (switch-to-buffer buffer t t)
      (selected-window))))



(use-package magit
  :ensure t
  :config
  (setq
   magit-save-repository-buffers nil
   magit-revision-insert-related-refs nil
   magit-diff-refine-hunk t)

  ;; Display
  (setq magit-display-buffer-function '+magit-display-buffer-fn))


(provide 'igloo-git)

;;; igloo-git.el ends here
