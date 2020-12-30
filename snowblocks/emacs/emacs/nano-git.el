;; Lib -------------------------------------------------------------------------

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



;; Config ----------------------------------------------------------------------

(use-package magit
  :ensure t
  :commands magit-file-delete
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)
  ;; Address minor mode to mark all URLs in magit mode as clickable
  (add-hook 'magit-process-mode-hook #'goto-address-mode)
  ;; The default location for git-credential-cache is in
  ;; ~/.cache/git/credential. However, if ~/.git-credential-cache/ exists, then
  ;; it is used instead. Magit seems to be hardcoded to use the latter, so here
  ;; we override it to have more correct behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (setq magit-credential-cache-daemon-socket
          (nano-glob (or (getenv "XDG_CACHE_HOME")
                         "~/.cache/")
                     "git/credential/socket")))
  ;; Prevent scrolling when manipulating magit-status hunks. Otherwise you must
  ;; reorient yourself every time you stage/unstage/discard/etc a hunk.
  ;; Especially so on larger projects."
  (defvar +magit--pos nil)
  (add-hook 'magit-pre-refresh-hook
    (defun +magit--set-window-state-h ()
      (setq-local +magit--pos (list (current-buffer) (point) (window-start)))))
  (add-hook 'magit-post-refresh-hook
    (defun +magit--restore-window-state-h ()
      (when (and +magit--pos (eq (current-buffer) (car +magit--pos)))
        (goto-char (cadr +magit--pos))
        (set-window-start nil (caddr +magit--pos) t)
        (kill-local-variable '+magit--pos))))
  ;; Magit uses `magit-display-buffer-traditional' to display windows, by
  ;; default, which is a little primitive. `+magit-display-buffer' marries
  ;; `magit-display-buffer-fullcolumn-most-v1' with
  ;; `magit-display-buffer-same-window-except-diff-v1', except:
  ;;
  ;; 1. Magit sub-buffers (like `magit-log') that aren't spawned from a status
  ;;    screen are opened as popups.
  ;; 2. The status screen isn't buried when viewing diffs or logs from the
  ;;    status screen.
  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'+magit-display-buffer-fn
        magit-bury-buffer-function #'magit-mode-quit-window)
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)


  ;; Clean up after magit by killing leftover magit buffers and reverting
  ;; affected buffers (or at least marking them as need-to-be-reverted).
  (define-key magit-status-mode-map [remap magit-mode-bury-buffer] #'+magit/quit)
  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one)
  ;; An optimization that particularly affects macOS and Windows users: by
  ;; resolving `magit-git-executable' Emacs does less work to find the
  ;; executable in your PATH, which is great because it is called so frequently.
  ;; However, absolute paths will break magit in TRAMP/remote projects if the
  ;; git executable isn't in the exact same location.
  (add-hook 'magit-status-mode-hook
    (defun +magit-optimize-process-calls-h ()
      (when-let (path (executable-find magit-git-executable t))
        (setq-local magit-git-executable path)))))


(use-package github-review
  :after magit
  :config
  (transient-append-suffix 'magit-merge "i"
    '("y" "Review pull request" +magit/start-github-review)))


(use-package evil-collection
  :after '(magit evil)
  :defer t
  :init (defvar evil-collection-magit-use-z-for-folds t)
  :config
  ;; These numbered keys mask the numerical prefix keys. Since they've already
  ;; been replaced with z1, z2, z3, etc (and 0 with g=), there's no need to keep
  ;; them around:
  (general-unbind magit-mode-map "M-1" "M-2" "M-3" "M-4" "1" "2" "3" "4" "0")
  ;; q is enough; ESC is way too easy for a vimmer to accidentally press,
  ;; especially when traversing modes in magit buffers.
  ;;(evil-define-key* 'normal magit-status-mode-map [escape] nil)
  ;; Some extra vim conf
  (evil-define-key* '(normal visual) magit-mode-map
    "%"  #'magit-gitflow-popup
    "zt" #'evil-scroll-line-to-top
    "zz" #'evil-scroll-line-to-center
    "zb" #'evil-scroll-line-to-bottom
    "g=" #'magit-diff-default-context
    "gi" #'forge-jump-to-issues
    "gm" #'forge-jump-to-pullreqs)
 
  (general-define-key
   :states 'normal
   (
    ;;magit-status-mode-map
    magit-stash-mode-map
    magit-revision-mode-map
    magit-process-mode-map
    magit-diff-mode-map)
   [tab] #'magit-section-toogle))


(use-package git-auto-commit-mode
  :defer t)



(provide 'nano-git)
