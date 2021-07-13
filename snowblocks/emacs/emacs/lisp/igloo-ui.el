;;; igloo-ui.el --- UI config.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Lib -------------------------------------------------------------------------
;;;###autoload
(defun igloo-apply-ansi-color-to-compilation-buffer-h ()
  "Apply ansi codes to the compilation buffers.Meant for `compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))


;; Config ----------------------------------------------------------------------

;; Custom hooks
(defvar igloo-init-ui-hook nil
  "Hooks to run when the UI has been initialized.")

(defvar igloo-switch-buffer-hook nil
  "Hooks to run after changing buffer.")

(defvar igloo-switch-window-hook nil
  "Hooks to run after changing window.")

(defvar igloo-switch-frame-hook nil
  "Hooks to run after changing frame.")

(defvar igloo-inhibit-switch-buffer-hooks nil
  "Letvar for inhibiting `igloo-switch-buffer-hook'.Do not set this directly.")
(defvar igloo-inhibit-switch-window-hooks nil
  "Letvar for inhibiting `igloo-switch-window-hook'.Do not set this directly.")
(defvar igloo-inhibit-switch-frame-hooks nil
  "Letvar for inhibiting `igloo-switch-frame-hook'.Do not set this directly.")

(defvar igloo--last-window nil)
(defvar igloo--last-frame nil)

(defun igloo-run-switch-window-hooks-h ()
  (unless (or igloo-inhibit-switch-window-hooks
              (eq igloo--last-window (selected-window))
              (minibufferp))
    (let ((gc-cons-threshold most-positive-fixnum)
          (igloo-inhibit-switch-window-hooks t)
          (inhibit-redisplay t))
      (run-hooks 'igloo-switch-window-hook)
      (setq igloo--last-window (selected-window)))))

(defun igloo-run-switch-frame-hooks-h (&rest _)
  (unless (or igloo-inhibit-switch-frame-hooks
              (eq igloo--last-frame (selected-frame))
              (frame-parameter nil 'parent-frame))
    (let ((gc-cons-threshold most-positive-fixnum)
          (igloo-inhibit-switch-frame-hooks t))
      (run-hooks 'igloo-switch-frame-hook)
      (setq igloo--last-frame (selected-frame)))))

(defun igloo-run-switch-buffer-hooks-a (orig-fn buffer-or-name &rest args)
  (if (or igloo-inhibit-switch-buffer-hooks
          (and buffer-or-name
               (eq (current-buffer)
                   (get-buffer buffer-or-name)))
          (and (eq orig-fn #'switch-to-buffer) (car args)))
      (apply orig-fn buffer-or-name args)
    (let ((gc-cons-threshold most-positive-fixnum)
          (igloo-inhibit-switch-buffer-hooks t)
          (inhibit-redisplay t))
      (when-let (buffer (apply orig-fn buffer-or-name args))
        (with-current-buffer (if (windowp buffer)
                                 (window-buffer buffer)
                               buffer)
          (run-hooks 'igloo-switch-buffer-hook))
        buffer))))

(defun igloo-run-switch-to-next-prev-buffer-hooks-a (orig-fn &rest args)
  (if igloo-inhibit-switch-buffer-hooks
      (apply orig-fn args)
    (let ((gc-cons-threshold most-positive-fixnum)
          (igloo-inhibit-switch-buffer-hooks t)
          (inhibit-redisplay t))
      (when-let (buffer (apply orig-fn args))
        (with-current-buffer buffer
          (run-hooks 'igloo-switch-buffer-hook))
        buffer))))

(defun igloo-protect-fallback-buffer-h ()
  "Don't kill the scratch buffer. Meant for `kill-buffer-query-functions'."
  (not (eq (current-buffer) (igloo-fallback-buffer))))


;; UX --------------------------------------------------------------------------
; (setq confirm-kill-emacs t)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)


;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)


;;; Cursor

;; The blinking cursor is distracting
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;; Don't stretch the cursor to fit wide characters
(setq x-stretch-cursor nil)



;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;; Windows/frames

;; A simple frame title
(setq frame-title-format '("%b – Igloo")
      icon-title-format frame-title-format)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; But do not resize windows pixelwise, this can cause crashes in some cases
;; where we resize windows too quickly.
(setq window-resize-pixelwise nil)

;; Disable tool, menu, and scrollbars.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; These are disabled directly through their frame parameters to avoid the extra
;; work their minor modes do, but their variables must be unset too, otherwise
;; users will have to cycle them twice to re-enable them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'igloo-init-ui-hook #'window-divider-mode)

;; always avoid GUI
(setq use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
;; ...especially on linux
(when IS-LINUX
  (setq x-gtk-use-system-tooltips nil))

 ;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
      split-height-threshold nil)


;;; Minibuffer -----------------------------------------------------------------

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only)

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;; Packages ----------------------------------------------------------

;;;###package ansi-color
(setq ansi-color-for-comint-mode t)


(eval-after-load 'comint
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048)) ; double the default


; (eval-after-load 'compile
;   (progn (setq compilation-always-kill t       ; kill compilation process before starting another
;         compilation-ask-about-save nil  ; save all buffers on `compile'
;         compilation-scroll-output 'first-error)
;   ;; Handle ansi codes in compilation buffer
;   (add-hook 'compilation-filter-hook #'igloo-apply-ansi-color-to-compilation-buffer-h)

;   ;; Automatically truncate compilation buffers so they don't accumulate too
;   ;; much data and bog down the rest of Emacs.
;   (autoload 'comint-truncate-buffer "comint" nil t)
;   (add-hook 'compilation-filter-hook #'comint-truncate-buffer)))


(use-package all-the-icons)


(eval-after-load 'ediff
  (progn
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

  (defvar igloo--ediff-saved-wconf nil)
  ;; Restore window layout after quiting ediff
  (add-hook 'ediff-before-setup-hook
            (defun igloo-ediff-save-wconf-h ()
              (setq igloo--ediff-saved-wconf (current-window-configuration))))
  (add-hook 'ediff-quit-hook (defun igloo-ediff-restore-wconf-h ()
              (when (window-configuration-p igloo--ediff-saved-wconf)
                (set-window-configuration igloo--ediff-saved-wconf))))))


(provide 'igloo-ui)

;;; igloo-ui.el ends here
