(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (evil-mode 1)
  :general
  (general-global-window
    ;; movement
    "j" 'evil-window-down
    "k" 'evil-window-up
    "l" 'evil-window-right
    "h" 'evil-window-left
    ;; split
    "s" 'evil-window-split
    "v" 'evil-window-vsplit
    ;; actions
    "q" 'evil-quit
    "r" 'evil-window-rotate-downwards)
  (evil-mode-map
   :states '(normal visual)
   "gc" 'evilnc-comment-operator))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))

(use-package evil-embrace
  :ensure t
  :commands embrace-add-pair embrace-add-pair-regexp
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook)
  :hook (org-mode . embrace-org-mode-hook)
  :init
  (eval-after-load "evil-surround"
    (evil-embrace-enable-evil-surround-integration))
  :config
  (setq evil-embrace-show-help-p nil))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general ([remap comment-line] #'evilnc-comment-or-uncomment-lines))


(use-package evil-escape
  :commands evil-escape
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer, unless `evil-collection-setup-minibuffer'
  ;; is enabled, where we could be in insert mode in the minibuffer.
  (add-hook 'evil-escape-inhibit-functions
    (defun +evil-inhibit-escape-in-minibuffer-fn ()
      (and (minibufferp)
           (or (not (bound-and-true-p evil-collection-setup-minibuffer))
               (evil-normal-state-p)))))
  ;; so that evil-escape-mode-hook runs, and can be toggled by evil-mc
  (evil-escape-mode +1))


(use-package smartparens
  ;; Auto-close delimiters and blocks as you type.
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :config
  (smartparens-global-mode 1)
  ;; smartparens recognizes `slime-mrepl-mode', but not `sly-mrepl-mode', so...
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil)
    ;; Smartparens conditional binds a key to C-g when sp overlays are active
    ;; (even if they're invisible). This disruptively changes the behavior of
    ;; C-g in insert mode, requiring two presses of the key to exit insert mode.
    ;; I don't see the point of this keybind, so...
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)
  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook 'eval-expression-minibuffer-setup-hook
    (defun nano-init-smartparens-in-eval-expression-h ()
      "Enable `smartparens-mode' in the minibuffer for `eval-expression'.
This includes everything that calls `read--expression', e.g.
`edebug-eval-expression' Only enable it if
`smartparens-global-mode' is on."
      (when smartparens-global-mode (smartparens-mode +1))))
  (add-hook 'minibuffer-setup-hook
    (defun nano-init-smartparens-in-minibuffer-maybe-h ()
      "Enable `smartparens' for non-`eval-expression' commands.
Only enable `smartparens-mode' if `smartparens-global-mode' is
on."
      (when (and smartparens-global-mode (memq this-command '(evil-ex)))
        (smartparens-mode +1))))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (defvar nano-buffer-smartparens-mode nil)
  (add-hook 'evil-replace-state-exit-hook
    (defun nano-enable-smartparens-mode-maybe-h ()
      (when nano-buffer-smartparens-mode
        (turn-on-smartparens-mode)
        (kill-local-variable 'nano-buffer-smartparens-mode))))
  (add-hook 'evil-replace-state-entry-hook
    (defun nano-disable-smartparens-mode-maybe-h ()
      (when smartparens-mode
        (setq-local nano-buffer-smartparens-mode t)
        (turn-off-smartparens-mode)))))


(provide 'nano-evil)
