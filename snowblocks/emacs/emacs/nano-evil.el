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
  :init
  (eval-after-load "evil-surround"
    (evil-embrace-enable-evil-surround-integration)))

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


(provide 'nano-evil)
