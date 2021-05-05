;;; igloo-evil.el --- Evil config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'general)

(use-package evil
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
    "r" 'evil-window-rotate-downwards))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter)
  :general
  ([remap comment-line] #'evilnc-comment-or-uncomment-lines)
  (:states '(normal visual)
           "gc" 'evilnc-comment-operator))

(provide 'igloo-evil)

;;; igloo-evil.el ends here
