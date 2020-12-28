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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide 'nano-evil)
