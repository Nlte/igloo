(require 'igloo-hydra)


(use-package general
    :ensure t)

(general-define-key
  :states '(emacs normal hybrid motion visual operator)
  :keymaps 'override
  :prefix "SPC"
  "!" 'shell-command
  ":" 'counsel-M-x
  "m" 'major-mode-hydra
  "f" 'ig-hydra-find/body
  "SPC" 'counsel-projectile-find-file
  "TAB" 'ig-hydra-workspace/body
  "w" 'ig-hydra-window/body
  "g" 'ig-hydra-git/body
  "b" 'ig-hydra-buffer/body
  "p" 'ig-hydra-project/body
  "s" 'ig-hydra-search/body
  "i" 'ig-hydra-insert/body
  "," '+ivy/switch-workspace-buffer)

; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide 'igloo-general)
