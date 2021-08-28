;;; init-ivy.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ivy.el
;; Description: Configure Ivy Completion

(use-package counsel)

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
  ;; is way too big (30,000). Turn it down so big repos affect project
  ;; navigation less.
  (setq ivy-sort-max-size 7500)

  (setq ivy-height 17
        ivy-count-format ""
        ivy-initial-inputs-alist nil
        ivy-fixed-height-minibuffer t
        ivy-read-action-function #'ivy-hydra-read-action
        ivy-read-action-format-function #'ivy-read-action-format-columns
        projectile-completion-system 'ivy
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(provide 'init-ivy)
;;; init-ivy.el ends here 
