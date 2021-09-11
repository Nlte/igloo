;;; ui.el --- UI configuration -*- lexical-binding: t -*-

;; Vertical Scroll
;; (setq scroll-step 1)
;; (setq scroll-margin 1)
;; (setq scroll-conservatively 101)
;; (setq scroll-up-aggressively 0.01)
;; (setq scroll-down-aggressively 0.01)
;; (setq auto-window-vscroll nil)
;; (setq fast-but-imprecise-scrolling nil)
;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; (setq mouse-wheel-progressive-speed nil)
;; ;; Horizontal Scroll
;; (setq hscroll-step 1)
;; (setq hscroll-margin 1)

;; ;; Notifications
;; (setq ring-bell-function 'ignore)



;; Theme
(use-package nano-theme
  :straight (:host github :repo "rougier/nano-theme"))
(load-theme 'nano t)

;; Set default font
(set-face-attribute 'default nil
                    ;:family "Source Code Pro"
                    :family "Roboto Mono"
                    :height 100
                    :weight 'normal
                    :width 'normal)
;; ;; Layout
;; ;; Vertical window divider
(setq window-divider-default-right-width 3)
(set-face-foreground 'vertical-border "#000000")
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; ;; No ugly button for checkboxes
(setq widget-image-enable nil)


(provide 'ui)
;;; ui.el ends here
