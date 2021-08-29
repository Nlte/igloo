;;; init-ui.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ui.el
;; Description: Configure the ui

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; Notifications
(setq ring-bell-function 'ignore)

;; Layout
;; Vertical window divider
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Set default font
(set-face-attribute 'default nil
                    ;:family "Source Code Pro"
                    :family "Roboto Mono"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Theme
(use-package nano-theme
  :ensure nil
  :defer t
  :quelpa (nano-theme
           :fetcher github
           :repo "rougier/nano-theme"))

(load-theme 'nano t)
(set-face-attribute 'ivy-current-match nil :foreground "white")
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil :foreground "black" :background "lightgrey")
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil :foreground "black" :background "orange")
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil :foreground "black" :background "lightblue")
  ;;
  (set-face-attribute 'swiper-match-face-2         nil :foreground "black" :background "lightgrey")
  (set-face-attribute 'swiper-match-face-3         nil :foreground "black" :background "orange")
  (set-face-attribute 'swiper-match-face-4         nil :foreground "black" :background "lightblue")

(provide 'init-ui)
;;; init-ui.el ends here
