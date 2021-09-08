(use-package bespoke-themes
  :quelpa (bespoke-themes :fetcher github :repo "mclear-tools/bespoke-themes")
  :config
  ;; Set header line
  (setq bespoke-set-mode-line 'footer)
  ;; Set mode line height
  (setq bespoke-set-mode-line-size 2)
  ;; Show diff lines in modeline
  (setq bespoke-set-git-diff-mode-line t)
  ;; Set mode-line cleaner
  (setq bespoke-set-mode-line-cleaner t)
  ;; Set evil cursor colors
  (setq bespoke-set-evil-cursors t)
  ;; Use mode line visual bell
  (setq bespoke-set-visual-bell nil)
  ;; Set use of italics
  (setq bespoke-set-italic-comments nil
        bespoke-set-italic-keywords nil)
  ;; Set variable pitch
  (setq bespoke-set-variable-pitch t)
  ;; Set initial theme variant
  (setq bespoke-set-theme 'light)
  ;; Load theme
  (load-theme 'bespoke t))

;; Notifications
(setq ring-bell-function 'ignore)

;; Layout
;; Vertical window divider
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

(use-package nano-sidebar
  ;; :ensure nil
  ;; :defer t
  :quelpa (nano-sidebar
           :fetcher github
           :repo "Nlte/nano-sidebar"))

;; Set default font
;;(set-face-attribute 'default nil
 ;                   ;:family "Source Code Pro"
 ;                   :family "Roboto Mono"
 ;                   :height 110
 ;                   :weight 'normal
 ;                   :width 'normal)

(provide 'init-theme)
