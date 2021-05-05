;;; igloo-ui-nano.el --- UI config.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package bespoke-themes
  :straight (:host github :repo "mclear-tools/bespoke-themes")
  :init
  ;; Add to load path
  (add-to-list 'custom-theme-load-path (concat custom-theme-directory "bespoke-themes/"))
    (if (not (display-graphic-p))
      ;; No header line in terminal
      (setq set-bespoke-header-line nil)
      ;; Set header line in GUI
      (setq set-bespoke-header-line t))
  :config
  ;; Load dark theme
  (load-theme 'bespoke-dark t)
  ;; Use mode line visual bell
  (bespoke-themes-visual-bell-config))

(provide 'igloo-ui-nano)

;;; igloo-ui-nano.el ends here
