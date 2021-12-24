;;; ig-ui.el --- UI configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'ig-theme)
(load-theme 'ig t)

;; (require 'ig-nord-theme)
;; (load-theme 'ig-nord t)

(require 'ig-modeline)
(ig-modeline)

(use-package all-the-icons
  :if (display-graphic-p)
  :straight t)

(use-package nerd-fonts
  :straight (:host github :repo "twlz0ne/nerd-fonts.el"))

(use-package neotree
  :straight t)

(use-package svg-lib
  :straight t)

;; (use-package doom-themes
;;   :straight t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-nord-light t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))


(setq ring-bell-function 'ignore)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)


;; Popup rules -----------------------------------------------------------------
(use-package popper
  :straight t
  :bind (("C--"   . popper-toggle-latest)
         ("M--"   . popper-cycle)
         ("C-M--" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
          "\\*pytest\\*.*"
          help-mode
          compilation-mode))
  ;; (setq popper-group-function #'popper-group-by-projectile) ; projectile projects
  (popper-mode +1)
  (popper-echo-mode +1))


;; Fixes
(defun his-tracing-function (orig-fun &rest args)
  (let ((original-modeline-format mode-line-format))
    (message "org-tag called with args %S" args)
    (message "removing modeline")
    (setq mode-line-format nil)
    (force-mode-line-update)
    (message "redrawing")
    (redraw-display)
    (let ((res (apply orig-fun args)))
      (message "restoring modeline")
      (setq mode-line-format original-modeline-format)
        (message "org-tag returned %S" res)
        res))
    )


;; Disable modeline for org fast tag selection (we can't see everything otherwise)
(advice-add 'org-fast-tag-selection :around #'his-tracing-function)

(defun igloo-hide-mode-line ()
  (setq mode-line-format nil))


;; Buffer display alist
(setq display-buffer-alist nil)
;; (add-to-list 'display-buffer-alist
;;                '("\\*Org tags\\*"
;;                  (display-buffer-in-direction)
;;                  (direction . bottom)
;;                  (window-width . fit-window-to-buffer)
;;                  (window-height . 300))
;;                )
;; (add-to-list 'display-buffer-alist
;;              '("\\*mu4e-headers\\*"
;;                (display-buffer-in-side-window)
;;                ))
(add-to-list 'display-buffer-alist
               '("*mu4e-headers*"
                 (display-buffer-in-side-window)
                 (side                . right)
                 ))

(provide 'ig-ui)
;;; ig-ui.el ends here
