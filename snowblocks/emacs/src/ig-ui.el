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
  :straight t)

(use-package neotree
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
          help-mode
          compilation-mode))
  (setq popper-group-function #'popper-group-by-project) ;; project.el
  ;; (setq popper-group-function #'popper-group-by-projectile) ; projectile projects
  (popper-mode +1)
  (popper-echo-mode +1))


;; (setq display-buffer-alist
;; '(
;;     ("*Org Select*"
;;     (display-buffer-same-window))
;; ))

;; (use-package shackle
;;     :straight t
;;     :diminish shackle-mode  ; hide name in mode-line
;;     :config
;;     (setq shackle-rules
;;           '(("*Org Agenda*" :align below :select t)
;;             (" *Org todo*" :align below :select t)))
;;     (shackle-mode t))

(provide 'ig-ui)
;;; ig-ui.el ends here
