;;; ig-ui.el --- UI configuration -*- lexical-binding: t -*-

;; (straight-use-package '(nano-theme :type git :host github
;;                                    :repo "rougier/nano-theme"))
;; (load-theme 'nano t)

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(setq ring-bell-function 'ignore)


(provide 'ig-ui)
;;; ig-ui.el ends here
