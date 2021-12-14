;; ;; Orderless completion style


;; Consult completion --------------------------------------------------------------

;; ;; Vertical selection
;; (use-package selectrum
;;   :straight t
;;   :config
;;   (selectrum-mode))

;; ;; Search/completion style
;; (use-package orderless
;;   :straight t
;;   :init
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))

;; ;; Command history
;; (use-package selectrum-prescient
;;   :straight t
;;   :after selectrum
;;   :config
;;   (prescient-persist-mode)
;;   (selectrum-prescient-mode))

;; ;; Completion engine
;; (use-package consult
;;   :straight t)

;; (use-package consult-flycheck
;;   :straight t)

;; ;; Marginalias in the minibuffer (fileinfo etc.)
;; (use-package marginalia
;;   :straight t
;;   :init
;;   (marginalia-mode))


;; Ivy completion --------------------------------------------------------------
(use-package ivy
  :straight t
  :init
  (setq ivy-height 10)
  (setq ivy-fixed-height-minibuffer 10)
  (setq ivy-initial-inputs-alist nil)

  :config
  (ivy-mode))

(use-package ivy-prescient
  :straight t
  :config
  (ivy-prescient-mode))

(use-package counsel
  :straight t
  :config
  (counsel-mode))

(use-package swiper
  :straight t
  :config
  (defun ig/swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'word))
    ))


(provide 'ig-completion)
