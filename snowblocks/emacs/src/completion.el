;; Vertico completion engine
(use-package vertico
  :straight t
  :init
  (setq
   vertico-count 10
   vertico-resize nil)
  (vertico-mode))

;; Orderless completion style
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight t
  :init
  (savehist-mode))


(use-package consult
  :straight t)

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(provide 'completion)
