;;; init-selectrum.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-selectrum.el
;; Description: Configure Selectrum Completion

(use-package selectrum
  :init
  (selectrum-mode 1))

(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode 1)
  :after selectrum)

(use-package ctrlf
  :init
  (ctrlf-mode 1))


(provide 'init-selectrum)
;;; init-selectrum.el ends here 
