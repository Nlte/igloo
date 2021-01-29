;;; igloo-python.el --- Python config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'igloo-hydra)

(use-package lsp-python-ms
  :defer 0.3
  :custom
  (lsp-python-ms-auto-install-server t))

(use-package python-mode
  :init
  (setq-default
   python-shell-interpreter "ipython"))

(major-mode-hydra-define python-mode
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Eval"
   (("b" nil "run buffer"))))
	    

(provide 'igloo-python)
;;; igloo-python.el ends here
