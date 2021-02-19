;;; igloo-python.el --- Python config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-python-ms
  :defer 0.3
  :custom
  (lsp-python-ms-auto-install-server t))

(use-package python-mode
  :init
  (setq-default
   python-shell-interpreter "ipython"))

(use-package python-pytest)

(pretty-hydra-define python-hydra-test
  (:forein-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Hydra python test"
   (("v" python-pytest-file     "python-pytest-file")
    ("a" python-pytest-dispatch "python-pytest-dispatch")
    ("f" python-pytest-function "python-pytest-function"))))

(pretty-hydra-define python-hydra-insert
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Hydra python insert"
   (("s" yas-insert-snippet "yas-insert-snippet"))))

(major-mode-hydra-define python-mode
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Eval"
   (("b" nil "run buffer"))
   "REPL"
   (("c" nil "open repl"))
   "Test"
   (("t" python-hydra-test/body "test-hydra"))
   "Insert"
   (("i" python-hydra-insert/body "insert-hydra"))
   ))
	    

(provide 'igloo-python)
;;; igloo-python.el ends here
