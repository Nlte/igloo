;;; igloo-elisp.el --- Emacs elisp settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'igloo-hydra)

(major-mode-hydra-define emacs-lisp-mode
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" nil "describe thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

(provide 'igloo-elisp)
;;; igloo-elisp.el ends here
