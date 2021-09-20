;;; ig-lang-elisp.el --- -*- lexical-binding: t -*-

(use-package emacs-lisp-mode
  :mode-hydra
  (emacs-lisp-mode
   (:title "Elisp mode" :idle 0.5 :quit-key ("q" "<escape>"))
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
    (("d" describe-foo-at-point "thing-at-pt")
     ("f" describe-function "function")
     ("v" describe-variable "variable")
     ("i" info-lookup-symbol "info lookup")))))


(provide 'ig-lang-elisp)
;;; ig-lang-elisp.el ends here
