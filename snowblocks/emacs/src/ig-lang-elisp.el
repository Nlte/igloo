;;; ig-lang-elisp.el --- -*- lexical-binding: t -*-

;;;###autoload
(defun igloo-elisp/open-repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (pop-to-buffer
    (or (get-buffer "*ielm*")
      (progn (ielm)
        (let ((buf (get-buffer "*ielm*")))
          (bury-buffer buf)
          buf)))))

(use-package emacs-lisp-mode
  :config
  (setq lisp-indent-offset 2)
  :mode-hydra
  (emacs-lisp-mode
    (:title "Elisp mode" :color teal :idle 0.5 :quit-key ("q" "<escape>") :separator " ")
    ("Eval"
      (("b" eval-buffer "buffer")
        ("e" eval-defun "defun")
        ("r" eval-region "region"))
      "REPL"
      (("I" igloo-elisp/open-repl "ielm"))
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
