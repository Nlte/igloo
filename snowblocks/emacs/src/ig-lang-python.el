;;; ig-lang-python.el --- -*- lexical-binding: t -*-


;; Lib -------------------------------------------------------------------------



;; Config ----------------------------------------------------------------------
(use-package python-pytest
  :straight t)

(use-package python-mode
  :mode-hydra
  (python-mode
   (:title "Elisp mode" :color teal :idle 0.5 :quit-key ("q" "<escape>"))
   ("Eval"
    (("b" nil "buffer"))
    "REPL"
    (("i" nil "ipython"))
    "Test"
    (("t" python-pytest-function "python-pytest-function")
     ("f" python-pytest-file "python-pytest-file")
     ("F" python-pytest-file-dwim "python-pytest-file"))
    "Doc"
    (("f" nil "function"))))
  :init
  (setq python-indent-offset 4
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))


(use-package python-black
  :straight t
  :after python
  ;; :hook (python-mode . python-black-on-save-mode-enable-dwim))
  )

(use-package jupyter
  :straight t)


(provide 'ig-lang-python)
;;; ig-lang-python.el ends here
