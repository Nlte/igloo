;;; igloo-python.el --- Python config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Lib -------------------------------------------------------------------------
(defun python-pytest--get-buffer ()
  "Get a create a suitable compilation buffer."
  (if (eq major-mode 'python-pytest-mode)
      (current-buffer)  ;; re-use buffer
    (let ((name python-pytest-buffer-name))
      (when python-pytest-project-name-in-buffer-name
        (setq name (format "%s<%s>" name (python-pytest--project-name))))
      (get-buffer-create name))))

(defun igloo-pytest-run-in-popup ()
  "Run pytest function cmd in pop window."
  (interactive)
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      ;; (#'cmd)
      (python-pytest-file (buffer-file-name))
      ;; (let ((prefix-arg cmd))
      ;;   (call-interactively #'cmd))
      (current-buffer))
    '((side . bottom)))))

(use-package lsp-python-ms
  :defer 0.3
  :custom
  (lsp-python-ms-auto-install-server t))

(use-package python-mode
  :init
  (setq-default
   python-shell-interpreter "ipython")
  (add-hook 'python-mode'hook 'nose-mode))

(use-package python-pytest)

(pretty-hydra-define python-hydra-test
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Hydra python test"
   (("f" python-pytest-file     "python-pytest-file")
    ("p" python-pytest-dispatch "python-pytest-dispatch")
    ("t" python-pytest-function "python-pytest-function"))
   ""
   (("v" python-pytest-file "python-pytest-file"))))

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
