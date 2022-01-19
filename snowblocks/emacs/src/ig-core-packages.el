;;; ig-core-packages.el --- Core packages -*- lexical-binding: t -*-

(straight-use-package 'use-package)

(use-package gcmh
  :straight t
  :config
  (gcmh-mode 1))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize)
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      (progn
	    (message "Native comp is available")
        ;; Using Emacs.app/Contents/MacOS/bin since it was compiled with
        ;; ./configure --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS"
        ;; Append to path to give priority to values from exec-path-from-shell-initialize.
        (add-to-list 'exec-path (concat invocation-directory (file-name-as-directory "bin")) t)
	    (setenv "LIBRARY_PATH" (concat (getenv "LIBRARY_PATH")
                                       (when (getenv "LIBRARY_PATH")
                                         ":")
				                       ;; This is where Homebrew puts libgccjit libraries.
                                       (car (file-expand-wildcards
                                             (expand-file-name "/usr/local/opt/libgccjit/lib/gcc/*")))))
	    ;; Only set after LIBRARY_PATH can find gcc libraries.
	    (setq comp-deferred-compilation t)
        (setq comp-speed 3))
    (message "Native comp is *not* available")))

(use-package interaction-log
  :straight t)

(use-package which-key
  :straight t)

(provide 'ig-core-packages)
;;; ig-core-packages.el ends here
