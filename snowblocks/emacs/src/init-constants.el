;;; init-const.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-constants.el
;; Description: Initialize Constants

(set-language-environment "UTF-8")


;; Platform
(defconst IS-WINDOWS
  (eq system-type '(windows-nt ms-dos cygwin)))

(defconst IS-LINUX 
  (eq system-type 'gnu/linux))

(defconst IS-MAC 
  (eq system-type 'darwin))
;; ~Platform


;; Directories and config files 
(defconst igloo-local-dir (concat (file-name-as-directory (getenv "HOME")) ".local/share/igloo.emacs.d/")
  "Local non versioned directory for packages, local system config and other files.
   Must end with slash.")

(defconst igloo-etc-dir (concat igloo-local-dir "etc/")
  "Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst igloo-cache-dir (concat igloo-local-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files. Must end with a slash.")

(defconst igloo-local-config-file (concat igloo-local-dir "local.el")
  "Local .el file to set system specific variables such as keyboard layout.")


;; Load local config file
(if (file-exists-p igloo-local-config-file)
    (load-file igloo-local-config-file))


;; Keyboard layout
(defconst igloo-keyboard-layout
  (if (boundp 'igloo-localconfig-keyboard-layout) (symbol-value 'igloo-localconfig-keyboard-layout) "qwerty")
  "The keyboard layout: azerty / qwerty")
(unless (member igloo-keyboard-layout '("azerty" "qwerty"))
  (error "unkown keyboard layout: %s" igloo-keyboard-layout))

(defconst igloo-keyboard-layout-is-azerty
  (string= igloo-keyboard-layout "azerty"))

(defconst igloo-keyboard-layout-is-qwerty
  (string= igloo-keyboard-layout "qwerty"))


;; Executables
(defconst python-p
  (or (executable-find "python3")
      (and (executable-find "python")
           (> (length (shell-command-to-string "python --version | grep 'Python 3'")) 0)))
  "Python3 executable found.")

(defconst pip-p
  (or (executable-find "pip3")
      (and (executable-find "pip")
           (> (length (shell-command-to-string "pip --version | grep 'python 3'")) 0)))
  "Pip3 executable found.")

(defconst clangd-p
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd"))  ;; macOS
  "Clangd executable found.")
;; ~Executables 


(provide 'init-constants)
;; init-const.el ends here
