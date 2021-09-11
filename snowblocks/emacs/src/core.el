;; core.el --- Constant and overall optimisations -*- lexical-binding: t; -*-

(set-language-environment "UTF-8")

;; Platform
(defconst IS-WINDOWS
  (eq system-type '(windows-nt ms-dos cygwin)))

(defconst IS-LINUX 
  (eq system-type 'gnu/linux))

(defconst IS-MAC 
  (eq system-type 'darwin))
;; ~Platform

(defun make-dir-if-not-exists (dir)
  (when (not (file-exists-p dir))
    (make-directory dir t)))


;; Directories and config files 
(defconst igloo-local-dir (concat (file-name-as-directory (getenv "HOME")) ".local/share/igloo.emacs.d/")
  "Local non versioned directory for packages, local system config and other files.
   Must end with slash.")
(make-dir-if-not-exists igloo-local-dir)

(defconst igloo-etc-dir (concat igloo-local-dir "etc/")
  "Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")
(make-dir-if-not-exists igloo-etc-dir)

(defconst igloo-cache-dir (concat igloo-local-dir "cache/")
  "Directory for volatile local storage.
Use this for files that change often, like cache files. Must end with a slash.")
(make-dir-if-not-exists igloo-cache-dir)

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

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather `text-mode', which pulls in many packages
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Littering
(setq async-byte-compile-log-file  (concat igloo-etc-dir "async-bytecomp.log")
      desktop-dirname              (concat igloo-etc-dir "desktop")
      desktop-base-file-name       "autosave"
      desktop-base-lock-name       "autosave-lock"
      pcache-directory             (concat igloo-cache-dir "pcache/")
      request-storage-directory    (concat igloo-cache-dir "request")
      shared-game-score-directory  (concat igloo-etc-dir "shared-game-score/"))


;; Doom Optimisations

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text scanning for a modest performance boost.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; The GC introduces annoying pauses and stuttering.
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle.
(setq gcmh-idle-delay 0.5  ; default is 15s
      gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb 
      gcmh-verbose nil)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)  ; default is 0.5

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))


;; Package management

;; MelpaPackages
;; Select the folder to store packages
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
;; -MelpaPackages


;; ConfigurePackageManager
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))


;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(provide 'core)
;;; core.el ends here
