;; init.el --- Emacs init.el -*- lexical-binding: t -*-
;; Igloo emacs

;; GarbageCollection
(setq gc-cons-threshold 134217728) ; 128mb
;; Process read
(setq read-process-output-max (* 1024 1024)) ;; 10mb
                        
;; LoadPath
(defun ig/update-load-path (folder)
  "Add FOLDER and its subdirectories to `load-path'.

Hidden directories (names starting with \".\") are skipped.
This is a small helper to avoid needing `normal-top-level-add-subdirs-to-load-path`.

FOLDER is interpreted relative to `user-emacs-directory` when not already
an absolute path."
  (let ((base (expand-file-name folder user-emacs-directory)))
    (when (file-directory-p base)
      (unless (member base load-path)
        (add-to-list 'load-path base))
      (dolist (entry (directory-files base t "^[^.]"))
        (when (file-directory-p entry)
          (add-to-list 'load-path entry))))))

(ig/update-load-path "src")
;; ~LoadPath

;;; Core ------------------------------------------------------------

;; --- Module loader ------------------------------------------------------
;; Define two lists of modules: `ig/core-modules` are required for everything
;; else, while `ig/optional-modules` can be toggled by commenting/uncommenting.
(defvar ig/core-modules
  '(ig-core
    ig-core-packages)
  "Modules that must be loaded before anything else.")

(defvar ig/optional-modules
  '(ig-lib
    ;; ig-web
    ig-editor
    ig-evil
    ig-ui

    ;; Core packages
    ig-keybinds
    ig-completion
    ig-git
    ig-project
    ig-workspaces
    ig-snippets

    ;; Optional / experimental
    ;; ig-terminal
    ;; ig-ledger
    ;; ig-docker
    ;; ig-email
    ;; ig-org
    ;; ig-dashboard
    ;; ig-lsp

    ;; Language helpers
    ;; ig-lang-elisp
    ;; ig-lang-bison
    ;; ig-lang-python
    ;; ig-lang-cpp
    ;; ig-lang-markdown
    )
  "IG modules loaded at startup.

Comment out entries to quickly disable a module during debugging.")

(defun ig/load-modules (modules)
  "Require each module in MODULES.

Errors are handled gracefully so a broken module won’t stop Emacs from
starting."  ; `elisp-lint` likes docstrings to be on one line when short.
  (dolist (mod modules)
    (when (and mod (symbolp mod))
      (condition-case ex
          (require mod)
        (error
         (message "[ig] failed to load %s: %S" mod ex))))))

(ig/load-modules ig/core-modules)

;; Initialize theme and modeline before deferring optional modules.
;; This ensures consistent UI appearance from startup.
(condition-case ex
    (progn
      (require 'ig-theme)
      (load-theme 'ig t)
      (require 'ig-modeline)
      (ig-modeline))
  (error
   (message "[ig] failed to load theme/modeline: %S" ex)))

;; Load optional modules after startup to avoid slowing down initial launch.
;; This keeps Emacs responsive while still loading the extra features.
(when (not noninteractive)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-idle-timer 0.5 nil
                                   (lambda ()
                                     (ig/load-modules ig/optional-modules))))))

;; Put auto-generated `custom-set-variables` and `custom-set-faces` in a
;; separate file so this init file stays focused on hand-written configuration.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
