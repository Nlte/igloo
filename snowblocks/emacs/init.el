;; init.el --- Emacs init.el -*- lexical-binding: t -*-
;; Igloo emacs

;; GarbageCollection
;; (defvar igloo-gc-cons-threshold 134217728 ; 128mb
;;   "The default value to use for `gc-cons-threshold'.
;;   gc-cons-threshold determines how many bytes can be allocated without triggering a garbage collection.
;;   In case of freezes, decrease the value. In case of stuttering, increase the value.")

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold igloo-gc-cons-threshold)
;;             (setq file-name-handler-alist file-name-handler-alist-original)
;;             (makunbound 'file-name-handler-alist-original)))
;; ~GarbageCollection

;; LoadPath
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "src" user-emacs-directory))
;; ~LoadPath

;;; Core ------------------------------------------------------------

;; Core
(require 'ig-core)
(require 'ig-core-packages)
; Core of emacs is now loaded, we can use-package to configure additional
; functionalities

;; Editor
(require 'ig-editor)

;; Evil navigation
(require 'ig-evil)

;; UI
(require 'ig-ui)

;;; Packages -------------------------------------------------------
;; (require 'init-theme)

;; Perspectives
; (require 'init-perspective)
;; Terminals
(require 'ig-terminal)

;; Keybinds
(require 'ig-keybinds)

;; Completions
(require 'ig-completion)

;; Git
(require 'ig-git)

;; Project management
(require 'ig-project)

;; Workspaces
(require 'ig-workspaces)

;; Snippets
(require 'ig-snippets)

;; Languages
(require 'ig-lang-org)
(require 'ig-lang-elisp)
(require 'ig-lang-bison)
(require 'ig-lang-python)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("dff67645a672aa16487a7bb64520bc39895ad5315c11d112820a8a7b0d9ee3b1" default))
 '(package-selected-packages
   '(persp-projectile nano-theme counsel-projectile projectile nano-sidebar evil-commentary-mode evil-commentary evil-commentary: magit frame perspective org-bullets evil-org quelpa ctrlf selectrum-prescient prescient selectrum evil-collection use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
