;; init.el --- Emacs init.el -*- lexical-binding: t -*-
;; Igloo emacs

;; GarbageCollection
(setq gc-cons-threshold 134217728) ; 128mb
;; Process read
(setq read-process-output-max (* 1024 1024)) ;; 10mb
                        
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

;; Custom functions
(require 'ig-lib)

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

;; Ledger
(require 'ig-ledger)

;; Docker
(require 'ig-docker)

;; Languages
(require 'ig-lsp)

(require 'ig-lang-org)
(require 'ig-lang-elisp)
(require 'ig-lang-bison)
(require 'ig-lang-python)
(require 'ig-lang-cpp)
(require 'ig-lang-markdown)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "db5b906ccc66db25ccd23fc531a213a1afb500d717125d526d8ff67df768f2fc" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "dff67645a672aa16487a7bb64520bc39895ad5315c11d112820a8a7b0d9ee3b1" default))
 '(electric-indent-mode nil)
 '(interaction-log-mode nil)
 '(package-selected-packages
   '(persp-projectile nano-theme counsel-projectile projectile nano-sidebar evil-commentary-mode evil-commentary evil-commentary: magit frame perspective org-bullets evil-org quelpa ctrlf selectrum-prescient prescient selectrum evil-collection use-package))
 '(warning-suppress-types '((comp) (comp)))
 '(widget-image-enable nil)
 '(x-underline-at-descent-line t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
