;;; init.el --- Emacs init.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
; (require 'init-benchmarking) ;; Measure startup time


;; Package initialisation ------------------------------------------------------
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package) ;; init on non linux platform
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Customize support for 'emacs -q' (Optional)
;; TODO maybe delete
;; You can enable customizations by creating the nano-custom.el file
;; with e.g. `touch nano-custom.el` in the folder containing this file.
(let* ((this-file  (or load-file-name (buffer-file-name)))
       (this-dir  (file-name-directory this-file))
       (custom-path  (concat this-dir "igloo-custom.el")))
  (when (and (eq nil user-init-file)
             (eq nil custom-file)
             (file-exists-p custom-path))
    (setq user-init-file this-file)
    (setq custom-file custom-path)
    (load custom-file)))

;; -----------------------------------------------------------------------------
;; Load configs
;; -----------------------------------------------------------------------------

;; ui --------------------------------------------------------------------------
; (require 'nano-theme-light)
(require 'igloo-ui)
; (require 'nano-faces)
; (nano-faces)
; (require 'nano-theme)
; (nano-theme)
(require 'igloo-theme)
;; (require 'nano-modeline)
(require 'igloo-buffer)
(require 'igloo-workspace)

;; general ---------------------------------------------------------------------
(require 'igloo-config)
(require 'igloo-evil)
(require 'igloo-env)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(require 'igloo-cache)
(setq default-directory "~/") ;; TODO
; (igloo--setup-env)

;; completion and keybinds -----------------------------------------------------
(require 'igloo-general)
(require 'igloo-ivy)
(require 'igloo-hydra)

;; tools -----------------------------------------------------------------------
(require 'igloo-git)
(require 'igloo-projectile)
(require 'igloo-lsp)
(require 'igloo-company)
(require 'igloo-snippet)
(require 'igloo-flycheck)

;; Languages -------------------------------------------------------------------
(require 'igloo-elisp)
(require 'igloo-csv)
(require 'igloo-python)
(require 'igloo-yaml)
(require 'igloo-org)

;; Lib -------------------------------------------------------------------------
(require 'igloo-lib)

(message "init.el loaded")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "77113617a0642d74767295c4408e17da3bfd9aa80aaa2b4eeb34680f6172d71a" "6084dce7da6b7447dcb9f93a981284dc823bab54f801ebf8a8e362a5332d2753" "188fed85e53a774ae62e09ec95d58bb8f54932b3fd77223101d036e3564f9206" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" default))
 '(package-selected-packages
   '(all-the-icons neotree doom-themes org-jira shell-pop nose yasnippet-snippets python-pytest exec-path-from-shell pipenv yaml-mode flucui-themes direnv nord-theme python-mode flycheck yasnippet company-box company lsp-python-ms lsp-ui lsp-mode evil-smartparens evil-nerd-commenter csv-mode which-key major-mode-hydra use-package smex persp-mode ivy-rich hydra general counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; init.el ends here
