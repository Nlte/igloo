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
;; (require 'ig-workspaces)

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
   '("0f38739f8c04db9e179c6fa7012b309978bc389d5d574f7cf4d91af4c1fc6ec0" "72b8c89c90365e8e3e6f775bdf6860a302be30d36577466e96989a280f6d3335" "42e4b2c5ec81089ddf6ce6a59a57edc71098e7608f9e1958986b1a051a34c7a9" "591e1afff7a17e1c7745e7e9de5603a7a693a71a56e0711b274d7c901a04b2af" "75e2c5fae392599a46e54f1e4098729bf232cd6f81bcff5f8a93cdb8f583fcfb" "bdb9e9b81cdae0fdf1c3133844f4ac6c77af55266508c5dfabc162807e2ab596" "f73d7c277b0179720fe58d3ec1de3965f16cf87489a93500635d3bc88ad83e52" "bdd43b6ca0bc9cec328ed95c5a66a80491603d9cdb4d828550163670084f7313" "8a5d4c8644038d381e5f77aab3039d620a1b1526607c56b12d7811e6acbe83c3" "63b4151df18a3ae034bb17160a93ca8a1f47986e79443f1144ee7e072b2c2480" "c24990b92371ddccfc1b1bd23734aa185fc52a618039626b260115b8774a55fe" "7d19cb330c5c8357cdca5526af9eb0aac3f1c7e5e0955cfa40ddcdb8b6bfa099" "7d8749cc8e72ae89bd47bb6bd641fb14d41009f7f89afaabc2601ff87a5c749b" "8c0dd814fbca9a08e6c927b82e4289410af565533a7369f5fd74e159fc5eadb2" "ede9e1a55419c3d81ec1c96bac0e5780697814c8d0a21338a592d1aeaf6c8733" "43688e98d30194829cb66f9e503e201d76d334f003f91f5fef127a0338d47a82" "eb33355b30a9c928b0edab850c05476d7362a45e5d62cdc92ec4736280b133ac" "12716871d6b6a7f744ac553f6e5f82a01e06bf48a8b184b7c9a301ce822b705a" "9985f306be01a90bbab0cf1ceceb748dd40eda3e5682c36025f18f4a9054ed6a" "21157e179ef7937c92bf36b837cc9af0eb4e76d80629b04c06b22d68ef130468" "e912e6df2504f0f1eadbfbf469190cf617be65881c0f04c748b29da5488a0825" "edd5dbbd75368532fdb9677a0100b1e2b14636fcc39cea0c2fef8892be7a3473" "b8dd2642a64059d0fb7d411f652f7f1c000a37343e2828ec2f700109c7900101" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "db5b906ccc66db25ccd23fc531a213a1afb500d717125d526d8ff67df768f2fc" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "dff67645a672aa16487a7bb64520bc39895ad5315c11d112820a8a7b0d9ee3b1" default))
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
