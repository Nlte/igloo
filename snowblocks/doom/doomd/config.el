;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-font (font-spec :family "Source code pro" :size 12 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Source code pro" :size 12))
(setq +helm-posframe-text-scale 0)


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Set default directory to $HOME
(setq default-directory "~/")
;;(cd (getenv "HOME")) ;; for windows


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Global config

;; Org-mode
;;
(setq org-src-tab-acts-natively t)

;; Python
(setq python-shell-interpreter "python")


;; Custom modes
;;


;; Package configs
;;
;; EIN
(use-package! ein
  :init
  (setq ein:notebook-autosave-frequency 10)
  (setq ein:notebook-create-checkpoint-on-save t))
(map! (:when (featurep! :tools ein)
       (:map ein:notebook-mode-map
        :leader
        (:prefix-map ("j" . "jupyter")
        :desc "Execute cell"            "c" #'ein:worksheet-execute-cell-and-goto-next-km
        :desc "Execute all cells"       "R" #'ein:worksheet-execute-all-cells
        :desc "Insert cell below"       "b" #'ein:worksheet-insert-cell-below-km
        :desc "Insert cell above"       "a" #'ein:worksheet-insert-cell-above-km
        :desc "Next cell"               "j" #'ein:worksheet-goto-next-input-km
        :desc "Previous cell"           "k" #'ein:worksheet-goto-prev-input-km
        :desc "Yank cell"               "y" #'ein:worksheet-yank-cell-km
        :desc "Rename notbook"          "w" #'ein:notebook-rename-command-km
        ))))
(add-hook 'kill-emacs-hook 'ein:stop)

;; Neotree
(setq neo-theme 'ascii)

;; Ivy
(map! (:leader
      (:prefix "f"
       :desc "ivy-project-grep"         "g" #'+ivy:project-search
       )))


;; Buffer
;; (setq buffer-read-only nil)
;; (setq comint-prompt-read-only nil)

;; Persp mode
;;
;; disable auto save (causes emacs to freeze on exit)
(setq persp-auto-save-opt 0)
