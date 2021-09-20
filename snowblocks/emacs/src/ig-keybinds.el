;;; ig-keybinds.el --- Keybindings configuration -*- lexical-binding: t -*-

(defvar igloo-leader-key "SPC"
  "The leader prefix key.")

(defvar igloo-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states.")

(defvar igloo-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")


(cond
 (IS-MAC
  ;; mac-* variables are used by the special emacs-mac build of Emacs by
  ;; Yamamoto Mitsuharu, while other builds use ns-*.
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier nil 
        ns-right-option-modifier  nil))
 (IS-WINDOWS
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))


;;
;;; Universal, non-nuclear escape from doom emacs

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar igloo-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).
More specifically, when `igloo/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun igloo/escape (&optional interactive)
  "Run `igloo-escape-hook'."
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'igloo-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

(global-set-key [remap keyboard-quit] #'igloo/escape)
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)


(use-package major-mode-hydra
  :straight t)


(defun ig-find-file-in-private-directory ()
  (interactive)
  (consult-find "~/.emacs.d/"))

(pretty-hydra-define ig-hydra-find
 (:foreign-keys warn :color teal :idle 1.0 :quit-key ("q" "<escape>"))
 ("Find"
  (("f" find-file "find file")
   ("p" ig-find-file-in-private-directory "open config"))))

(pretty-hydra-define ig-hydra-search
 (:foreign-keys warn :color teal :idle 1.0 :quit-key ("q" "<escape>"))
 ("Search"
  (("b" consult-line "search buffer")
   ("i" consult-imenu "consult imenu")
   ("g" consult-ripgrep "consult ripgrep"))))


(pretty-hydra-define ig-hydra-window
 (:foreign-keys warn :color teal :idle 1.0 :quit-key ("<escape>"))
 ("Window"
  (("l" evil-window-right "select right window")
   ("h" evil-window-left "select left window")
   ("k" evil-window-up "switch up window")
   ("j" evil-window-down "switch down window")
   ("s" evil-window-split "horizontal split")
   ("v" evil-window-vsplit "vertical split"))

  "Resize"
   (("q" evil-quit "quit window")
   ("=" balance-windows "resize windows"))))

(pretty-hydra-define ig-hydra-git
 (:foreign-keys warn :color teal :idle 1.0 :quit-key ("q" "<escape>"))
 ("Git"
  (("g" magit-status "magit status")
   ("b" magit-branch-checkout "magit switch branch")
   ("B" magit-blame "magit blame")
   ("F" magit-fetch "magit fetch")
   ("L" magit-log "magit log"))))


(pretty-hydra-define ig-hydra-buffer
  (:foreign-keys warn :color teal :idle 1.0 :quit-key ("q" "<escape>"))
  ("Buffer"
   (("b" projectile-switch-to-buffer "switch workspace buffer")
    ("B" switch-to-buffer "switch buffer")
    ("d" kill-current-buffer "kill buffer")
    ("k" kill-current-buffer "kill buffer"))))

(pretty-hydra-define ig-hydra-project
  (:foreign-keys warn :color teal :idle 1.0 :quit-key ("q" "<escape>"))
  ("Project"
   (("p" projectile-switch-project))))


(use-package general
  :straight t)

(general-define-key
  :states '(emacs normal hybrid motion visual operator)
  :keymaps 'override
  :prefix "SPC"
  "!" 'shell-command
  "SPC" 'projectile-find-file
  "X" 'org-capture
  ":" 'execute-extended-command
  "," 'projectile-switch-to-buffer
  "m" 'major-mode-hydra
  "f" 'ig-hydra-find/body
  "w" 'ig-hydra-window/body
  "g" 'ig-hydra-git/body
  "p" 'ig-hydra-project/body
  "b" 'ig-hydra-buffer/body
  "s" 'ig-hydra-search/body)



(provide 'ig-keybinds)

;;; ig-keybinds.el ends here 
