;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers 
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------


;; General keybindings

;; Utils

(use-package general)

(general-create-definer global-definer
  :keymaps 'override
  :states '(insert emacs normal hybrid motion visual operator)
  :prefix "SPC"
  :non-normal-prefix "C-SPC")

(defmacro general-global-menu-definer (def infix-key &rest body)
  "Create a definer named general-global-DEF wrapping global-definer.
The prefix map is named 'my-DEF-map'."
  `(progn
     (general-create-definer ,(intern (concat "general-global-" def))
       :wrapping global-definer
       :prefix-map (quote ,(intern (concat "my-" def "-map")))
       :infix ,infix-key
       :wk-full-keys nil
       "" '(:ignore t :which-key ,def))
     (,(intern (concat "general-global-" def))
      ,@body)))



(global-unset-key (kbd "C-SPC"))

;; Global
(global-definer
  "!"   'shell-command
  ":"   'counsel-M-x
  ","   '+ivy/switch-workspace-buffer
  "SPC" 'projectile-find-file)

;; c --- code
(general-global-menu-definer
  "Code" "c")

;; f --- file
(general-global-menu-definer
  "find" "f"
  "f" 'counsel-find-file
  "g" 'counsel-rg)

;; r --- remote
(general-global-menu-definer
  "remote" "r")

;; s --- search
(general-global-menu-definer
  "search" "s")

;; n --- Notes
(general-global-menu-definer
  "note" "n")

;; o --- open
(general-global-menu-definer
  "open" "o")

;; p --- project
(general-global-menu-definer
  "project" "p"
  "p" 'counsel-projectile-switch-project)

;; w --- window
(general-global-menu-definer
  "window" "w")

;; b --- buffer
(general-global-menu-definer
 "buffer" "b"
 "b" '+ivy/switch-workspace-buffer
 "d" 'kill-current-buffer)

;; TAB --- workspace
(general-global-menu-definer
  "workspace" "TAB"
  "TAB" '+workspace/display
  "n" '+workspace/new
  "d" '+workspace/delete
  "]" '+workspace/switch-right
  "[" '+workspace/switch-left
  "1" '+workspace/switch-to-0
  "2" '+workspace/switch-to-1
  "3" '+workspace/switch-to-2
  "4" '+workspace/switch-to-3
  "5" '+workspace/switch-to-4
  "6" '+workspace/switch-to-5
  "7" '+workspace/switch-to-6
  "8" '+workspace/switch-to-7
  "9" '+workspace/switch-to-8)

;; Close frame if not the last, kill emacs else
(defun nano--delete-frame-or-kill-emacs ()
  "Delete frame or kill Emacs if there is only one frame."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") 'nano--delete-frame-or-kill-emacs)

;; Platform keybindings
(cond
 (IS-MAC
  (setq mac-command-modifier      'super
        ns-command-modifier       'super
        mac-option-modifier       'meta
        ns-option-modifier        'meta
        ;; Free up the right option for character composition
        mac-right-option-modifier 'none
        ns-right-option-modifier  'none))
 (IS-WINDOWS
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

;; which-key
(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1
        which-key-side-window-max-height 0.25))



(provide 'nano-bindings)

