;;; igloo-hydra.el --- Hydra configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package major-mode-hydra)


;; Find
(pretty-hydra-define ig-hydra-find
 (:foreign-keys warn :color teal :idle 1.0 :quit-key ("q" "<escape>"))
 ("Hydra finder"
  (("f" counsel-find-file "find file")
   ("g" counsel-rg "grep")
   ("p" ig/open-private-config "open private config"))))

;; Workspace
(pretty-hydra-define ig-hydra-workspace
 (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
 ("Hydra workspace"
  (("n" +workspace/new "new workspace")
   ("d" +workspace/delete "delete workspace")
   ("[" +workspace/switch-left "switch left")
   ("]" +workspace/switch-right "switch right")
   ("TAB" +workspace/display "show workspaces"))

   ""
   (("1" +workspace/switch-to-0 "switch to 0")
   ("2" +workspace/switch-to-1 "switch to 1")
   ("3" +workspace/switch-to-2 "switch to 2")
   ("4" +workspace/switch-to-3 "switch to 3")
   ("5" +workspace/switch-to-4 "switch to 4")
   ("6" +workspace/switch-to-5 "switch to 5")
   ("7" +workspace/switch-to-6 "switch to 6")
   ("8" +workspace/switch-to-7 "switch to 7")
   ("9" +workspace/switch-to-8 "switch to 8"))))

;; Window
(pretty-hydra-define ig-hydra-window
 (:foreign-keys warn :color teal :idle 1.0 :quit-key ("<escape>"))
 ("Hydra window"
  (("l" evil-window-right "select right window")
   ("h" evil-window-left "select left window")
   ("k" evil-window-up "switch up window")
   ("j" evil-window-down "switch down window")
   ("s" evil-window-split "horizontal split")
   ("v" evil-window-vsplit "vertical split"))

  "Resize"
   (("q" evil-quit "quit window")
   ("=" evil-resize "resize window"))))


;; Git
(pretty-hydra-define ig-hydra-git
 (:foreign-keys warn :color teal :idle 1.0 :quit-key ("q" "<escape>"))
 ("Hydra git"
  (("g" magit-status "magit status")
   ("b" magit-branch-checkout "magit switch branch")
   ("B" magit-blame "magit blame")
   ("F" magit-fetch "magit fetch")
   ("L" magit-log "magit log"))))

;; Buffer
(pretty-hydra-define ig-hydra-buffer
  (:foreign-keys warn :color teal :idle 1.0 :quit-key ("q" "<escape>"))
  ("Hydra buffer"
   (("b" +ivy/switch-workspace-buffer "switch workspace buffer")
    ("B" +ivy/switch-buffer "switch buffer")
    ("d" kill-current-buffer "kill buffer")
    ("k" kill-current-buffer "kill buffer"))))

;; Project
(pretty-hydra-define ig-hydra-project
    (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
    ("Hydra project"
     (("p" counsel-projectile-switch-project "switch project")
      ("i" projectile-invalidate-cache "invalidate project cache")
      ("m" ivy-make-projectile "makefile"))))

;; Search
(pretty-hydra-define ig-hydra-search
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Hydra search"
   (("b" swiper "swiper"))))

;; Insert
(pretty-hydra-define ig-hydra-insert
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Hydra insert"
   (("s" yas-insert-snippet "Snippet"))))

;; Open
(pretty-hydra-define ig-hydra-open
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Hydra open"
   (("p" neotree-project-dir "neotree-toggle"))))

(provide 'igloo-hydra)

;;; igloo-hydra.el ends here
