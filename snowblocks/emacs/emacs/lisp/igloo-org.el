;;; igloo-org.el --- Org config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (pretty-hydra-define org-hydra-date
;;   (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
;;   ("Hydra org date"
;;    (("d" nil "nil"))
;;    ))
   ;; (("d" org-deadline "org-deadline"
   ;;   "s" org-schedule "org-schedule"
   ;;   "t" org-time-stamp "org-time-stamp"
   ;;   "T" org-time-stamp-inactive "org-timestamp-inactive"))))

(pretty-hydra-define org-hydra-date
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Hydra org date"
   (("d" org-deadline "org-deadline")
    ("s" org-schedule "org-schedule")
    ("t" org-time-stamp "org-time-stamp")
    ("T" org-time-stamp-inactive "org-time-stamp-inactive"))))



(pretty-hydra-define org-hydra-link
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  ("Hydra org link"
   (("l" org-insert-link "org-insert-link"))))

(major-mode-hydra-define org-mode
  (:foreign-keys warn :color teal :idle 0.5 :quit-key ("q" "<escape>"))
  (""
   (("l" org-hydra-link/body "org-hydra-link")
    ("d" org-hydra-date/body "org-hydra-date"))))


(provide 'igloo-org)

;;; igloo-org.el ends here
