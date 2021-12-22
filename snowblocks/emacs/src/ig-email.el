;;; ig-email.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'use-package)


(defconst email-client-command "mu"
  "Command line tool email client.")

(defconst email-sync-command "offlineimap"
  "Command line tool to manage email sync.")

(if (not (executable-find email-client-command))
    (warn (format "ig-email.el: %s command not found" email-client-command)))

(if (not (executable-find email-sync-command))
    (warn (format "ig-email.el: %s command not found" email-sync-command)))


;; Mu4e ------------------------------------------------------------------------

;; .el sources added as a git submodule because use-package will build mu
;; the build script is broken for MacOS (compiler flag issues)
;; Install mu with package manager / source build and git clone recursive for the .el
(use-package mu4e
  :load-path "~/igloo/snowblocks/emacs/mu/mu4e"
  :config
    ;; This is set to 't' to avoid mail syncing issues when using mbsync
    (setq mu4e-change-filenames-when-moving t)

    ;; Refresh mail using offlineimap every 10 minutes
    (setq mu4e-update-interval (* 10 60))
    (setq mu4e-get-mail-command email-sync-command) 
    (setq mu4e-maildir "~/Maildir")
    ;; Make sure plain text mails flow correctly for recipients
    (setq mu4e-compose-format-flowed t)

    ;; Configure the function to use for sending mail
    (setq message-send-mail-function 'smtpmail-send-it)

    (setq mu4e-contexts
            (list
            ;; Work account
            (make-mu4e-context
            :name "Personal"
            :match-func
                (lambda (msg)
                (when msg
                    (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "nathanael.t.leaute@gmail.com")
                    (user-full-name    . "Nathanaël Leaute")
                    (smtpmail-smtp-server  . "smtp.gmail.com")
                    (smtpmail-smtp-service . 465)
                    (smtpmail-stream-type  . ssl)
                    (mu4e-compose-signature . "Thanks,\nNathanael")
                    (mu4e-drafts-folder  . "/Gmail/[Gmail]/Drafts")
                    (mu4e-sent-folder  . "/Gmail/[Gmail]/Sent Mail")
                    (mu4e-refile-folder  . "/Gmail/[Gmail]/All Mail")
                    (mu4e-trash-folder  . "/Gmail/[Gmail]/Trash")))))

    (setq mu4e-maildir-shortcuts
        '(("/Inbox"             . ?i)
          ("/[Gmail]/Sent Mail" . ?s)
          ("/[Gmail]/Trash"     . ?t)
          ("/[Gmail]/Drafts"    . ?d)
          ("/[Gmail]/All Mail"  . ?a)))
    )


(major-mode-hydra-define mu4e-headers-mode
  (:idle 0.3
    :color blue
    :body-pre (ig-hydra-reset)
    :quit-key ("q" "<escape>")
    :inherit (ig-base/heads)
    :separator " ")
  ("Email"
   (("c" igloo-org-capture-email "Capture email"))))

(major-mode-hydra-define mu4e-view-mode
  (:idle 0.3
    :color blue
    :body-pre (ig-hydra-reset)
    :quit-key ("q" "<escape>")
    :inherit (ig-base/heads)
    :separator " ")
  ("Email"
   (("c" igloo-org-capture-email "Capture email"))))

(define-key mu4e-headers-mode-map (kbd "C-c C-c") 'mu4e-org-store-and-capture)
(define-key mu4e-view-mode-map    (kbd "C-c C-c") 'mu4e-org-store-and-capture)

;; Dashboard
(use-package svg-tag-mode 
  :straight (:host github :repo "rougier/svg-tag-mode"))

(use-package mu4e-thread-folding
  :straight (:host github :repo "rougier/mu4e-thread-folding"))

(use-package mu4e-dashboard
  :straight (:host github :repo "rougier/mu4e-dashboard"))

;; Dashboard -------------------------------------------------------------------
(require 'mu4e)

;; Navigations functions -------------------------------------------------------
(defun igloo-email-close ()
  (interactive)
  (kill-buffer "*igloo-email*"))

;; Minor Mode / Keymaps --------------------------------------------------------
(define-minor-mode igloo-email-mode
  "Minor mode for igloo-email."
  :init nil
  :lighter "Email"
  :keymap (make-sparse-keymap)
  (when igloo-email-mode
    (setq buffer-read-only t)
    (setq cursor-type nil)))

(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-email-mode "q" #'igloo-email-close)

;; Display functions -----------------------------------------------------------
(defun igloo-email-sidebar ()
  (igloo-email-sidebar-mailboxes)
  (insert "\n\n\n\n")
  (igloo-email-sidebar-mailbox-times)
  (insert "\n\n")
  (igloo-email-sidebar-mailbox-additionals)
  )


;; Mailboxes -------------------------------------------------------------------
(defun igloo-email-sidebar-mailboxes ()
  "Mailbox section of the sidebar."
  (insert (igloo-email-sidebar-mailbox-header))
  (insert "\n")
  (insert "\n")
  (insert (igloo-email-sidebar-mailbox-inbox))
  (insert "\n")
  (insert (igloo-email-sidebar-mailbox-flagged))
  (insert "\n")
  (insert (igloo-email-sidebar-mailbox-drafts))
  (insert "\n")
  (insert (igloo-email-sidebar-mailbox-sent))
  (insert "\n")
  (insert (igloo-email-sidebar-mailbox-archive)))

(defun igloo-email-sidebar-mailbox-header ()
  "Sidebar: Header line for mailboxes."
  (concat (all-the-icons-material "mail") " Unread"))

(defun igloo-email-sidebar-mailbox-inbox ()
  "Sidebar inbox mailbox."
  (concat (all-the-icons-material "inbox") " Inbox"))

(defun igloo-email-sidebar-mailbox-flagged ()
  "Sidebar flagged mailbox."
  (concat (all-the-icons-material "flag") " Flagged"))

(defun igloo-email-sidebar-mailbox-drafts ()
  "Sidebar drafts mailbox."
  (concat (all-the-icons-material "line_weight") " Drafts"))

(defun igloo-email-sidebar-mailbox-sent ()
  "Sidebar drafts mailbox."
  (concat (all-the-icons-material "play_arrow") " Sent"))

(defun igloo-email-sidebar-mailbox-archive ()
  "Sidebar drafts mailbox."
  (concat (all-the-icons-material "archive") " Archive"))

;; Mailbox Times ---------------------------------------------------------------
(defun igloo-email-sidebar-mailbox-times ()
  "Time mailbox section of the sidebar."
  (insert (igloo-email-sidebar-mailbox-today))
  (insert "\n")
  (insert (igloo-email-sidebar-mailbox-yesterday))
  (insert "\n")
  (insert (igloo-email-sidebar-mailbox-last-week))
  (insert "\n")
  (insert (igloo-email-sidebar-mailbox-last-month))
  )

(defun igloo-email-sidebar-mailbox-today ()
  "Sidebar today mailbox."
  (concat (all-the-icons-material "album") " Today"))

(defun igloo-email-sidebar-mailbox-yesterday ()
  "Sidebar yesterday mailbox."
  (concat (all-the-icons-material "today") " Yesterday"))

(defun igloo-email-sidebar-mailbox-last-week ()
  "Sidebar last week mailbox."
  (concat (all-the-icons-material "today") " Last week"))

(defun igloo-email-sidebar-mailbox-last-month ()
  "Sidebar last month mailbox."
  (concat (all-the-icons-material "today") " Last month"))


;; Mailbox attachments ---------------------------------------------------------
(defun igloo-email-sidebar-mailbox-additionals ()
  "Mailbox additionals section of the sidebar."
  (insert (igloo-email-sidebar-mailbox-attachments)))

(defun igloo-email-sidebar-mailbox-attachments ()
  "Sidebar attachments mailbox."
  (concat (all-the-icons-material "attach_file") " Attachments"))



;; Main display ----------------------------------------------------------------
(defun igloo-email ()
  (interactive)
  (with-current-buffer (get-buffer-create "*igloo-email*")
    (switch-to-buffer "*igloo-email*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (set-window-margins nil 1)

      (setq header-line-format nil)
      (insert "\n")
      (igloo-email-sidebar)
      )
    (igloo-email-mode 1)))

(provide 'ig-email)
;;; ig-email.el ends here
