;;; ig-keybinds.el --- Keybindings configuration -*- lexical-binding: t -*-
(require 'org)

(defvar ig-leader-key "SPC"
  "The leader prefix key.")

(defvar ig-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states.")

(defvar ig-localleader-key "SPC m"
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

(defvar ig-escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users).
More specifically, when `ig/escape' is pressed. If any hook returns non-nil,
all hooks after it are ignored.")

(defun ig/escape (&optional interactive)
  "Run `ig-escape-hook'."
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'ig-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

(global-set-key [remap keyboard-quit] #'ig/escape)
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)


(use-package hydra
             :straight t)

(use-package major-mode-hydra
   :straight t)

(use-package pretty-hydra
   :straight t)


(defgroup ig-hydra nil
  "Customization for `ig-hydra'."
  :tag "ig-hydra")


;;* ig-hydra utilities

;; Lexical closure to encapsulate the stack variable.
(let ((ig-hydra-stack '()))
  (defun ig-hydra-push (expr)
    "Push an EXPR onto the stack."
    (push expr ig-hydra-stack))

  (defun ig-hydra-pop ()
    "Pop an expression off the stack and call it."
    (interactive)
    (let ((x (pop ig-hydra-stack)))
      (when x
	(call-interactively x))))

  (defun ig-hydra ()
    "Show the current stack."
    (interactive)
    (with-help-window (help-buffer)
      (princ "Ig-hydra-stack\n")
      (pp ig-hydra-stack)))

  (defun ig-hydra-reset ()
    "Reset the stack to empty."
    (interactive)
    (setq ig-hydra-stack '())))

(defmacro ig-open-hydra (hydra)
  "Push current HYDRA to a stack.
This is a macro so I don't have to quote the hydra name."
  `(progn
     (ig-hydra-push hydra-curr-body-fn)
     (call-interactively ',hydra)))

(defun ig-hydra-help ()
  "Show help buffer for current hydra."
  (interactive)
  (with-help-window (help-buffer)
    ;; (with-current-buffer (help-buffer)
    ;;   (unless (featurep 'emacs-keybinding-command-tooltip-mode)
	;; (require 'emacs-keybinding-command-tooltip-mode))
      ;; (emacs-keybinding-command-tooltip-mode +1))
    (let ((s (format "Help for %s\n" hydra-curr-body-fn)))
      (princ s)
      (princ (make-string (length s) ?-))
      (princ "\n"))

    (princ (mapconcat
	    (lambda (head)
	      (format "%s%s"
		      ;;  key
		      (s-pad-right 10 " " (car head))
		      ;; command
		      (let* ((hint (if (stringp (nth 2 head))
				       (concat " " (nth 2 head))
				     ""))
			     (cmd (cond
				   ;; quit
				   ((null (nth 1 head))
				    "")
				   ;; a symbol
				   ((symbolp (nth 1 head))
				    (format "`%s'" (nth 1 head)))
				   ((and (listp (nth 1 head))
					 (eq 'ig-open-hydra (car (nth 1 head))))
				    (format "`%s'" (nth 1 (nth 1 head))))
				   ((listp (nth 1 head))
				    (with-temp-buffer
				      (pp (nth 1 head) (current-buffer))
				      (let ((fill-prefix (make-string 10 ? )))
					(indent-code-rigidly
					 (save-excursion
					   (goto-char (point-min))
					   (forward-line)
					   (point))
					 (point-max) 10))
				      (buffer-string)))
				   (t
				    (format "%s" (nth 1 head)))))
			     (l1 (format "%s%s" (s-pad-right 50 " " (car (split-string cmd "\n"))) hint))
			     (s (s-join "\n" (append (list l1) (cdr (split-string cmd "\n"))))))
			(s-pad-right 50 " " s))))
	    (symbol-value
	     (intern
	      (replace-regexp-in-string
	       "/body$" "/heads"
	       (symbol-name  hydra-curr-body-fn))))
	    "\n"))))


;; Base hydra to inherit from
(defhydra ig-base (:color teal)
  "base"
  (":" counsel-M-x "M-x")
  ("?" ig-hydra-help "Menu help")
  ("m" major-mode-hydra "Major mode hydra")
  ("-" popper-toggle-latest "Popup"))


;; Main hydra - SPC
(pretty-hydra-define ig-hydra
    (:idle 0.3
    :title "Igloo"
    :color teal
    :body-pre (ig-hydra-reset)
    :quit-key ("q" "<escape>")
    :inherit (ig-base/heads)
    :separator " ")
  ("Search"
  (("f" (ig-open-hydra ig-hydra-find/body) "Find")
   ("p" (ig-open-hydra ig-hydra-project/body) "Project")
   ("s" (ig-open-hydra ig-hydra-search/body) "Search")
   ("," counsel-projectile-switch-to-buffer "Switch to project buffer")
   ("SPC" projectile-find-file "Find in project"))

   "Application"
   (("o" (ig-open-hydra ig-hydra-open/body) "Open")
    ("g" (ig-open-hydra ig-hydra-git/body) "Git")
    ("c" (ig-open-hydra ig-hydra-compile/body) "Compile")
    ("X" counsel-org-capture "Org capture"))

   "Emacs"
   (("i" (ig-open-hydra ig-hydra-insert/body) "Insert")
    ("b" (ig-open-hydra ig-hydra-buffer/body) "Buffer")
    ("w" (ig-open-hydra ig-hydra-windows/body) "Window")
    ("h" (ig-open-hydra ig-hydra-help/body) "Help"))))


;;** applications

(defun ig-app-hints ()
  "Calculate some variables for the applications hydra."
  (setq elfeed-count
	(s-pad-right 12 " "
		     (if (get-buffer "*elfeed-search*")
			 (format "RSS(%s)"
				 (car (s-split "/" (with-current-buffer "*elfeed-search*"
						     (elfeed-search--count-unread)))))
		       "RSS(?)"))))

;; Help ------------------------------------------------------------------------
(pretty-hydra-define ig-hydra-help 
  (:idle 0.3
         :color blue
         :body-pre (ig-hydra-reset)
         :quit-key ("q" "<escape>")
         :inherit (ig-base/heads)
         :separator " ")
  ("Help"
   (("f" counsel-describe-function "Describe function")
    ("v" counsel-describe-variable "Describe variable")
    ("k" describe-key "Describe key"))))


;; Open ----------------------------------------------------------------

;; Hydra org agenda (replaces the *Agenda Commands* with a hydra)
(pretty-hydra-define ig-hydra-org-agenda 
  (:idle 0.3
         :color blue
         :body-pre (ig-hydra-reset)
         :quit-key ("q" "<escape>")
         :inherit (ig-base/heads)
         :separator " ")
  ("Org Agenda"
   (("a" org-agenda-list "Weekly agenda")
    ("t" org-todo-list "Todo list"))))


(pretty-hydra-define ig-hydra-open (:hint nil
				     :pre (ig-app-hints)
				     :color teal
                     :quit-key ("q" "<escape>")
				     :inherit (ig-base/heads)
                     :idle 0.3
                     :separator " ")
  ("Emacs"

  (("t" term "Term")
  ("e"  eshell "Eshell")
  ("a"  (ig-open-hydra ig-hydra-org-agenda/body) "Agenda")
  ("u"  mu4e "Email")
  ("p"  igloo-org-open-projects "Projects"))
  
  "OS"
  (("b" bash "bash")
  ("f" finder "Finder"))

  "Web"
  (("c" google-calendar "Calendar")
  ("g" google "Google")
  ("o" (ig-open-hydra ig-office/body) "MS Office")
  ("G" (ig-open-hydra ig-gsuite/body) "GSuite")
  ("s" slack/body "Slack")  
  ("k" package-list-packages "List packages")
  ("m" compose-mail "Compose mail"))))


;; Windows ---------------------------------------------------------------------

(pretty-hydra-define ig-hydra-windows 
  (:idle 0.3
    :color teal
    :body-pre (ig-hydra-reset)
    :quit-key ("<escape>")
    :inherit (ig-base/heads)
    :separator " ")
 ("Window"
  (("l" evil-window-right "select right window")
   ("h" evil-window-left "select left window")
   ("k" evil-window-up "switch up window")
   ("j" evil-window-down "switch down window")
   ("s" evil-window-split "horizontal split")
   ("v" evil-window-vsplit "vertical split"))
  "Resize"
   (("q" evil-quit "quit window")
    ("=" balance-windows "resize windows")
    ("r" window-swap-states "swap windows"))))


;; Find ------------------------------------------------------------------------
(defun ig-find-file-in-private-directory ()
  (interactive)
  (counsel-find-file "~/igloo/snowblocks/emacs/"))

(pretty-hydra-define ig-hydra-find 
  (:idle 0.3
         :color teal
         :body-pre (ig-hydra-reset)
         :quit-key ("q" "<escape>")
         :inherit (ig-base/heads)
         :separator " ")
 ("Find"
  (("f" find-file "find file")
   ("p" ig-find-file-in-private-directory "open config"))))


;; Buffer ----------------------------------------------------------------------

(pretty-hydra-define ig-hydra-buffer 
  (:idle 0.3
         :color teal
         :body-pre (ig-hydra-reset)
         :quit-key ("q" "<escape>")
         :inherit (ig-base/heads)
         :separator " ")
("Buffer"
   (("b" projectile-switch-to-buffer "switch project buffer")
    ("B" switch-to-buffer "switch buffer")
    ("d" kill-current-buffer "kill buffer")
    ("K" kill-all-buffers "kill all buffers"))))

;; Git -------------------------------------------------------------------------
(pretty-hydra-define ig-hydra-git 
  (:idle 0.3
         :color teal
         :body-pre (ig-hydra-reset)
         :quit-key ("q" "<escape>")
         :inherit (ig-base/heads)
         :separator " ")
    ("Git"
    (("g" magit-status "magit status")
    ("b" magit-branch-checkout "magit switch branch")
    ("B" magit-blame "magit blame")
    ("F" magit-fetch "magit fetch")
    ("L" magit-log "magit log"))))


;; Insert ----------------------------------------------------------------------
(pretty-hydra-define ig-hydra-insert 
  (:idle 0.3
         :color teal
         :body-pre (ig-hydra-reset)
         :quit-key ("q" "<escape>")
         :inherit (ig-base/heads)
         :separator " ")
    ("Insert"
    (("s" yas-insert-snippet))))


;; Project ---------------------------------------------------------------------
(pretty-hydra-define ig-hydra-project 
  (:idle 0.3
         :color teal
         :body-pre (ig-hydra-reset)
         :quit-key ("q" "<escape>")
         :inherit (ig-base/heads)
         :separator " ")
  ("Project"
   (("p" projectile-switch-project)
    ("d" projectile-remove-known-project)
    ("a" projectile-add-known-project)
    ("i" projectile-invalidate-cache))))


;; Compile ---------------------------------------------------------------------
(pretty-hydra-define ig-hydra-compile 
  (:idle 0.3
         :color teal
         :body-pre (ig-hydra-reset)
         :quit-key ("q" "<escape>")
         :inherit (ig-base/heads)
         :separator " ")
    ("Compile"
    (("c" projectile-compile-project)
     ("f" counsel-flycheck))))

;; Search ----------------------------------------------------------------------
(pretty-hydra-define ig-hydra-search 
    (:idle 0.3
        :color teal
        :body-pre (ig-hydra-reset)
        :quit-key ("q" "<escape>")
        :inherit (ig-base/heads)
        :separator " ")
    ("Search"
    (("b" swiper "search buffer")
    ("i" counsel-imenu "imenu")
    ("g" igloo/counsel-rg-project "ripgrep")
    ("p" igloo/counsel-rg-project-at-point "ripgrep at point"))))

(use-package general
  :straight t)


(general-define-key
  :states '(emacs normal hybrid motion visual operator)
  :keymaps 'override
  ig-leader-key 'ig-hydra/body)


(provide 'ig-keybinds)

;;; ig-keybinds.el ends here 
