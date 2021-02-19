;; Lib -------------------------------------------------------------------------

;;;###autoload
(defvar +popup--display-buffer-alist nil)

;;;###autoload
(defvar +popup-defaults
  (list :side   'bottom
        :height 0.16
        :width  40
        :quit   t
        :select #'ignore
        :ttl    5)
  "Default properties for popup rules defined with `set-popup-rule!'.")

;;;###autoload
(defun +popup-make-rule (predicate plist)
  (if (plist-get plist :ignore)
      (list predicate nil)
    (let* ((plist (append plist +popup-defaults))
           (alist
            `((actions       . ,(plist-get plist :actions))
              (side          . ,(plist-get plist :side))
              (size          . ,(plist-get plist :size))
              (window-width  . ,(plist-get plist :width))
              (window-height . ,(plist-get plist :height))
              (slot          . ,(plist-get plist :slot))
              (vslot         . ,(plist-get plist :vslot))))
           (params
            `((ttl      . ,(plist-get plist :ttl))
              (quit     . ,(plist-get plist :quit))
              (select   . ,(plist-get plist :select))
              (modeline . ,(plist-get plist :modeline))
              (autosave . ,(plist-get plist :autosave))
              ,@(plist-get plist :parameters))))
      `(,predicate (+popup-buffer)
                   ,@alist
                   (window-parameters ,@params)))))

;;;###autodef
(defun set-popup-rule (predicate &rest plist)
  "Define a popup rule.

These rules affect buffers displayed with `pop-to-buffer' and `display-buffer'
(or their siblings). Buffers displayed with `switch-to-buffer' (and its
variants) will not be affected by these rules (as they are unaffected by
`display-buffer-alist', which powers the popup management system).

PREDICATE can be either a) a regexp string (matched against the buffer's name)
or b) a function that takes two arguments (a buffer name and the ACTION argument
of `display-buffer') and returns a boolean.

PLIST can be made up of any of the following properties:

:ignore BOOL
  If BOOL is non-nil, popups matching PREDICATE will not be handled by the popup
  system. Use this for buffers that have their own window management system like
  magit or helm.

:actions ACTIONS
  ACTIONS is a list of functions or an alist containing (FUNCTION . ALIST). See
  `display-buffer''s second argument for more information on its format and what
  it accepts. If omitted, `+popup-default-display-buffer-actions' is used.

:side 'bottom|'top|'left|'right
  Which side of the frame to open the popup on. This is only respected if
  `+popup-display-buffer-stacked-side-window-fn' or `display-buffer-in-side-window'
  is in :actions or `+popup-default-display-buffer-actions'.

:size/:width/:height FLOAT|INT|FN
  Determines the size of the popup. If more than one of these size properties are
  given :size always takes precedence, and is mapped with window-width or
  window-height depending on what :side the popup is opened. Setting a height
  for a popup that opens on the left or right is harmless, but comes into play
  if two popups occupy the same :vslot.

  If a FLOAT (0 < x < 1), the number represents how much of the window will be
    consumed by the popup (a percentage).
  If an INT, the number determines the size in lines (height) or units of
    character width (width).
  If a function, it takes one argument: the popup window, and can do whatever it
    wants with it, typically resize it, like `+popup-shrink-to-fit'.

:slot/:vslot INT
  (This only applies to popups with a :side and only if :actions is blank or
  contains the `+popup-display-buffer-stacked-side-window-fn' action) These control
  how multiple popups are laid out. INT can be any integer, positive and
  negative.

  :slot controls lateral positioning (e.g. the horizontal positioning for
    top/bottom popups, or vertical positioning for left/right popups).
  :vslot controls popup stacking (from the edge of the frame toward the center).

  Let's assume popup A and B are opened with :side 'bottom, in that order.
    If they possess the same :slot and :vslot, popup B will replace popup A.
    If popup B has a higher :slot, it will open to the right of popup A.
    If popup B has a lower :slot, it will open to the left of popup A.
    If popup B has a higher :vslot, it will open above popup A.
    If popup B has a lower :vslot, it will open below popup A.

:ttl INT|BOOL|FN
  Stands for time-to-live. It can be t, an integer, nil or a function. This
  controls how (and if) the popup system will clean up after the popup.

  If any non-zero integer, wait that many seconds before killing the buffer (and
    any associated processes).
  If 0, the buffer is immediately killed.
  If nil, the buffer won't be killed and is left to its own devices.
  If t, resort to the default :ttl in `+popup-defaults'. If none exists, this is
    the same as nil.
  If a function, it takes one argument: the target popup buffer. The popup
    system does nothing else and ignores the function's return value.

:quit FN|BOOL|'other|'current
  Can be t, 'other, 'current, nil, or a function. This determines the behavior
  of the ESC/C-g keys in or outside of popup windows.

  If t, close the popup if ESC/C-g is pressed anywhere.
  If 'other, close this popup if ESC/C-g is pressed outside of any popup. This
    is great for popups you may press ESC/C-g a lot in.
  If 'current, close the current popup if ESC/C-g is pressed from inside of the
    popup. This makes it harder to accidentally close a popup until you really
    want to.
  If nil, pressing ESC/C-g will never close this popup.
  If a function, it takes one argument: the to-be-closed popup window, and is
    run when ESC/C-g is pressed while that popup is open. It must return one of
    the other values to determine the fate of the popup.

:select BOOL|FN
  Can be a boolean or function. The boolean determines whether to focus the
  popup window after it opens (non-nil) or focus the origin window (nil).

  If a function, it takes two arguments: the popup window and originating window
    (where you were before the popup opened). The popup system does nothing else
    and ignores the function's return value.

:modeline BOOL|FN|LIST
  Can be t (show the default modeline), nil (show no modeline), a function that
  returns a modeline format or a valid value for `mode-line-format' to be used
  verbatim. The function takes no arguments and is run in the context of the
  popup buffer.

:autosave BOOL|FN
  This parameter determines what to do with modified buffers when closing popup
  windows. It accepts t, 'ignore, a function or nil.

  If t, no prompts. Just save them automatically (if they're file-visiting
    buffers). Same as 'ignore for non-file-visiting buffers.
  If nil (the default), prompt the user what to do if the buffer is
    file-visiting and modified.
  If 'ignore, no prompts, no saving. Just silently kill it.
  If a function, it is run with one argument: the popup buffer, and must return
    non-nil to save or nil to do nothing (but no prompts).

:parameters ALIST
  An alist of custom window parameters. See `(elisp)Window Parameters'.

If any of these are omitted, defaults derived from `+popup-defaults' will be
used.

\(fn PREDICATE &key IGNORE ACTIONS SIDE SIZE WIDTH HEIGHT SLOT VSLOT TTL QUIT SELECT MODELINE AUTOSAVE PARAMETERS)"
  (declare (indent defun))
  (push (+popup-make-rule predicate plist) +popup--display-buffer-alist)
  (when (bound-and-true-p +popup-mode)
    (setq display-buffer-alist +popup--display-buffer-alist))
  +popup--display-buffer-alist)


;; Config ----------------------------------------------------------------------

(setq default-frame-alist
      (append (list
	       '(font . "Roboto Mono:style=Light:size=12")
	       ;; '(font . "Roboto Mono Emacs Regular:size=14")
	       '(min-height . 1)  '(height     . 45)
	       '(min-width  . 1) '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 5)
               '(left-fringe    . 2)
               '(right-fringe   . 2)
               '(tool-bar-lines . 1)
               '(menu-bar-lines . 1))))


(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
;; (global-hl-line-mode 1)
(setq x-underline-at-descent-line t)

;; Line numbers
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(setq-default
 display-line-numbers-current-absolute nil
 display-line-numbers-type 'absolute
 display-line-numbers-width 2)

;; Vertical window divider
(setq window-divider-default-right-width 5)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Hide org markup for README
(setq org-hide-emphasis-markers t)


;; No startup  screen
(setq inhibit-startup-screen t)

;; No startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer 
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq cursor-in-non-selected-windows nil)

;; Moderate font lock
(setq font-lock-maximum-decoration nil)

;; No limit on font lock
(setq font-lock-maximum-size nil)

;; No line break space points
(setq auto-fill-mode nil)

;; Fill column at 80
(setq fill-column 80)

;; No scroll bars
(scroll-bar-mode 0)

;; No toolbar
(tool-bar-mode 0)

;; No menu bar
(if (display-graphic-p)
    (menu-bar-mode t) ;; When nil, focus problem on OSX
  (menu-bar-mode -1))

;; Navigate windows using shift+direction
(windmove-default-keybindings)

;; Tab behavior
;; (setq tab-always-indent 'complete)
;; (global-company-mode)
;; (define-key company-mode-map [remap indent-for-tab-command]
;;   #'company-indent-or-complete-common)

;; Pixel scroll (as opposed to char scrool)
(pixel-scroll-mode t)

;; Mac specific
;; (setq ns-use-native-fullscreen t
;;       mac-option-key-is-meta nil
;;       mac-command-key-is-meta t
;;       mac-command-modifier 'meta
;;       mac-option-modifier nil
;;       mac-use-title-bar nil)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq tab-width 4)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 5)

;; Minimum window height
(setq window-min-height 1)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)



(provide 'igloo-ui)
