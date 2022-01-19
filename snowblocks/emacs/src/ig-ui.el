;;; ig-ui.el --- UI configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(require 'ig-theme)
(load-theme 'ig t)

;; (require 'ig-nord-theme)
;; (load-theme 'ig-nord t)

(require 'ig-modeline)
(ig-modeline)

(use-package all-the-icons
  :if (display-graphic-p)
  :straight t)

(use-package nerd-fonts
  :straight (:host github :repo "twlz0ne/nerd-fonts.el"))

(use-package neotree
  :straight t)


;; SVG lib ---------------------------------------------------------------------
(defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
(defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
(defconst day-re "[A-Za-z]\\{3\\}")

(defun svg-progress-percent (value)
  (svg-image (svg-lib-concat
              (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                    nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
              (svg-lib-tag (concat value "%")
                           nil :stroke 0 :margin 0)) :ascent 'center))

(defun svg-progress-count (value)
  (let* ((seq (mapcar #'string-to-number (split-string value "/")))
         (count (float (car seq)))
         (total (float (cadr seq))))
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ count total) nil
                                      :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag value nil
                             :stroke 0 :margin 0)) :ascent 'center)))

(use-package svg-lib
  :straight t)

(use-package svg-tag-mode 
  :straight (:host github :repo "rougier/svg-tag-mode")
  :init (add-hook 'org-mode-hook 'svg-tag-mode)
  :config
  (setq svg-tag-tags
        `(
          ;; Org tags
          (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          (":\\(@[A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
          (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))
          
          ;; Task priority
          ("\\[#[A-Z]\\]" . ( (lambda (tag)
                                (svg-tag-make tag :face 'org-priority 
                                              :beg 2 :end -1 :margin 0))))
          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))
          
          ;; TODO keywords
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
          ("NEXT" . ((lambda (tag) (svg-tag-make "NEXT" :face 'org-todo :inverse nil :margin 0))))
          ("STRT" . ((lambda (tag) (svg-tag-make "STRT" :face 'org-todo :inverse nil :margin 0))))
          ("HOLD" . ((lambda (tag) (svg-tag-make "HOLD" :face '+org-todo-onhold :inverse t :margin 0))))
          ("KILL" . ((lambda (tag) (svg-tag-make "KILL" :face '+org-todo-onhold :inverse t :margin 0))))
          
          ;; Active date (without day name, with or without time)
          (,(format "\\(<%s>\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0))))
          (,(format "\\(<%s *\\)%s>" date-re time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
          (,(format "<%s *\\(%s>\\)" date-re time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

          ;; Inactive date  (without day name, with or without time)
          (,(format "\\(\\[%s\\]\\)" date-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
          (,(format "\\(\\[%s *\\)%s\\]" date-re time-re) .
           ((lambda (tag)
              (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
          (,(format "\\[%s *\\(%s\\]\\)" date-re time-re) .
           ((lambda (tag)
              (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))))
  )


;; (use-package doom-themes
;;   :straight t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-nord-light t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))


(setq ring-bell-function 'ignore)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)


;; Popup rules -----------------------------------------------------------------
(use-package popper
  :straight t
  :bind (("C--"   . popper-toggle-latest)
         ("M--"   . popper-cycle)
         ("C-M--" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
          "\\*pytest\\*.*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))


;; Fixes
(defun his-tracing-function (orig-fun &rest args)
  (let ((original-modeline-format mode-line-format))
    (message "org-tag called with args %S" args)
    (message "removing modeline")
    (setq mode-line-format nil)
    (force-mode-line-update)
    (message "redrawing")
    (redraw-display)
    (let ((res (apply orig-fun args)))
      (message "restoring modeline")
      (setq mode-line-format original-modeline-format)
        (message "org-tag returned %S" res)
        res))
    )


;; Disable modeline for org fast tag selection (we can't see everything otherwise)
(advice-add 'org-fast-tag-selection :around #'his-tracing-function)

(defun igloo-hide-mode-line ()
  (setq mode-line-format nil))


;; Buffer display alist
(setq display-buffer-alist nil)
;; (add-to-list 'display-buffer-alist
;;                '("\\*Org tags\\*"
;;                  (display-buffer-in-direction)
;;                  (direction . bottom)
;;                  (window-width . fit-window-to-buffer)
;;                  (window-height . 300))
;;                )
;; (add-to-list 'display-buffer-alist
;;              '("\\*mu4e-headers\\*"
;;                (display-buffer-in-side-window)
;;                ))
(add-to-list 'display-buffer-alist
               '("*mu4e-headers*"
                 (display-buffer-in-side-window)
                 (side                . right)
                 ))

(provide 'ig-ui)
;;; ig-ui.el ends here
