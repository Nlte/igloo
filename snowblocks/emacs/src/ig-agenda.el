;;; ig-agenda.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package ts
  :straight t)

(require 'evil)
(require 'org)
(require 'org-agenda)
(require 'calendar)
(require 'holidays)


;; --- Faces -----------------------------------------------------------
(defgroup igloo-agenda-faces nil
  "Igloo-Agenda faces")

(defface igloo-agenda-face-default
  '((t :inherit 'default ))
  "Default face"
  :group 'igloo-agenda-faces)

(defface igloo-agenda-face-selected
  `((t :foreground ,igloo-background
       :background ,igloo-foreground ))
  "Face for the selected day"
  :group 'igloo-agenda-faces)

(defface igloo-agenda-face-today
  `((t :foreground ,igloo-popout
       :inherit 'bold ))
  "Today face when not selected."
  :group 'igloo-agenda-faces)

(defface igloo-agenda-face-selected-today
  `((t :foreground ,igloo-background
       :background ,igloo-popout ))
  "Today face when selected."
  :group 'igloo-agenda-faces)

(defface igloo-agenda-face-weekend
  `((t :foreground ,igloo-faded ))
  "Weekend face"
  :group 'igloo-agenda-faces)

(defface igloo-agenda-face-holidays
  `((t :foreground ,igloo-faded ))
  "Holidays face"
  :group 'igloo-agenda-faces)

(defface igloo-agenda-face-outday
  `((t :foreground ,igloo-subtle ))
  "Out day face"
  :group 'igloo-agenda-faces)

(defface igloo-agenda-face-day-name
  `((t :foreground ,igloo-faded ))
  "Day name face (on second line)"
  :group 'igloo-agenda-faces)

(defface igloo-agenda-face-month-name
  '((t :inherit 'bold ))
  "Month name face (on first line)"
  :group 'igloo-agenda-faces)

(defface igloo-agenda-face-mouse
  '((t :inherit 'highlight ))
  "Mouse highlight face"
  :group 'igloo-agenda-faces)

(defface igloo-agenda-face-button
  `((t :foreground ,igloo-faded ))
  "Header button (left and right)"
  :group 'igloo-agenda-faces)


;; --- Global variable -------------------------------------------------
(setq igloo-agenda-selected (ts-now))
(setq igloo-agenda-file "~/org/agenda.org")


;; --- Useful functions ------------------------------------------------
(defun center-string (string size)
  (let* ((padding (/ (- size (length string)) 2))
         (lpad (+ (length string) padding))
         (lformat (format "%%%ds" lpad))
         (rformat (format "%%%ds" (- size))))
    (format rformat (format lformat string))))


;; Igloo-Agenda minor mode -----------------------------------------------------
(define-minor-mode igloo-agenda-mode
  "Minor mode for igloo-agenda."
  :init nil
  :lighter "Calendar"
  :keymap (make-sparse-keymap)
  
  (when igloo-agenda-mode
    (setq buffer-read-only t)
    (setq cursor-type nil)))

(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-agenda-mode "q" #'igloo-agenda-close)
(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-agenda-mode "t" #'igloo-agenda-select-today)
(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-agenda-mode (kbd "<left>") #'igloo-agenda-backward-day)
(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-agenda-mode (kbd "<right>") #'igloo-agenda-forward-day)
(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-agenda-mode (kbd "<down>") #'igloo-agenda-forward-week)
(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-agenda-mode (kbd "<up>") #'igloo-agenda-backward-week)
(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-agenda-mode (kbd "<S-left>") #'igloo-agenda-backward-month)
(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-agenda-mode (kbd "<S-right>") #'igloo-agenda-forward-month)
(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-agenda-mode (kbd "<S-down>") #'igloo-agenda-forward-year)
(evil-define-key '(normal visual replace operator motion emacs)
  'igloo-agenda-mode (kbd "<S-up>") #'igloo-agenda-backward-year)

;; Navigation ------------------------------------------------------------------
(defun igloo-agenda-forward-day ()
  (interactive)
  (setq igloo-agenda-selected (ts-inc 'day 1 igloo-agenda-selected))
  (igloo-agenda))

(defun igloo-agenda-backward-day ()
  (interactive)
  (setq igloo-agenda-selected (ts-dec 'day 1 igloo-agenda-selected))
  (igloo-agenda))

(defun igloo-agenda-forward-week ()
  (interactive)
  (setq igloo-agenda-selected (ts-inc 'day 7 igloo-agenda-selected))
  (igloo-agenda))

(defun igloo-agenda-backward-week ()
  (interactive)
  (setq igloo-agenda-selected (ts-dec 'day 7 igloo-agenda-selected))
  (igloo-agenda))

(defun igloo-agenda-forward-month ()
  (interactive)
  (setq igloo-agenda-selected (ts-inc 'month 1 igloo-agenda-selected))
  (igloo-agenda))

(defun igloo-agenda-backward-month ()
  (interactive)
  (setq igloo-agenda-selected (ts-dec 'month 1 igloo-agenda-selected))
  (igloo-agenda))

(defun igloo-agenda-forward-year ()
  (interactive)
  (setq igloo-agenda-selected (ts-inc 'year 1 igloo-agenda-selected))
  (igloo-agenda))

(defun igloo-agenda-backward-year ()
  (interactive)
  (setq igloo-agenda-selected (ts-dec 'year 1 igloo-agenda-selected))
  (igloo-agenda))

(defun igloo-agenda-select-today ()
  (interactive)
  (setq igloo-agenda-selected (ts-now))
  (igloo-agenda))

(defun igloo-agenda-close ()
  (interactive)
  (kill-buffer "*igloo-agenda*"))

(defun igloo-agenda-select ()
  (interactive)
  (kill-buffer "*igloo-agenda*"))

(defun igloo-agenda-cancel ()
  (interactive)
  (kill-buffer "*igloo-agenda*"))

;; Display functions -----------------------------------------------------------
(defun igloo-agenda-header-month (selected)
  (let* ((map-left (make-sparse-keymap))
         (map-right (make-sparse-keymap)))

    (define-key map-left (kbd "<down-mouse-1>") #'igloo-agenda-backward-month)
    (define-key map-right (kbd "<down-mouse-1>") #'igloo-agenda-forward-month)
    
    (concat
     (propertize "<" 'face 'igloo-agenda-face-button
                     'mouse-face 'igloo-agenda-face-mouse
                     'help-echo "Previous month"
                     'keymap map-left)

     (propertize (center-string (format "%s %d" (ts-month-name selected)
                                                (ts-year selected)) 18)
                 'face 'igloo-agenda-face-month-name)
     
     (propertize ">" 'face 'igloo-agenda-face-button
                     'mouse-face 'igloo-agenda-face-mouse
                     'help-echo "Next month"
                     'keymap map-right)
     " ")))

(defun igloo-agenda-header-names (selected)
  (propertize "Mo Tu We Th Fr Sa Su "
              'face 'igloo-agenda-face-day-name))

(defun igloo-agenda-body-days (selected)
  (let* ((today  (ts-now))
         (day    (ts-day   selected))
         (month  (ts-month selected))
         (year   (ts-year  selected))
         (start (make-ts :year year :month month :day 1
                         :hour 0 :minute 0 :second 0))
         (dow   (mod (+ 6 (ts-dow start)) 7))
         (start (ts-dec 'day dow start))
         (result ""))

    (dotimes (row 6)
      (dotimes (col 7)
        (let* ((day (+ (* row 7) col))
               (date (ts-inc 'day day start))
               (map (make-sparse-keymap))
               (is-today (and (= (ts-year date) (ts-year today))
                              (= (ts-doy date) (ts-doy today))))
               (is-selected (and (= (ts-year date) (ts-year selected))
                                 (= (ts-doy date) (ts-doy selected))))
               (is-selected-today (and is-selected is-today))
               (is-outday (not (= (ts-month date) month)))
               (is-holidays (calendar-check-holidays (list
                                                      (ts-month date)
                                                      (ts-day date)
                                                      (ts-year date))))
               (is-weekend (or (= (ts-dow date) 0) (= (ts-dow date) 6)))
               (face (cond (is-selected-today 'igloo-agenda-face-selected-today)
                           (is-selected       'igloo-agenda-face-selected)
                           (is-today          'igloo-agenda-face-today)
                           (is-outday         'igloo-agenda-face-outday)
                           (is-weekend        'igloo-agenda-face-weekend)
                           (is-holidays       'igloo-agenda-face-holidays)
                           (t                 'igloo-agenda-face-default))))
          
            (setq result (concat result
                                 (propertize (format "%2d" (ts-day date))
                                             'face face
                                             'mouse-face (cond (is-selected-today 'igloo-agenda-face-selected-today)
                                                               (is-selected       'igloo-agenda-face-selected)
                                                               (t 'igloo-agenda-face-mouse))
                                             'help-echo (format "%s%s" (ts-format "%A %-e %B %Y" date)
                                                                (if is-holidays (format " (%s)" (nth 0 is-holidays))
                                                                     ""))
                                             'keymap map)
                                 " "))))
      (setq result (concat result "\n")))
    
    result))


;; Main display ---------------------------------------------------------------------
(defun igloo-agenda (&optional selected)
  (interactive)
  
  (if selected
      (setq igloo-agenda-selected selected))

  (with-current-buffer (get-buffer-create "*igloo-agenda*")
    (switch-to-buffer "*igloo-agenda*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (set-window-margins nil 2)
      (face-remap-add-relative 'header-line
                               `(:family "Roboto Mono"
                                         :foreground ,igloo-background
                                         :background ,igloo-faded
                                         :weight regular
                                         :box (:line-width 2 :color "#ffffff" :style nil)))

      (setq header-line-format nil)

      (insert "\n")
      (insert (igloo-agenda-header-month igloo-agenda-selected))
      (insert "\n")
      (insert (igloo-agenda-header-names igloo-agenda-selected))
      (insert "\n")
      (insert (igloo-agenda-body-days igloo-agenda-selected))
      (insert "\n")
      
      ;; --- Org agenda entries ---
      (goto-char (point-min))
      (forward-line 1)
      (end-of-line)
      (insert (concat (propertize (ts-format "  %A %-e %B %Y" igloo-agenda-selected)
                                  'face 'igloo-agenda-face-month-name)
                      ))
      (forward-line 2)
      (end-of-line))

    (igloo-agenda-mode 1)
    ))

;;
(provide 'igloo-agenda)

;;; ig-agenda.el ends here
