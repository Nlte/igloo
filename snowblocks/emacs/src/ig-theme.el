;;; ig-theme.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;; References:
;; https://yeun.github.io/open-color/
;; https://github.com/rougier/igloo-theme
;; https://github.com/rougier/igloo-emacs

;; Code:


(deftheme ig 
  "Igloo theme")

(defcustom igloo-foreground "#37474F" ;; Blue Grey / L800
  "Default foreground color"
  :type 'color :group nil)

(defcustom igloo-background "#FFFFFF" ;; White
  "Default background color"
  :type 'color :group nil)

;; (defcustom igloo-background "#ECEFF4" ;; White
;;   "Default background color"
;;   :type 'color :group nil)

(defcustom igloo-highlight "#FAFAFA" ;; Very Light Grey
  "Highlight color is used to highlight part of the screen."
  :type 'color :group nil)

(defcustom igloo-subtle "#ECEFF1" ;; Blue Grey / L50
  "Subtle color is used to suggest a physical area on the screen."
  :type 'color :group nil)

(defcustom igloo-faded "#B0BEC5" ;; Blue Grey / L200
  "Faded face is for information that are less important."
  :type 'color :group nil)

(defcustom igloo-salient "#845ef7" ;; Deep Violet
  "Salient color is used for information that are important."
  :type 'color :group nil)

(defcustom igloo-strong "#263238" ;; Blue Grey / L900
  "Strong color is used for information of a structural nature."
  :type 'color :group nil)

(defcustom igloo-popout "#FAB005" ;; Deep Orange / L200
  "Popout colour is used for information that needs attention."
  :type 'color :group nil)

(defcustom igloo-string "#74b816" ;; Lime 
  "Popout colour is used for strings."
  :type 'color :group nil)

(defcustom igloo-critical "#FF6F00" ;; Amber / L900
  "Critical face is for information that requires immediate action."
  :type 'color :group nil)

(defface igloo-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group nil)

(defface igloo-critical-i nil
  "Critical face inversed."
  :group nil)

(defface igloo-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group nil)

(defface igloo-popout-i nil
  "Popout face inversed."
  :group nil)

(defface igloo-string nil
  "String face."
  :group nil)

(defface igloo-string-i nil
  "String face inversed."
  :group nil)

(defface igloo-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group nil)

(defface igloo-strong-i nil
  "Strong face inversed."
  :group nil)

(defface igloo-strong-hi nil
  "Strong face inversed."
  :group nil)

(defface igloo-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group nil)

(defface igloo-salient-i nil
  "Strong face inversed."
  :group nil)

(defface igloo-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group nil)

(defface igloo-faded-i nil
  "Faded face inversed."
  :group nil)

(defface igloo-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group nil)

(defface igloo-subtle-i nil
  "Subtle face inversed."
  :group nil)

(defface igloo-default nil
  "Default face."
  :group nil)

(defface igloo-default-i nil
  "Default face inversed."
  :group nil)

(defun set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))



(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces

   'ig

   ;; Base ---------------------------------------------------------------------
   `(default ((,class :background ,igloo-background :foreground ,igloo-foreground)))
   `(cursor ((,class :background ,igloo-background :foreground ,igloo-foreground)))
   `(mouse ((,class :background ,igloo-background :foreground ,igloo-foreground)))
   `(highlight ((,class :background ,igloo-highlight)))

   `(igloo-subtle ((,class (:background ,igloo-subtle))))

   `(igloo-subtle-i ((,class (:foreground ,igloo-subtle))))
   
   `(igloo-faded ((,class  (:foreground ,igloo-faded))))

   `(igloo-faded-i ((,class (:foreground ,igloo-background
                            :background ,igloo-faded))))
   
   `(igloo-default ((,class  (:foreground ,igloo-foreground))))

   `(igloo-default-i ((,class (:foreground ,igloo-background
                              :background ,igloo-foreground))))

   
   `(igloo-salient ((,class (:foreground ,igloo-salient))))

   `(igloo-salient-i ((,class (:foreground ,igloo-background
                              :background ,igloo-salient))))

   `(igloo-strong ((,class (:weight normal))))

   `(igloo-strong-i ((,class (:foreground ,igloo-background
                             :background ,igloo-strong
                             :weight normal))))

   `(igloo-strong-hi ((,class (:foreground ,igloo-strong
                             :background ,igloo-faded))))

   `(igloo-popout ((,class (:foreground ,igloo-popout))))

   `(igloo-popout-i ((,class (:foreground ,igloo-background
                             :background ,igloo-popout))))

   `(igloo-string ((,class (:foreground ,igloo-string))))

   `(igloo-string-i ((,class (:foreground ,igloo-background
                             :background ,igloo-string))))
   
   `(igloo-critical ((,class (:foreground ,igloo-background
                             :background ,igloo-critical))))

   `(igloo-critical-i ((,class (:foreground ,igloo-critical))))


   ;; Structural ---------------------------------------------------------------
   '(bold                        ((t (:inherit igloo-strong))))
   '(italic                      ((t (:inherit igloo-faded))))
   '(bold-italic                 ((t (:inherit igloo-strong))))
   '(region                      ((t (:inherit igloo-subtle))))
   '(fringe                      ((t (:inherit (igloo-faded)))))
   '(hl-line                     ((t (:inherit highlight))))
   '(link                        ((t (:inherit igloo-salient))))
   '(fixed-pitch                 ((t (:inherit default))))
   '(fixed-pitch-serif           ((t (:inherit default))))

   ;; Window divider -----------------------------------------------------------
   `(window-divider                ((,class (:foreground ,igloo-foreground))))
   '(window-divider-first-pixel    ((t (:inherit window-divider))))
   '(window-divider-last-pixel     ((t (:inherit window-divider))))
   ;; `(vertical-border               ((,class (:foreground ,igloo-background))))
   '(vertical-border               ((t (:inherit igloo-faded))))

   ;; Tab bar ------------------------------------------------------------------
   '(tab-bar                       ((t (:inherit default))))
   '(tab-bar-tab                   ((t (:inherit default))))
   '(tab-bar-tab-inactive          ((t (:inherit igloo-faded))))
   '(tab-line                      ((t (:inherit default))))

   ;; Mode line ----------------------------------------------------------------
   `(mode-line ((,class (:foreground ,igloo-foreground
                         :background ,igloo-subtle))))
   `(mode-line-highlight ((t (:inherit igloo-popout))))
   `(mode-line-buffer-id ((t (:weight regular))))
   `(mode-line-emphasis  ((t (:weight regular))))
               
   `(mode-line-inactive ((,class (:foreground ,igloo-foreground
                                  :background ,igloo-highlight))))

   ;; Semantic -----------------------------------------------------------------
   '(shadow                        ((t (:inherit igloo-faded))))
   '(success                       ((t (:inherit igloo-salient))))
   '(warning                       ((t (:inherit igloo-popout))))
   '(error                         ((t (:inherit igloo-critical))))
   '(match                         ((t (:inherit igloo-popout))))

   ;; General ------------------------------------------------------------------
   '(buffer-menu-buffer            ((t (:inherit igloo-strong))))
   '(minibuffer-prompt             ((t (:inherit igloo-strong))))
   '(isearch                       ((t (:inherit igloo-strong))))
   '(isearch-fail                  ((t (:inherit igloo-faded))))
   '(show-paren-match              ((t (:inherit igloo-strong))))
   '(show-paren-mismatch           ((t (:inherit igloo-critical))))
   '(lazy-highlight                ((t (:inherit igloo-subtle))))
   '(trailing-whitespace           ((t (:inherit igloo-subtle))))
   '(secondary-selection           ((t (:inherit igloo-subtle))))
   '(completions-annotations       ((t (:inherit igloo-faded))))
   '(completions-common-part       ((t (:inherit igloo-strong))))
   '(completions-first-difference  ((t (:inherit igloo-default))))
   '(tooltip                       ((t (:inherit igloo-subtle))))
   '(read-multiple-choice-face     ((t (:inherit igloo-strong))))
   '(nobreak-hyphen                ((t (:inherit igloo-popout))))
   '(nobreak-space                 ((t (:inherit igloo-popout))))
   '(help-argument-name            ((t (:inherit igloo-faded))))
   '(tabulated-list-fake-header    ((t (:inherit igloo-strong))))
   '(tool-bar                      ((t (:inherit igloo-faded-i))))

   ;; Line numbers -------------------------------------------------------------
   '(line-number                  ((t (:inherit igloo-faded))))
   '(line-number-current-line     ((t (:inherit default))))
   `(line-number-major-tick       ((t (:inherit igloo-faded))))
   '(line-number-minor-tick       ((t (:inherit igloo-faded))))

   ;; Font lock / Syntax highlighting ------------------------------------------
   '(font-lock-comment-face        ((t (:inherit igloo-faded))))
   '(font-lock-doc-face            ((t (:inherit igloo-faded))))
   '(font-lock-string-face         ((t (:inherit igloo-string))))
   '(font-lock-constant-face       ((t (:inherit igloo-salient))))
   '(font-lock-warning-face        ((t (:inherit igloo-popout))))
   '(font-lock-function-name-face  ((t (:inherit igloo-strong))))
   '(font-lock-variable-name-face  ((t (:inherit igloo-strong igloo-salient))))
   '(font-lock-builtin-face        ((t (:inherit igloo-salient))))
   '(font-lock-type-face           ((t (:inherit igloo-salient))))
   '(font-lock-keyword-face        ((t (:inherit igloo-salient))))

   ;; Edit ---------------------------------------------------------------------
   '(widget-field                  ((t (:inherit igloo-subtle))))
   '(widget-button                 ((t (:inherit igloo-strong))))
   '(widget-single-line-field      ((t (:inherit igloo-subtle))))
   '(custom-group-subtitle         ((t (:inherit igloo-strong))))
   '(custom-group-tag              ((t (:inherit igloo-strong))))
   '(custom-group-tag-1            ((t (:inherit igloo-strong))))
   '(custom-comment                ((t (:inherit igloo-faded))))
   '(custom-comment-tag            ((t (:inherit igloo-faded))))
   '(custom-changed                ((t (:inherit igloo-salient))))
   '(custom-modified               ((t (:inherit igloo-salient))))
   '(custom-face-tag               ((t (:inherit igloo-strong))))
   '(custom-variable-tag           ((t (:inherit igloo-strong))))
   '(custom-invalid                ((t (:inherit igloo-popout))))
   '(custom-visibility             ((t (:inherit igloo-salient))))
   '(custom-state                  ((t (:inherit igloo-salient))))
   '(custom-link                   ((t (:inherit igloo-salient))))
   '(custom-variable-obsolete      ((t (:inherit igloo-faded))))
   

   ;; Company ------------------------------------------------------------------
   '(company-tooltip                      ((t (:inherit igloo-subtle))))
    '(company-tooltip-mouse                ((t (:inherit igloo-faded-i))))
    '(company-tooltip-selection            ((t (:inherit igloo-salient-i))))

    '(company-scrollbar-fg                 ((t (:inherit igloo-default-i))))
    '(company-scrollbar-bg                 ((t (:inherit igloo-faded-i))))

    '(company-tooltip-common               ((t (:inherit igloo-strong))))
    '(company-tooltip-common-selection     ((t (:inherit igloo-salient-i
                                                :weight normal))))
    '(company-tooltip-annotation           ((t (:inherit igloo-default))))
    '(company-tooltip-annotation-selection ((t (:inherit igloo-subtle))))

   ;; Diff ---------------------------------------------------------------------
   '(diff-header                    ((t (:inherit igloo-faded))))
   '(diff-file-header               ((t (:inherit igloo-strong))))
   '(diff-context                   ((t (:inherit igloo-default))))
   '(diff-removed                   ((t (:inherit igloo-faded))))
   '(diff-changed                   ((t (:inherit igloo-popout))))
   '(diff-added                     ((t (:inherit igloo-salient))))
   '(diff-refine-added              ((t (:inherit (igloo-salient
                                                   igloo-strong)))))
   '(diff-refine-changed            ((t (:inherit igloo-popout))))
   '(diff-refine-removed            ((t (:inherit igloo-faded
                                         :strike-through t))))


   ;; Org agenda ---------------------------------------------------------------
   '(org-agenda-calendar-event      ((t (:inherit igloo-default))))
   '(org-agenda-calendar-sexp       ((t (:inherit igloo-salient))))
   '(org-agenda-clocking            ((t (:inherit igloo-faded))))
   '(org-agenda-column-dateline     ((t (:inherit igloo-faded))))
   '(org-agenda-current-time        ((t (:inherit igloo-strong))))
   '(org-agenda-date                ((t (:inherit igloo-salient))))
   '(org-agenda-date-today          ((t (:inherit (igloo-salient
                                                   igloo-strong)))))
   '(org-agenda-date-weekend        ((t (:inherit igloo-faded))))
   '(org-agenda-diary               ((t (:inherit igloo-faded))))
   '(org-agenda-dimmed-todo-face    ((t (:inherit igloo-faded))))
   '(org-agenda-done                ((t (:inherit igloo-faded))))
   '(org-agenda-filter-category     ((t (:inherit igloo-faded))))
   '(org-agenda-filter-effort       ((t (:inherit igloo-faded))))
   '(org-agenda-filter-regexp       ((t (:inherit igloo-faded))))
   '(org-agenda-filter-tags         ((t (:inherit igloo-faded))))
   '(org-agenda-property-face       ((t (:inherit igloo-faded))))
   '(org-agenda-restriction-lock    ((t (:inherit igloo-faded))))
   '(org-agenda-structure           ((t (:inherit igloo-strong))))


   ;; Org ----------------------------------------------------------------------
   '(org-archived                            ((t (:inherit igloo-faded))))
   '(org-block                               ((t (:inherit igloo-subtle))))
   '(org-block-begin-line                    ((t (:inherit igloo-faded))))
   '(org-block-end-line                      ((t (:inherit igloo-faded))))
   '(org-checkbox                            ((t (:inherit igloo-faded))))
   '(org-checkbox-statistics-done            ((t (:inherit igloo-faded))))
   '(org-checkbox-statistics-todo            ((t (:inherit igloo-faded))))
   '(org-clock-overlay                       ((t (:inherit igloo-faded))))
   '(org-code                                ((t (:inherit igloo-faded))))
   '(org-column                              ((t (:inherit igloo-faded))))
   '(org-column-title                        ((t (:inherit igloo-faded))))
   '(org-date                                ((t (:inherit igloo-faded))))
   '(org-date-selected                       ((t (:inherit igloo-faded))))
   '(org-default                             ((t (:inherit igloo-faded))))
   '(org-document-info                       ((t (:inherit igloo-faded))))
   '(org-document-info-keyword               ((t (:inherit igloo-faded))))
   '(org-document-title                      ((t (:inherit igloo-faded))))
   '(org-done                                ((t (:inherit igloo-faded))))
   '(org-drawer                              ((t (:inherit igloo-faded))))
   '(org-ellipsis                            ((t (:inherit igloo-faded))))
   '(org-footnote                            ((t (:inherit igloo-faded))))
   '(org-formula                             ((t (:inherit igloo-faded))))
   '(org-headline-done                       ((t (:inherit igloo-faded))))
   '(org-latex-and-related                   ((t (:inherit igloo-faded))))
   '(org-level-1                             ((t (:inherit igloo-strong))))
   '(org-level-2                             ((t (:inherit igloo-strong))))
   '(org-level-3                             ((t (:inherit igloo-strong))))
   '(org-level-4                             ((t (:inherit igloo-strong))))
   '(org-level-5                             ((t (:inherit igloo-strong))))
   '(org-level-6                             ((t (:inherit igloo-strong))))
   '(org-level-7                             ((t (:inherit igloo-strong))))
   '(org-level-8                             ((t (:inherit igloo-strong))))
   '(org-link                                ((t (:inherit igloo-salient))))
   '(org-list-dt                             ((t (:inherit igloo-faded))))
   '(org-macro                               ((t (:inherit igloo-faded))))
   '(org-meta-line                           ((t (:inherit igloo-faded))))
   '(org-mode-line-clock                     ((t (:inherit igloo-faded))))
   '(org-mode-line-clock-overrun             ((t (:inherit igloo-faded))))
   '(org-priority                            ((t (:inherit igloo-faded))))
   '(org-property-value                      ((t (:inherit igloo-faded))))
   '(org-quote                               ((t (:inherit igloo-faded))))
   '(org-scheduled                           ((t (:inherit igloo-faded))))
   '(org-scheduled-previously                ((t (:inherit igloo-faded))))
   '(org-scheduled-today                     ((t (:inherit igloo-faded))))
   '(org-sexp-date                           ((t (:inherit igloo-faded))))
   '(org-special-keyword                     ((t (:inherit igloo-faded))))
   '(org-table                               ((t (:inherit igloo-faded))))
   '(org-tag                                 ((t (:inherit igloo-popout))))
   '(org-tag-group                           ((t (:inherit igloo-faded))))
   '(org-target                              ((t (:inherit igloo-faded))))
   '(org-time-grid                           ((t (:inherit igloo-faded))))
   '(org-todo                                ((t (:inherit igloo-salient))))
   '(org-upcoming-deadline                   ((t (:inherit igloo-popout))))
   '(org-verbatim                            ((t (:inherit igloo-popout))))
   '(org-verse                               ((t (:inherit igloo-faded))))
   '(org-warning                             ((t (:inherit igloo-popout))))

   ;; Magit --------------------------------------------------------------------
   '(magit-blame-highlight                  ((t (:inherit (highlight)))))
    '(magit-diff-added-highlight             ((t (:inherit (highlight igloo-salient igloo-strong)))))
    '(magit-diff-base-highlight              ((t (:inherit (highlight)))))
    '(magit-diff-context-highlight           ((t (:inherit (highlight igloo-faded)))))
    '(magit-diff-file-heading-highlight      ((t (:inherit (highlight igloo-strong)))))
    '(magit-diff-hunk-heading-highlight      ((t (:inherit (igloo-default)))))
    '(magit-diff-our-highlight               ((t (:inherit (highlight)))))
    '(magit-diff-removed-highlight           ((t (:inherit (highlight igloo-popout igloo-strong)))))
    '(magit-diff-revision-summary-highlight  ((t (:inherit ()))))
    '(magit-diff-their-highlight             ((t (:inherit (highlight)))))
    '(magit-section-highlight                ((t (:inherit (highlight)))))

    '(magit-blame-heading                    ((t (:inherit (igloo-subtle igloo-strong)))))
    '(magit-diff-conflict-heading            ((t (:inherit (igloo-subtle igloo-strong)))))
    '(magit-diff-file-heading                ((t (:inherit (igloo-strong)))))
    '(magit-diff-hunk-heading                ((t (:inherit (igloo-subtle igloo-default)))))
    '(magit-diff-lines-heading               ((t (:inherit (igloo-subtle igloo-strong)))))
    '(magit-section-heading                  ((t (:inherit (igloo-salient igloo-strong)))))

    '(magit-bisect-bad                       ((t (:inherit igloo-default))))
    '(magit-bisect-good                      ((t (:inherit igloo-default))))
    '(magit-bisect-skip                      ((t (:inherit igloo-default))))
    '(magit-blame-date                       ((t (:inherit igloo-default))))
    '(magit-blame-dimmed                     ((t (:inherit igloo-default))))
    '(magit-blame-hash                       ((t (:inherit igloo-faded))))

    '(magit-blame-margin                     ((t (:inherit igloo-default))))
    '(magit-blame-name                       ((t (:inherit igloo-default))))
    '(magit-blame-summary                    ((t (:inherit igloo-default))))

    '(magit-branch-current                   ((t (:inherit (igloo-strong igloo-salient)))))
    '(magit-branch-local                     ((t (:inherit igloo-salient))))
    '(magit-branch-remote                    ((t (:inherit (igloo-salient)))))
    '(magit-branch-remote-head               ((t (:inherit (igloo-salient)))))
    '(magit-branch-upstream                  ((t (:inherit (igloo-salient)))))

    '(magit-cherry-equivalent                ((t (:inherit igloo-default))))
    '(magit-cherry-unmatched                 ((t (:inherit igloo-default))))

    '(magit-diff-added                       ((t (:inherit (highlight igloo-salient igloo-strong)))))
    '(magit-diff-base                        ((t (:inherit igloo-default))))
    '(magit-diff-context                     ((t (:inherit (highlight igloo-faded)))))
    '(magit-diff-file-heading-selection      ((t (:inherit igloo-default))))
    '(magit-diff-hunk-heading-selection      ((t (:inherit igloo-default))))
    '(magit-diff-hunk-region                 ((t (:inherit igloo-default))))
    '(magit-diff-lines-boundary              ((t (:inherit igloo-default))))
    '(magit-diff-our                         ((t (:inherit igloo-default))))
    '(magit-diff-removed                     ((t (:inherit (highlight igloo-popout igloo-strong)))))
    '(magit-diff-revision-summary            ((t (:inherit igloo-popout))))
    '(magit-diff-their                       ((t (:inherit igloo-default))))
    '(magit-diff-whitespace-warning          ((t (:inherit igloo-default))))
    '(magit-diffstat-added                   ((t (:inherit igloo-default))))
    '(magit-diffstat-removed                 ((t (:inherit igloo-default))))

    '(magit-dimmed                           ((t (:inherit igloo-faded))))
    '(magit-filename                         ((t (:inherit igloo-default))))
    '(magit-hash                             ((t (:inherit igloo-faded))))
    '(magit-head                             ((t (:inherit igloo-default))))
    '(magit-header-line                      ((t (:inherit igloo-default))))
    '(magit-header-line-key                  ((t (:inherit igloo-default))))
    '(magit-header-line-log-select           ((t (:inherit igloo-default))))

    '(magit-keyword                          ((t (:inherit igloo-default))))
    '(magit-keyword-squash                   ((t (:inherit igloo-default))))

    '(magit-log-author                       ((t (:inherit igloo-default))))
    '(magit-log-date                         ((t (:inherit igloo-default))))
    '(magit-log-graph                        ((t (:inherit igloo-default))))

    '(magit-mode-line-process                ((t (:inherit igloo-default))))
    '(magit-mode-line-process-error          ((t (:inherit igloo-default))))

    '(magit-process-ng                       ((t (:inherit igloo-default))))
    '(magit-process-ok                       ((t (:inherit igloo-default))))

    '(magit-reflog-amend                     ((t (:inherit igloo-default))))
    '(magit-reflog-checkout                  ((t (:inherit igloo-default))))
    '(magit-reflog-cherry-pick               ((t (:inherit igloo-default))))
    '(magit-reflog-commit                    ((t (:inherit igloo-default))))
    '(magit-reflog-merge                     ((t (:inherit igloo-default))))
    '(magit-reflog-other                     ((t (:inherit igloo-default))))
    '(magit-reflog-rebase                    ((t (:inherit igloo-default))))
    '(magit-reflog-remote                    ((t (:inherit igloo-default))))
    '(magit-reflog-reset                     ((t (:inherit igloo-default))))
    '(magit-refname                          ((t (:inherit igloo-default))))
    '(magit-refname-pullreq                  ((t (:inherit igloo-default))))
    '(magit-refname-stash                    ((t (:inherit igloo-default))))
    '(magit-refname-wip                      ((t (:inherit igloo-default))))

    '(magit-section-heading-selection        ((t (:inherit igloo-default))))
    '(magit-section-secondary-heading        ((t (:inherit igloo-default))))
    '(magit-sequence-done                    ((t (:inherit igloo-default))))
    '(magit-sequence-drop                    ((t (:inherit igloo-default))))
    '(magit-sequence-exec                    ((t (:inherit igloo-default))))
    '(magit-sequence-head                    ((t (:inherit igloo-default))))
    '(magit-sequence-onto                    ((t (:inherit igloo-default))))
    '(magit-sequence-part                    ((t (:inherit igloo-default))))
    '(magit-sequence-pick                    ((t (:inherit igloo-default))))
    '(magit-sequence-stop                    ((t (:inherit igloo-default))))

    '(magit-signature-bad                    ((t (:inherit igloo-default))))
    '(magit-signature-error                  ((t (:inherit igloo-default))))
    '(magit-signature-expired                ((t (:inherit igloo-default))))
    '(magit-signature-expired-key            ((t (:inherit igloo-default))))
    '(magit-signature-good                   ((t (:inherit igloo-default))))
    '(magit-signature-revoked                ((t (:inherit igloo-default))))
    '(magit-signature-untrusted              ((t (:inherit igloo-default))))

    '(magit-tag                              ((t (:inherit igloo-default-i))))

    ;; Ledger ------------------------------------------------------------------
    '(ledger-font-xact-highlight-face        ((t (:inherit igloo-default))))
    '(ledger-font-report-clickable-face      ((t (:inherit igloo-default))))

    ;; Term / VTerm ------------------------------------------------------------
    '(term-bold                              ((t (:inherit igloo-strong))))
    '(term-color-white                       ((t (:inherit igloo-default))))
    `(term-color-blue                        ((,class (:foreground "#42A5F5"
                                                       :background "#BBDEFB"))))
    `(term-color-cyan                        ((,class (:foreground "#26C6DA"
                                                       :background "#B2EBF2"))))
    `(term-color-green                       ((,class (:foreground "#66BB6A"
                                                       :background "#C8E6C9"))))
    `(term-color-magenta                     ((,class (:foreground "#AB47BC"
                                                       :background "#E1BEE7"))))
    `(term-color-red                         ((,class (:foreground "#EF5350"
                                                       :background "#FFCDD2"))))
    `(term-color-yellow                      ((,class (:foreground "#FFEE58"
                                                       :background "#FFF9C4"))))

    '(vterm-color-white                      ((t (:inherit igloo-default))))

    ;; Eshell ------------------------------------------------------------------
    '(eshell-prompt                          ((t (:inherit igloo-salient))))

    ;; Hydra -------------------------------------------------------------------
    '(hydra-face-blue                        ((t (:inherit igloo-salient))))
    '(hydra-face-teal                        ((t (:inherit igloo-salient))))
    

    ;; Ivy ---------------------------------------------------------------------
    '(ivy-action                             ((t (:inherit igloo-faded))))
    '(ivy-completions-annotations            ((t (:inherit igloo-popout))))
    '(ivy-confirm-face                       ((t (:inherit igloo-faded))))
    '(ivy-current-match                      ((t (:inherit igloo-strong-hi))))
    '(ivy-cursor                             ((t (:inherit igloo-strong))))
    '(ivy-grep-info                          ((t (:inherit igloo-strong))))
    '(ivy-grep-line-number                   ((t (:inherit igloo-faded))))
    '(ivy-highlight-face                     ((t (:inherit igloo-strong))))
    '(ivy-match-required-face                ((t (:inherit igloo-faded))))
    '(ivy-minibuffer-match-face-1            ((t (:inherit igloo-faded))))
    '(ivy-minibuffer-match-face-2            ((t (:inherit igloo-faded))))
    '(ivy-minibuffer-match-face-3            ((t (:inherit igloo-faded))))
    '(ivy-minibuffer-match-face-4            ((t (:inherit igloo-faded))))
    '(ivy-minibuffer-match-highlight         ((t (:inherit igloo-strong))))
    '(ivy-modified-buffer                    ((t (:inherit igloo-popout))))
    '(ivy-modified-outside-buffer            ((t (:inherit igloo-strong))))
    '(ivy-org                                ((t (:inherit igloo-faded))))
    '(ivy-prompt-match                       ((t (:inherit igloo-faded))))
    '(ivy-remote                             ((t (:inherit igloo-default))))
    '(ivy-separator                          ((t (:inherit igloo-faded))))
    '(ivy-subdir                             ((t (:inherit igloo-faded))))
    '(ivy-virtual                            ((t (:inherit igloo-faded))))
    '(ivy-yanked-word                        ((t (:inherit igloo-faded))))))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'ig)

(provide 'ig-theme)

;;; ig-theme.el ends here
