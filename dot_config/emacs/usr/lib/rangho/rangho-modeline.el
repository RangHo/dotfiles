;;; rangho-modeline.el --- A custom modeline for Emacs

;; Copyright (C) 2026 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "26.1") (evil "1.14.2") (nerd-icons "0.1.0") (nyan-mode "1.1.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; `rangho-modeline-mode' is a global minor mode that replaces the default
;; Emacs modeline with a custom one that displays various information about the
;; current buffer and its context.  It is designed to be visually appealing and
;; informative, while also being customizable and extensible.

;; The basic design is inspired by the Powerline Vim statusline plugin.

;;; Code:

(require 'evil)
(require 'nerd-icons)
(require 'nyan-mode)
(require 'project)

(defgroup rangho-modeline nil
  "Custom modeline for Emacs."
  :group 'local)

(defcustom rangho-modeline-lhs-components
  nil
  "List of functions that generate the left-hand side components of the modeline."
  :type '(repeat function))

(defcustom rangho-modeline-rhs-components
  nil
  "List of functions that generate the right-hand side components of the modeline."
  :type '(repeat function))


(defvar rangho-modeline--selected-window
  (frame-selected-window)
  "Currently selected window.")

(defun rangho-modeline--selected-window-active-p (&optional target)
  "Check if TARGET window is active."
  (eq rangho-modeline--selected-window (or target (selected-window))))

(defun rangho-modeline--selected-window-graphic-p (&optional target)
  "Check if TARGET window's frame is graphical."
  (display-graphic-p (window-frame (or target (selected-window)))))

(defun rangho-modeline--set-selected-window (&rest _)
  "Update the selected window cache to a new one."
  (unless (minibuffer-window-active-p (frame-selected-window))
    (setq rangho-modeline--selected-window (frame-selected-window))))

(defvar rangho-modeline--current-buffer-project
  (project-current)
  "Name of the current project, updated whenever active buffer changes.")

(defun rangho-modeline--update-buffer-project (_)
  "Update the current buffer's project name."
  (setq rangho-modeline--current-buffer-project (project-current)))

(defconst rangho-modeline--buffer-status-alist
  `(("*" ; edited, yet to be saved
     ,(nerd-icons-faicon "nf-fa-plus_circle" :height 0.90 :v-adjust 0.10)
     :foreground "#19150c" :background "#f0c674")
    ("-" ; saved
     ,(nerd-icons-faicon "nf-fa-check_circle" :height 0.90 :v-adjust 0.10)
     :foreground "#18190e" :background "#b5bd68")
    ("%" ; read-only
     ,(nerd-icons-faicon "nf-fa-times_circle" :height 0.90 :v-adjust 0.10)
     :foreground "#190c0c" :background "#cc6666"))
  "Alist of icons and faces to indicate the current status of the buffer.")

(defun rangho-modeline-buffer-status-component ()
  "Modeline component that indicates the current status of buffer."
  (let* ((current-status (assoc (format-mode-line "%*")
                                rangho-modeline--buffer-status-alist))
         (status-icon (cadr current-status))
         (status-prop (cddr current-status))
         (status-fore (plist-get status-prop :foreground))
         (status-back (plist-get status-prop :background))
         (status-str (concat " " status-icon " ")))
    (add-face-text-property 0
                            (length status-str)
                            `(:foreground ,status-fore
                                          :background ,status-back
                                          :box (:color ,status-back))
                            nil
                            status-str)
    status-str))

(defun rangho-modeline-scroll-bar-component ()
  "Modeline component that displays the current position in a file, but nyan cat!"
  (let* ((nyan-cat-string (concat " " (nyan-create) " "))
         (nyan-cat-length (length nyan-cat-string))
         (percent-string " %p"))
    (if (string= nyan-cat-string "  ")
        percent-string
      (add-face-text-property 0
                              nyan-cat-length
                              '(:background "#003163" :box (:color "#003163"))
                              nil
                              nyan-cat-string)
      nyan-cat-string)))

(defun rangho-modeline-buffer-description-component ()
  "Modeline component that shows what file is being edited.

   Basically, it displays the current information in the following form:

       (Editing|Viewing) <filename> [in <project] [on <branch>] [using <major-mode>]"
  (concat
   " "
   ;; "Editing" if rw, "Viewing" if ro
   (if buffer-read-only "Viewing" "Editing")
   " "
   ;; Show the file/buffer name with appropriate icons
   (nerd-icons-icon-for-file (buffer-name) :height 0.90 :v-adjust 0.0)
   " "
   (buffer-name)
   " "
   ;; If project is available show that as well
   (when (and (buffer-file-name) rangho-modeline--current-buffer-project)
     (concat
      "in "
      (nerd-icons-octicon "nf-oct-repo" :height 0.90 :v-adjust 0.0)
      " "
      (file-name-nondirectory (directory-file-name (project-root rangho-modeline--current-buffer-project)))
      " "))
   ;; Show the branch name, if available
   (when nil
     (concat
      "on "
      (nerd-icons-octicon "nf-oct-git_branch" :height 0.90 :v-adjust 0.0)
      " "
      branch-name
      " "))
   ;; Show the current major mode
   "using "
   (when-let (icon (assoc major-mode nerd-icons-mode-icon-alist))
     (concat
      (nerd-icons-icon-for-mode major-mode :height 0.90 :v-adjust 0.0)
      " "))
   (format-mode-line mode-name)
   " "))

(defun rangho-modeline-position-component ()
  "Modeline component that displays the current line and column number."
  (propertize " %4l:%2c "
              'face '(:background "#202124" :foreground "#e8eaed" :box (:color "#202124"))))

(defconst rangho-modeline--evil-state-alist
  '((emacs "EMACS" :background "#d3d0c8" :foreground "#737373")
    (normal "NORMAL" :background "#99cc99" :foreground "#394d39")
    (insert "INSERT" :background "#6699cc" :foreground "#26394d")
    (replace "REPLACE" :background "#f2777a" :foreground "#733939")
    (visual "VISUAL" :background "#ffcc66" :foreground "#806330")
    (hades "HADES" :background "#cc99cc" :foreground "#4d394d"))
  "List of texts and colors that represent the current evil status.")

(defun rangho-modeline-evil-state-component ()
  "Modeline component that displays the current state of Evil mode."
  (when (rangho-modeline--selected-window-active-p (selected-window))
    (let* ((current-state (assq evil-state
                                rangho-modeline--evil-state-alist))
           (state-text (cadr current-state))
           (state-prop (cddr current-state)))
      (funcall 'propertize
               (concat " " state-text " ")
               'face (append state-prop
                             `(:weight bold :box (:color ,(plist-get state-prop :background))))))))

(defun rangho-modeline--render-mode-line (lhs rhs)
  "Render the mode line with LHS and RHS components."
  (let* ((lhs-rendered (seq-reduce (lambda (acc fun)
                                     (concat acc (funcall fun)))
                                   lhs ""))
         (rhs-rendered (seq-reduce (lambda (acc fun)
                                     (concat acc (funcall fun)))
                                   rhs ""))
         (lhs-length (length (format-mode-line lhs-rendered)))
         (rhs-length (length (format-mode-line rhs-rendered))))
    (when (< (window-total-width) (+ lhs-length rhs-length))
      (setq lhs-rendered
            (truncate-string-to-width lhs-rendered
                                      (- (window-total-width) rhs-length)
                                      nil
                                      nil
                                      "..."))
      (setq lhs-length
            (length (format-mode-line lhs-rendered))))
    (concat lhs-rendered
            (propertize " "
                        'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,rhs-length))))
            rhs-rendered)))

(defvar rangho-modeline--previous-mode-line-format
  nil
  "Cache for the previous mode line format, used to restore when disabling the modeline.")

(defun rangho-modeline--enable ()
  "Enable the custom modeline."
  (setq rangho-modeline--previous-mode-line-format mode-line-format)
  (setq-default mode-line-format
        '(:eval (rangho-modeline--render-mode-line
                 rangho-modeline-lhs-components
                 rangho-modeline-rhs-components)))
  (add-to-list 'pre-redisplay-functions #'rangho-modeline--set-selected-window)
  (add-to-list 'window-buffer-change-functions #'rangho-modeline--update-buffer-project))

(defun rangho-modeline--disable ()
  "Disable the custom modeline and restore the previous one."
  (setq-default mode-line-format rangho-modeline--previous-mode-line-format)
  (setq rangho-modeline--previous-mode-line-format nil)
  (delete #'rangho-modeline--set-selected-window pre-redisplay-functions)
  (delete #'rangho-modeline--update-buffer-project window-buffer-change-functians)
  )

;;;###autoload
(define-minor-mode rangho-modeline-mode
  "Minor mode to use the custom modeline."
  :global t
  :init-value nil
  (if rangho-modeline-mode
      (rangho-modeline--enable)
    (rangho-modeline--disable)))

(provide 'rangho-modeline)
;;; rangho-modeline.el ends here
