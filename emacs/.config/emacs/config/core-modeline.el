;;; core-modeline.el --- My personal custom modeline.

;; ============
;; Dependencies
;; ============

;; all-the-icons *should* be installed by now
;; unless Emacs is running in a terminal, of course
(use-package all-the-icons
  :if (or (daemonp) (display-graphic-p))
  :ensure t)

;; Nyanyanyanyanyanyanya!
(use-package nyan-mode
  :init
  (setq nyan-bar-length 22)
  (setq nyan-minimum-window-width 100)
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t))


;; =======
;; Helpers
;; =======

(defvar rangho/selected-window (frame-selected-window)
  "Currently selected window.")

(defun rangho/selected-window-active-p (&optional target)
  "Check if the selected window is active."
  (eq rangho/selected-window (or target
                                 (selected-window))))

(defun rangho/selected-window-graphic-p (&optional target)
  "Check if the selected window's frame is graphical."
  (display-graphic-p (window-frame (or target
                                       (selected-window)))))

(defun rangho/set-selected-window (&rest _)
  "Function to call when the selected window changes."
  (unless (minibuffer-window-active-p (frame-selected-window))
    (setq rangho/selected-window (frame-selected-window))))

;; Automatically update the selected window
(add-hook 'pre-redisplay-functions #'rangho/set-selected-window)


;; ===================
;; Modeline components
;; ===================

(defun rangho/faicon (icon-name)
  "Grab a FontAwesome icon using the `icon-name' string, with the appropriate centering offset."
  (if (rangho/selected-window-graphic-p)
      (all-the-icons-faicon icon-name :height 1.0 :v-adjust 0.0)
    " "))

(defconst rangho/buffer-status-alist
  `(("*" "circle" ; edited, yet to be saved
     :background "#f0c674" :foreground "#1d1f21")
    ("-" "check-circle" ; saved
     :background "#b5bd68" :foreground "#1d1f21")
    ("%" "times-circle" ; read-only
     :background "#cc6666" :foreground "#1d1f21"))
  "List of icons and faces to indicate the current status of the buffer")

(defun rangho/mode-line-buffer-status ()
  "Modeline component that indicates the current status of buffer."
  (let* ((current-status (assoc (format-mode-line "%*")
                                rangho/buffer-status-alist))
         (status-icon (rangho/faicon (cadr current-status)))
         (status-prop (cddr current-status)))
    (funcall 'propertize
             (concat " " status-icon " ")
             'face (append status-prop
                           `(:box (:color ,(plist-get status-prop :background)))))))

(defun rangho/mode-line-position ()
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

(defconst rangho/evil-status-alist
  '((emacs "EMACS"
           :background "#d3d0c8" :foreground "#737373")
    (normal "NORMAL"
            :background "#99cc99" :foreground "#394d39")
    (insert "INSERT"
            :background "#6699cc" :foreground "#26394d")
    (replace "REPLACE"
             :background "#f2777a" :foreground "#733939")
    (visual "VISUAL"
            :background "#ffcc66" :foreground "#806330"))
  "List of texts and colors that represent the current evil status.")

(defun rangho/mode-line-evil-status ()
  "Modeline component that displays the current status of Evil mode."
  (when (rangho/selected-window-active-p (selected-window))
    (let* ((current-status (assq evil-state
                                 rangho/evil-status-alist))
           (status-text (cadr current-status))
           (status-prop (cddr current-status)))
      (funcall 'propertize
               (concat " " status-text " ")
               'face (append status-prop
                             `(:weight bold :box (:color ,(plist-get status-prop :background))))))))

(defvar rangho/current-buffer-project
  (project-current)
  "Name of the current project, updated whenever active buffer changes.")

(defun rangho/update-buffer-project (_)
  "Update the current buffer's project name."
  (setq rangho/current-buffer-project (project-current)))

(add-hook 'window-buffer-change-functions #'rangho/update-buffer-project)

(defun rangho/mode-line-buffer-description ()
  "Modeline component that shows what file is being edited."
  ;; Basically,it displays the current information in the following form:
  ;;   (Editing|Viewing) <filename> [in <project>] [on <branch>] [using <major mode>]
  (concat
   ;; "Editing" if rw, "Viewing" if ro
   (if buffer-read-only
       "Viewing"
     "Editing")

   ;; Separator
   " "

   ;; Show the file/buffer name with appropriate icons
   (when (rangho/selected-window-graphic-p)
     (concat
      (all-the-icons-icon-for-file (buffer-name) :height 0.90 :v-adjust 0.0)
      " "))
   (buffer-name)

   ;; If project is available show that as well
   (when (and (buffer-file-name) rangho/current-buffer-project)
     (concat " in "
             (when (rangho/selected-window-graphic-p)
               (concat
                (all-the-icons-octicon "repo" :height 0.90 :v-adjust 0.0)
                " "))
             (file-name-nondirectory (directory-file-name (cdr rangho/current-buffer-project)))))

   ;; Show the branch name, if available
   (when (and (buffer-file-name) (vc-git--symbolic-ref (buffer-file-name)))
     (concat " on "
             (when (rangho/selected-window-graphic-p)
               (concat
                (all-the-icons-octicon "git-branch" :height 0.90 :v-adjust 0.0)
                " "))
             (vc-git--symbolic-ref (buffer-file-name))))

   ;; Show the current major mode
   (concat " using "
           (when (and (rangho/selected-window-graphic-p)
                      (assoc major-mode all-the-icons-mode-icon-alist))
             (concat
              (all-the-icons-icon-for-mode major-mode :height 0.90 :v-adjust 0.0)
              " "))
           (if (listp mode-name)
               (car mode-name)
             mode-name))))

(defun rangho/mode-line-line-column ()
  "Modeline component that displays the current line and column number."
  (propertize " %4l:%2c "
              'face '(:background "#202124" :foreground "#e8eaed" :box (:color "#202124"))))

(defun rangho/mode-line-space ()
  "Modeline component that shows a space."
  " ")

(defun rangho/mode-line-misc-info ()
  "Modeline component that shows miscellaneous information."
  (format-mode-line mode-line-misc-info))


;; ===================
;; Modeline definition
;; ===================

(defvar rangho/mode-line-left
  (list
   #'rangho/mode-line-buffer-status
   #'rangho/mode-line-position
   #'rangho/mode-line-space
   #'rangho/mode-line-buffer-description)
  "Components that are positioned on the left side of mode line.")

(defvar rangho/mode-line-right
  (list
   #'rangho/mode-line-misc-info
   #'rangho/mode-line-line-column
   #'rangho/mode-line-evil-status)
  "Components that are positioned on the right side of mode line.")

(require 'seq)
(defun rangho/render-mode-line (lhs rhs)
  "Render the mode line."
  (let* ((lhs-rendered (seq-reduce (lambda (acc fun)
                                     (concat acc (funcall fun)))
                                   lhs ""))
         (lhs-length (length lhs-rendered))
         (rhs-rendered (seq-reduce (lambda (acc fun)
                                     (concat acc (funcall fun)))
                                   rhs ""))
         (rhs-length (length rhs-rendered)))
    (if (> (+ lhs-length rhs-length) (window-total-width))
        (progn (setq lhs-rendered (truncate-string-to-width lhs-rendered
                                                            (- (window-total-width) rhs-length 5)
                                                            nil
                                                            nil
                                                            "..."))
               (setq lhs-length (length lhs-rendered))))
    (concat lhs-rendered
            (propertize " "
                        'display `((space :align-to (- right ,rhs-length))))
            rhs-rendered)))

(setq-default mode-line-format
              '(:eval (rangho/render-mode-line rangho/mode-line-left
                                               rangho/mode-line-right)))

(provide 'core-modeline)
