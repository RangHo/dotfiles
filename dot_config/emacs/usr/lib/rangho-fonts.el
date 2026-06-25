;;; rangho-fonts.el --- Font settings for Emacs

;; Copyright (C) 2026 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "30.2"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Emacs has weird ways of implementing font lookup.
;; There are two different concepts:

;; * Face font
;;   They govern the fonts for ASCII characters.  They cannot be overriden by
;;   fontsets.

;; * Fontsets
;;   Set of fonts used to show Unicode characters.  While they are not
;;   documented, one can define a brand-new fontset with
;;   `create-fontset-from-fontset-spec'.

;;; Code:

(require 'faces)

(defgroup rangho-fonts nil
  "Font settings for Emacs."
  :group 'local
  :prefix "rangho-fonts-")

(defcustom rangho-fonts-fixed-font-alist
  '((nil "monospace")
    (hangul "monospace")
    (kana "monospace")
    (han "monospace"))
  "Association list of fixed-width fonts for different character sets."
  :type '(repeat (cons (choice (const :tag "Default" nil)
                               (const :tag "Character set" symbol))
                       (repeat string))))

(defcustom rangho-fonts-variable-font-alist
  '((nil "sans-serif")
    (hangul "sans-serif")
    (kana "sans-serif")
    (han "sans-serif"))
  "Association list of variable-width fonts for different character sets."
  :type '(repeat (cons (choice (const :tag "Default" nil)
                               (const :tag "Character set" symbol))
                       (repeat string))))

(defconst rangho-fonts--unicode-private-use-areas
  '((#xe000 . #xf8ff)      ; Private Use Area
    (#xf0000 . #xffffd)    ; Supplementary Private Use Area-A
    (#x100000 . #x10fffd)) ; Supplementary Private Use Area-B
  "List of Unicode Private Use Areas, defined as pairs of (START . END) code points.")

(defun rangho-fonts--font-available-p (font)
  "Check if FONT is available."
  (find-font (font-spec :name font)))

(defun rangho-fonts--find-font-specs (alist charset)
  "Find a list of font specs for CHARSET in ALIST."
  (when-let* ((candidates (alist-get charset alist))
              (found-fonts (cl-loop
                            for font in candidates
                            when (rangho-fonts--font-available-p font)
                            return (if (listp font) font (list font)))))
    (mapcar (apply-partially #'font-spec :name) found-fonts)))

(defun rangho-fonts--find-first-available-font (alist charset)
  "Find the first available font for CHARSET in ALIST."
  (when-let ((candidates (alist-get charset alist)))
    (cl-find-if #'rangho-fonts--font-available-p candidates)))

(defun rangho-fonts--set-fontset-fonts (fontset alist)
  "Set fonts in FONTSET using ALIST."
  (dolist (item alist)
    (dolist (font (rangho-fonts--find-font-specs alist (car item)))
      (set-fontset-font fontset (car item) font)))
  (dolist (area rangho-fonts--unicode-private-use-areas)
    (set-fontset-font fontset area (car (rangho-fonts--find-font-specs alist 'symbol)))))

(defun rangho-fonts-set-default-font ()
  "Set the default font to use throughout Emacs."
  (interactive)
  (let ((fixed-font (rangho-fonts--find-first-available-font rangho-fonts-fixed-font-alist nil))
        (variable-font (rangho-fonts--find-first-available-font rangho-fonts-variable-font-alist nil)))
    ;; Default fontset.
    (rangho-fonts--set-fontset-fonts t rangho-fonts-fixed-font-alist)
    ;; Fixed-pitch fontset.
    (create-fontset-from-fontset-spec
     (font-xlfd-name
      (font-spec :name fixed-font
                 :registry "fontset-fixed")))
    (rangho-fonts--set-fontset-fonts "fontset-fixed" rangho-fonts-fixed-font-alist)
    ;; Variable-pitch fontset.
    (create-fontset-from-fontset-spec
     (font-xlfd-name
      (font-spec :name variable-font
                 :registry "fontset-variable")))
    (rangho-fonts--set-fontset-fonts "fontset-variable" rangho-fonts-variable-font-alist)
    ;; Frame-wide face attributes.
    (set-face-attribute 'default nil
                        :family fixed-font
                        :height 120)
    (set-face-attribute 'fixed-pitch nil
                        :family fixed-font
                        :fontset "fontset-fixed"
                        :inherit t)
    (set-face-attribute 'variable-pitch nil
                        :family variable-font
                        :fontset "fontset-variable"
                        :inherit t)))

(defun rangho-fonts--set-default-font-for-frame (frame)
  "Set the default font for FRAME."
  (with-selected-frame frame
    (rangho-fonts-set-default-font)))

(defun rangho-fonts--enable ()
  "Enable the font settings for the current frame."
  (cond
   ;;; If we're running in a daemon, we need to set up the fonts for each new frame.
   ((daemonp)
    (add-hook 'after-make-frame-functions #'rangho-fonts--set-default-font-for-frame))
   ((display-graphic-p)
    (rangho-fonts-set-default-font))
   (t nil)))

(defun rangho-fonts-unset-default-font ()
  "Unset the default font settings."
  (interactive)
  (set-face-attribute 'default nil :family unspecified :height unspecified)
  (set-face-attribute 'fixed-pitch nil :family unspecified :fontset unspecified)
  (set-face-attribute 'variable-pitch nil :family unspecified :fontset unspecified))

(defun rangho-fonts--disable ()
  "Disable the font settings."
  (cond
   ((daemonp)
    (remove-hook 'after-make-frame-functions #'rangho-fonts--set-default-font-for-frame))
   ((display-graphic-p)
    (rangho-fonts-unset-default-font))
   (t nil)))

;;;###autoload
(define-minor-mode rangho-fonts-mode
  "Minor mode to set up fonts for Emacs."
  :global t
  (if rangho-fonts-mode
      (rangho-fonts--enable)
    (rangho-fonts--disable)))

(provide 'rangho-fonts)
;;; rangho-fonts.el ends here
