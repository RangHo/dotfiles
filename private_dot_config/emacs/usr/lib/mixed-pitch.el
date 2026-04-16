;;; mixed-pitch.el --- Mixed pitch support with CJK characters

;; Copyright (C) 2023--2024 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "24.4"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; `mixed-pitch-mode' is a minor mode that lets mix-and-matching variable-pitch
;; and fixed-pitch fonts in a single buffer.  Before using this, you need to
;; have two fontsets defined: one named `variable-pitch' and the other named
;; `fixed-pitch'.

;;; Code:

(require 'face-remap)

(defconst mixed-pitch-fixed-pitch-faces
  '(diff-added
    diff-context
    diff-file-header
    diff-function
    diff-header
    diff-hunk-header
    diff-removed
    font-latex-math-face
    font-latex-sedate-face
    font-latex-warning-face
    font-latex-sectioning-5-face
    font-lock-builtin-face
    font-lock-comment-face
    font-lock-comment-delimiter-face
    font-lock-constant-face
    font-lock-doc-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-preprocessor-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    font-lock-string-face
    font-lock-type-face
    font-lock-variable-name-face
    line-number
    line-number-current-line
    line-number-major-tick
    line-number-minor-tick
    markdown-code-face
    markdown-gfm-checkbox-face
    markdown-inline-code-face
    markdown-language-info-face
    markdown-language-keyword-face
    markdown-math-face
    message-header-name
    message-header-to
    message-header-cc
    message-header-newsgroups
    message-header-xheader
    message-header-subject
    message-header-other
    mu4e-header-key-face
    mu4e-header-value-face
    mu4e-link-face
    mu4e-contact-face
    mu4e-compose-separator-face
    mu4e-compose-header-face
    org-block
    org-block-begin-line
    org-block-end-line
    org-document-info-keyword
    org-code
    org-indent
    org-latex-and-related
    org-checkbox
    org-formula
    org-meta-line
    org-table
    org-verbatim)
  "Faces that should remain as fixed-pitch.")

(defvar mixed-pitch-fixed-pitch-cookies
  nil
  "Face remapping cookies for fixed-pitch faces.")

(defun mixed-pitch--enable ()
  "Enable mixed-pitch mode for the current buffer."
  (make-local-variable 'mixed-pitch-fixed-pitch-cookies)
  (setq mixed-pitch-fixed-pitch-cookies nil)
  (buffer-face-set 'variable-pitch)
  (dolist (face mixed-pitch-fixed-pitch-faces)
    (add-to-list 'mixed-pitch-fixed-pitch-cookies
		 (face-remap-add-relative face :inherit 'fixed-pitch))))

(defun mixed-pitch--disable ()
  "Disable mixed-pitch mode for the current buffer."
  (buffer-face-set)
  (dolist (cookie mixed-pitch-fixed-pitch-cookies)
    (face-remap-remove-relative cookie)))

;;;###autoload
(define-minor-mode mixed-pitch-mode
  "Minor mode for mix-and-matching variable and fixed pitch fonts."
  :init-value nil
  (if mixed-pitch-mode
      (mixed-pitch--enable)
    (mixed-pitch--disable)))

(provide 'mixed-pitch)

;;; mixed-pitch.el ends here
