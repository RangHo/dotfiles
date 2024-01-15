;;; utility-mixed-pitch.el --- Mixed pitch support with CJK characters

;;; Commentary:

;;

;;; Code:

(defconst rangho/mixed-pitch-fixed-pitch-faces
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
  "This is a list holding names of faces that will not be variable pitch when function `mixed-pitch-mode' is enabled.")

(define-minor-mode mixed-pitch-mode
  "Minor mode for mix-and-matching variable and fixed pitch fonts."
  :init-value nil
  (if mixed-pitch-mode
      (progn
        (make-local-variable 'rangho/mixed-pitch-fixed-pitch-cookies)
        (setq rangho/mixed-pitch-fixed-pitch-cookies nil)
        (buffer-face-set 'variable-pitch)
        (dolist (face rangho/mixed-pitch-fixed-pitch-faces)
          (add-to-list 'rangho/mixed-pitch-fixed-pitch-cookies
                       (face-remap-add-relative face :inherit 'fixed-pitch))))
    (buffer-face-set)
    (dolist (cookie rangho/mixed-pitch-fixed-pitch-cookies)
      (face-remap-remove-relative cookie))))

(add-hook 'text-mode-hook #'mixed-pitch-mode)

(provide 'utility-mixed-pitch)

;;; utility-mixed-pitch.el ends here
