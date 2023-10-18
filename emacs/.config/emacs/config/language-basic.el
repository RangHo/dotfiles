;;; language-basic.el --- Language support for BASIC and its dialects

;;; Commentary:

;; This file also provides dialect definition for Visual Basic family.

;;; Code:

(use-package basic-mode
  :mode (("\\.vb\\'" . visual-basic-mode)
         ("\\.vba\\'" . visual-basic-mode)
         ("\\.vbs\\'" . visual-basic-mode))
  :init
  (define-derived-mode visual-basic-mode basic-qb45-mode "Visual Basic"
    "Major mode for editing Visual Basic code."
    (basic-mode-initialize)))

(provide 'language-basic)

;;; language-basic.el ends here
