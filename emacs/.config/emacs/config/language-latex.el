;;; language-latex.el --- Language support for LaTeX.

;;; Commentary:

;; This package provides a major mode for editing LaTeX source files.

;;; Code:

;; Install AUCTeX
(use-package auctex
  :hook (LaTeX-mode . prettify-symbols-mode))

;; Add LaTeX faces that need that should be left as monospace
(setq rangho/serif-font-ignore-face-list
      `(,@rangho/serif-font-ignore-face-list
        font-latex-math-face
        font-latex-sedate-face
        font-latex-warning-face))

(provide 'language-latex)

;;; language-latex.el ends here
