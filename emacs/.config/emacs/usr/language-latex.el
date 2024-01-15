;;; language-latex.el --- Language support for LaTeX.

;;; Commentary:

;;

;;; Code:

;; Install AUCTeX
(use-package auctex
  :hook (LaTeX-mode . prettify-symbols-mode))

(provide 'language-latex)

;;; language-latex.el ends here
