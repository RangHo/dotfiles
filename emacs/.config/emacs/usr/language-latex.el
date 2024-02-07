;;; language-latex.el --- Language support for LaTeX.

;;; Commentary:

;;

;;; Code:

;; Install AUCTeX
(use-package auctex
  :elpaca (auctex :pre-build (("./autogen.sh")
                              ("./configure" "--without-texmf-dir" "--with-lispdir=.")
                              ("make")
                              ("install-info" "doc/auctex.info" "doc/dir")
                              ("install-info" "doc/preview-latex.info" "doc/dir")))
  :hook (LaTeX-mode . prettify-symbols-mode))

(provide 'language-latex)

;;; language-latex.el ends here
