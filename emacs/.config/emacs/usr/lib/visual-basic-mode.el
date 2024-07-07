;;; visual-basic-mode.el --- Major mode for editing variants of Visual Basic code

;; Copyright (C) 2024 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "25.1") (basic-mode "1.2.1"))

;; This file is NOT part of GNU Emacs.Gaprc

;;; Commentary:

;; This package provides a new mode for Visual Basic and its variants.
;; All of them are derived from Microsoft QBASIC dialect.

(require 'basic-mode)

;;;###autoload
(define-derived-mode visual-basic-mode basic-qb45-mode "Visual Basic"
  "Major mode for editing Visual Basic code."
  (basic-mode-initialize))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vb[as]?\\'" . visual-basic-mode))

(provide 'visual-basic-mode)

;;; visual-basic-mode.el ends here
