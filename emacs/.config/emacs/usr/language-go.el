;;; language-go.el --- Language support for Go

;;; Commentary:

;;

;;; Code:

;; Add Go syntax highlight support
(use-package go-mode
  :hook (go-mode . eglot-ensure))

(provide 'language-go)

;;; language-go.el ends here
