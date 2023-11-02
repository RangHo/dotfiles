;;; language-kotlin.el --- Language support for Kotlin.

;;; Commentary:

;;

;;; Code:

;; Install Kotlin major mode
(use-package kotlin-mode
  :hook (kotlin-mode . eglot-ensure))

(provide 'language-kotlin)

;;; language-kotlin.el ends here
