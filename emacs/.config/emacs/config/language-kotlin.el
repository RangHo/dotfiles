;;; language-kotlin.el --- Language support for Kotlin.

;; Install Kotlin major mode
(use-package kotlin-mode
  :hook (kotlin-mode . lsp-deferred))

(provide 'language-kotlin)
