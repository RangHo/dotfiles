;;; language-java.el --- Language support for Java.

;; Automatically import Java classes
;; Note: This is disabled by default, as LSP is gonna handle this kind of
;; jobs for us anyway.
;(use-package java-imports
;  :hook (java-mode . java-imports-scan-file))

;; Use LSP for Java
(use-package lsp-java
  :after lsp-mode
  :hook (java-mode . lsp-deferred))

(provide 'language-java)
