;;; language-java.el --- Language support for Java and other JVM languages.

;; Automatically import Java classes
;; Note: This is disabled by default, as LSP is gonna handle this kind of
;; jobs for us anyway.
;(use-package java-imports
;  :hook (java-mode . java-imports-scan-file))

;; Install Groovy mode (pretty much only for gradle)
(use-package groovy-mode)

;; Install Kotlin mode
(use-package kotlin-mode)

(provide 'language-java)
