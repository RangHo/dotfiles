;;; language-csharp.el --- Language support for C#

;; Add C# syntax highlighting support
(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode)
         ("\\.csx\\'" . csharp-mode))
  :hook (csharp-mode . lsp-deferred))

(provide 'language-csharp)
