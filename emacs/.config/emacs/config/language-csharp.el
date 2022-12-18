;;; language-csharp.el --- Language support for C#

;; Add C# syntax highlighting support
(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode)
         ("\\.csx\\'" . csharp-mode))
  :hook (csharp-mode . eglot-ensure))

(provide 'language-csharp)
