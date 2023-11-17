;;; language-csharp.el --- Language support for C#

;;; Commentary:

;;

;;; Code:

;; Add C# syntax highlighting support
(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode)
         ("\\.csx\\'" . csharp-mode))
  :hook (csharp-mode . eglot-ensure))

;; When tree-sitter support is available
(when (featurep 'treesit)
  (add-hook 'csharp-ts-mode-hook #'eglot-ensure))

(provide 'language-csharp)

;;; language-csharp.el ends here
