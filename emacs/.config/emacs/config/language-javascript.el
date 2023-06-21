;;; language-javascript.el --- Language support for JavaScript and TypeScript

;;; Commentary:

;; This package provides support for JavaScript and TypeScript.

;;; Code:

(use-package js2-mode
  :mode (("\\.cjs\\'" . js2-mode)
         ("\\.mjs\\'" . js2-mode)
         ("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :hook (js2-mode . lsp-deferred))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook (typescript-mode . lsp-deferred))

(provide 'language-javascript)

;;; language-javascript.el ends here
