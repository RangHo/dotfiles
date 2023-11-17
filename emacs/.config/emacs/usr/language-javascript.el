;;; language-javascript.el --- Language support for JavaScript and TypeScript

;;; Commentary:

;;

;;; Code:

(use-package js2-mode
  :mode (("\\.cjs\\'" . js2-mode)
         ("\\.mjs\\'" . js2-mode)
         ("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :hook (js2-mode . eglot-ensure))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook (typescript-mode . eglot-ensure))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.jsonc\\'" . jsonc-mode)
         ("\\.json5\\'" . jsonc-mode)
         ("tsconfig\\.json\\'" . jsonc-mode)))

;; If tree-sitter support is enabled
(when (featurep 'treesit)
  (add-hook 'js-ts-mode-hook #'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure))

(provide 'language-javascript)

;;; language-javascript.el ends here
