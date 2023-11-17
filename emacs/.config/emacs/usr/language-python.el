;;; language-python.el --- Langugage support for python

;;; Commentary:

;;

;;; Code:

(add-hook 'python-mode-hook 'eglot-ensure)

;; When tree-sitter is enabled
(when (featurep 'treesit)
  (add-hook 'python-ts-mode-hook 'eglot-ensure))

(provide 'language-python)

;;; language-python.el ends here
