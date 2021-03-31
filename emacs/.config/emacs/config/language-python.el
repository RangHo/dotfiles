;;; language-python.el --- Langugage support for python

;; Use langauge server for Python
(add-hook 'python-mode-hook 'lsp)

(provide 'language-python)
