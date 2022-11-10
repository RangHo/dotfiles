;;; language-python.el --- Langugage support for python

;; Use langauge server for Python
(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'language-python)
