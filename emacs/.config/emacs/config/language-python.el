;;; language-python.el --- Langugage support for python

;;; Commentary:

;;

;;; Code:

(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'language-python)

;;; language-python.el ends here
