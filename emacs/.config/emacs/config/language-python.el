;;; language-python.el --- Langugage support for python

;; Use pyright as the primary language server
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(provide 'language-python)
