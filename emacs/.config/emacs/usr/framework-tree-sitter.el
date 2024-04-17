;;; framework-tree-sitter.el --- Emacs native tree-sitter integration

;;; Commentary:

;;

;;; Code:

(unless (< emacs-major-version 29)
  (require 'treesit))

(when (featurep 'treesit)
  ;; Change the default loading directory
  (setq treesit--install-language-grammar-out-dir-history
        (list (no-littering-expand-var-file-name "tree-sitter")))
  (setq treesit-extra-load-path
        (list (no-littering-expand-var-file-name "tree-sitter")))

  (use-package treesit-auto
    :config
    (global-treesit-auto-mode)))


(provide 'framework-tree-sitter)

;;; framework-tree-sitter.el ends here
