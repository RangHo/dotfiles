;;; language-rust.el --- Langugage support for Rust

;;; Commentary:

;;

;;; Code:

(defun rangho/rust-add-format-hook ()
  "Add a hook to format rust code before saving."
  (add-hook 'write-contents-functions
            (lambda ()
              (when (or (eq major-mode 'rust-mode)
                        (eq major-mode 'rust-ts-mode))
                (eglot-format-buffer)))))

;; Install Rust major mode
(use-package rust-mode
  :hook ((rust-mode . eglot-ensure)
         (rust-mode . rangho/rust-add-format-hook)))

;; When tree-sitter support is available
(when (featurep 'treesit)
  (add-hook 'rust-ts-mode-hook #'eglot-ensure)
  (add-hook 'rust-ts-mode-hook #'rangho/rust-add-format-hook))

(provide 'language-rust)

;;; language-rust.el ends here
