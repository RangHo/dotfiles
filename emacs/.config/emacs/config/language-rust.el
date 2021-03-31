;;; language-rust.el --- Langugage support for Rust

;; Install Rust major mode
(use-package rust-mode
  :hook (rust-mode . lsp))

;; Use rustfmt when saving sources
(add-hook 'before-save-hook (lambda ()
                              (when (eq major-mode 'rust-mode)
                                (lsp-format-buffer))))

(provide 'language-rust)
