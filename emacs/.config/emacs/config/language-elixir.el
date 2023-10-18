;;; language-elixir.el --- Language support for Elixir/OTP

;;; Commentary:

;;

;;; Code:

;; Add Elixir syntax highlight support
(use-package elixir-mode
  :hook (elixir-mode . eglot-ensure))

(provide 'language-elixir)

;;; language-elixir.el ends here
