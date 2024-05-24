;;; language-elixir.el --- Language support for Elixir/OTP

;;; Commentary:

;;

;;; Code:

;; Add Elixir syntax highlight support
(use-package elixir-mode
  :hook (elixir-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode . ("elixir-ls"))))

;; Use `web-mode' for EEx templates
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.leex\\'" . web-mode))

(provide 'language-elixir)

;;; language-elixir.el ends here
