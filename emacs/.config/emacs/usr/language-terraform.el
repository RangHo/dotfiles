;;; language-terraform.el --- Langauge support for Terraform.

;;; Commentary:

;;

;;; Code:

;; Use Terraform major mode
(use-package terraform-mode
  :hook (terraform-mode . eglot-ensure)
  :init
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve"))))

(provide 'language-terraform)

;;; language-terraform.el ends here
