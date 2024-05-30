;;; language-julia.el --- Language support for Julia

;;; Commentary:

;;

;;; Code:

(require 'eglot)
(require 'no-littering)

;; Set up Julia language server
(defconst rangho/julia-language-server-project
  (no-littering-expand-var-file-name "julia-language-server/")
  "Path to the Emacs-specific Julia project.")

(defconst rangho/julia-language-server-script
  (expand-file-name "usr/share/eglot.jl" user-emacs-directory))

(defun rangho/julia-language-server-contact (_interactive)
  "Produce the Eglot contact information for Julia language server."
  ;; Make sure that the Julia project directory is available
  (unless (file-exists-p rangho/julia-language-server-project)
    (make-directory rangho/julia-language-server-project t))

  ;; Produce the contact information
  (list (executable-find "julia")
	"--startup-file=no"
	(concat "--project=" rangho/julia-language-server-project)
	rangho/julia-language-server-script
	(file-name-directory (buffer-file-name))))

;; Install Julia major mode
(use-package julia-mode
  :hook (julia-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '(julia-mode . rangho/julia-language-server-contact)))

(use-package julia-ts-mode
  :if nil ; (featurep 'treesit)
  :hook (julia-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '(julia-ts-mode . rangho/julia-language-server-contact)))

(provide 'language-julia)

;;; language-julia.el ends here
