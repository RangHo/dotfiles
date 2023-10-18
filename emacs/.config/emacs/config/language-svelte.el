;;; language-svelte.el --- Language support for Svelte

;;; Commentary:

;;

;;; Code:

;; Use Svelte major mode
(use-package svelte-mode
  :hook (svelte-mode . eglot-ensure))

(provide 'language-svelte)

;;; language-svelte.el ends here
