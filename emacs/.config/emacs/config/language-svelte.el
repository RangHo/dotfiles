;;; language-svelte.el --- Language support for Svelte

;; Use Svelte major mode
(use-package svelte-mode
  :hook (svelte-mode . lsp-deferred))

(provide 'language-svelte)
