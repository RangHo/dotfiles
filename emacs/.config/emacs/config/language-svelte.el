;;; language-svelte.el --- Language support for Svelte

;; Use Svelte major mode
(use-package svelte-mode
  :hook (svelte-mode . lsp))

(add-to-list 'lsp-language-id-configuration '(svelte-mode . "svelte"))

(provide 'language-svelte)
