;;; language-svelte.el --- Language support for Svelte

;; Use Svelte major mode
(use-package svelte-mode
  :hook (svelte-mode . eglot-ensure))

(add-to-list 'eglot-server-programs
             '(svelte-mode :language-id "svelte"))

(provide 'language-svelte)
