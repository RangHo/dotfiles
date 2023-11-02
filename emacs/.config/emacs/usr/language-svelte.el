;;; language-svelte.el --- Language support for Svelte

;;; Commentary:

;;

;;; Code:

;; Load `web-mode' for Svelte files
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

;; Configure Svelte language server
(add-to-list 'rangho/web-mode-server-program-alist
             '("svelte" . ("svelteserver" "--stdio")))

(provide 'language-svelte)

;;; language-svelte.el ends here
