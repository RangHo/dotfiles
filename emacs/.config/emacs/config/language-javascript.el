;;; language-javascript.el --- Language support for JavaScript and TypeScript

;; Emacs 27 has good JS mode including JSX support
(if (< emacs-major-version 27)
    (use-package js2-mode
      :hook (js2-mode . lsp)))

(use-package typescript-mode
  :hook (typescript-mode . lsp))

;; For TypeScript blacklist deno-ls
(add-to-list 'lsp-disabled-clients "deno-ls")
