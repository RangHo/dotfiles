;;; language-javascript.el --- Language support for JavaScript and TypeScript

;; Emacs 27 has good JS mode including JSX support
(if (< emacs-major-version 27)
    (use-package js2-mode
      :hook (js2-mode . eglot-ensure)))

(use-package typescript-mode
  :hook (typescript-mode . eglot-ensure))
