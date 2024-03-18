;;; framework-tree-sitter.el --- Emacs native tree-sitter integration

;;; Commentary:

;;

;;; Code:

(unless (< emacs-major-version 29)
  (require 'treesit))

(when (featurep 'treesit)
  ;; Change the default loading directory
  (setq treesit--install-language-grammar-out-dir-history
        (list (no-littering-expand-var-file-name "tree-sitter")))
  (setq treesit-extra-load-path
        (list (no-littering-expand-var-file-name "tree-sitter")))

  ;; Build the list of languages and their sources
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Make sure that all the languages are installed
  (mapc (lambda (lang-assoc)
          (unless (treesit-language-available-p (car lang-assoc))
            (message "Installing %s" (car lang-assoc))
            (treesit-install-language-grammar (car lang-assoc))))
        treesit-language-source-alist)

  ;; Replace existing major modes with the tree-sitter equivalent
  (setq major-mode-remap-alist
        '((sh-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (css-mode . css-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (java-mode . java-ts-mode)
          (js-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
          (js2-jsx-mode . tsx-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))

  ;; Notify that the tree-sitter integration is ready
  (message "Emacs will use tree-sitter from now on!"))


(provide 'framework-tree-sitter)

;;; framework-tree-sitter.el ends here
