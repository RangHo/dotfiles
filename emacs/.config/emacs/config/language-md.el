;;; language-md.el --- Language support for markdown

;; Install markdown major mode
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t)
  :commands (markdown-mode gfm-mode))

(provide 'language-md)
