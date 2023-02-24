;;; language-vimscript.el --- Language support form Vim resource files.
;;;
;;; I honestly don't know why I'm making this, but here we are.

;; Install VimScript major mode
(use-package vimrc-mode
  :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(provide 'language-vimscript)
