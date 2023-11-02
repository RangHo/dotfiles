;;; language-vimscript.el --- Language support form Vim resource files.

;;; Commentary:

;; Yes, this *is* sacrilege.

;;; Code:

;; Install VimScript major mode
(use-package vimrc-mode
  :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))

(provide 'language-vimscript)

;;; language-vimscript.el ends here
