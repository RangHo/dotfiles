;;; language-fish.el --- Language support for fish shell.

;; Install fish major mode
(use-package fish-mode
  :straight (fish-mode :type git
                       :host github
                       :repo "wwwjfy/emacs-fish"))

;; Indent fish file before saving
(add-hook 'fish-mode-hook
          (lambda () (add-hook 'before-save-hook
                               'fish_indent-before-save)))

(provide 'language-fish)
