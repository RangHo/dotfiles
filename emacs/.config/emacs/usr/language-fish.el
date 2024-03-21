;;; language-fish.el --- Language support for fish shell.

;;; Commentary:

;;

;;; Code:

;; Install fish major mode
(use-package fish-mode
  :hook (fish-mode . (lambda ()
                       (add-hook 'before-save-hook
                                 'fish_indent-before-save))))

;; Indent fish file before saving
(add-hook 'fish-mode-hook
          (lambda () (add-hook 'before-save-hook
                               'fish_indent-before-save)))

(provide 'language-fish)

;;; language-fish.el ends here
