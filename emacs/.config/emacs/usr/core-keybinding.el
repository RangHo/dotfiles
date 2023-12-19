;;; core-keybinding.el --- List of keybindings

;;; Commentary:

;;

;;; Code:

;; Insert a zero-width space when pressing C-c SPC in insert mode
(evil-define-key 'insert 'global (kbd "C-c SPC")
  (lambda ()
    (interactive)
    (insert (char-from-name "ZERO WIDTH SPACE"))))

(provide 'core-keybinding)

;;; core-keybinding.el ends here
