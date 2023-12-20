;;; core-keybinding.el --- List of keybindings

;;; Commentary:

;;

;;; Code:

;; Insert a zero-width space when pressing C-c SPC in insert mode
(evil-define-key 'insert 'global (kbd "C-c SPC")
  (lambda ()
    (interactive)
    (insert (char-from-name "ZERO WIDTH SPACE"))))

;; Switch to treemacs buffer when pressing C-c TAB
(evil-define-key 'normal 'global (kbd "C-c TAB") 'treemacs-select-window)

(provide 'core-keybinding)

;;; core-keybinding.el ends here
