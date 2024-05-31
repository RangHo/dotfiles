;;; core-keybinding.el --- List of keybindings

;;; Commentary:

;; These keybindings do not belong to any specific package, but are
;; rather general keybindings that are useful to have in any context.
;; Package-specific keybindings should be defined in their respective
;; `use-package' declarations.

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
