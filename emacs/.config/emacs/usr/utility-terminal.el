;;; utility-terminal.el --- Terminal emulator for Emacs

;;; Commentary:

;;

;;; Code:

(use-package eat
  :bind (("C-c RET" . eat))
  :hook ((eshell-load . eat-eshell-mode)
	 (eshell-load . eat-eshell-visual-command-mode)))

(provide 'utility-terminal)

;;; utility-terminal.el ends here
