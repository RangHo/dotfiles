;;; utility-copilot.el --- GitHub Copilot support for Emacs

;;; Commentary:

;; This package provides support for using GitHub Copilot in Emacs.

;;; Code:

;; Define custom <tab> behavior
(defun rangho/copilot-tab ()
  "Complete the current line with Copilot or indent with tab."
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

;; Install GitHub Copilot unofficial plugin
(use-package copilot
  :elpaca (:host github :repo "copilot-emacs/copilot.el" :files (:defaults "dist"))
  :defer t
  :after (evil jsonrpc)
  :hook (prog-mode . copilot-mode)
  :config
  (evil-define-key 'insert copilot-mode-map (kbd "TAB") #'rangho/copilot-tab)
  (add-to-list 'warning-suppress-types '(copilot copilot-no-mode-indent)))

(provide 'utility-copilot)

;;; utility-copilot.el ends here
