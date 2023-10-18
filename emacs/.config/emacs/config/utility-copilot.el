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
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :after evil
  :ensure t
  :hook (prog-mode . copilot-mode)
  :init (evil-define-key 'insert copilot-mode-map (kbd "TAB") #'rangho/copilot-tab))

(provide 'utility-copilot)

;;; utility-copilot.el ends here
