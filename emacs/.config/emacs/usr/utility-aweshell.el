;;; utility-aweshell.el --- Extension for eshell

;;; Commentary:

;;

;;; Code:

(use-package aweshell
  :elpaca (:host github :repo "manateelazycat/aweshell")
  :bind (("C-c RET" . aweshell-new)))

(provide 'utility-aweshell)

;;; utility-aweshell.el ends here
