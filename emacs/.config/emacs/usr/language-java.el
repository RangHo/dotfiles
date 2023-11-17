;;; language-java.el --- Language support for Java.

;;; Commentary:

;;

;;; Code:

(add-hook 'java-mode-hook #'eglot-ensure)

(provide 'language-java)

;;; language-java.el ends here
