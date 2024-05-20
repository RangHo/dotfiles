;;; language-docker.el --- Language support for Dockerfile

;;; Commentary:

;;

;;; Code:

(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode)
         ("Containerfile\\'" . dockerfile-mode)
         ("\\.dockerfile\\'" . dockerfile-mode))

(provide 'language-docker)

;;; language-docker.el ends here
