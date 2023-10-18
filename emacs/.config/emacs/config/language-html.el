;;; language-html.el --- Language support for HTML and other template languages

;;; Commentary:

;;

;;; Code:

;; Install web-mode
(use-package web-mode
  :mode (("html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.handlebars\\'" . web-mode)
         ("\\.eco\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.rhtml\\'" . web-mode)
         ("\\.html\\.eex\\'" . web-mode)
         ("\\.html\\.leex\\'" . web-mode)
         ("\\.html\\.slim\\'" . web-mode)
         ("\\.pug\\'" . web-mode)
         ("\\.jade\\'" . web-mode)
         ("\\.twig\\'" . web-mode)
         ("\\.ctp\\'" . web-mode)
         ("\\.blade\\'" . web-mode)
         ("\\.haml\\'" . web-mode)
         ("\\.jsp?\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x?\\'" . web-mode)))

(provide 'language-html)

;;; language-html.el ends here
