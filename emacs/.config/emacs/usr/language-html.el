;;; language-html.el --- Language support for HTML and other template languages

;;; Commentary:

;;

;;; Code:

;; Register HTML template files with `web-mode'
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eco\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.slim\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.pug\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jade\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.haml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x?\\'" . web-mode))

(provide 'language-html)

;;; language-html.el ends here
