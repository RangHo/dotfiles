;;; language-css.el --- Langauge support for CSS and other stylesheet languages

;;; Commentary:

;;

;;; Code:

;; Add PostCSS extension to CSS mode
(add-to-list 'auto-mode-alist '("\\.pcss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.postcss\\'" . css-mode))

(provide 'language-css)

;;; language-css.el ends here
