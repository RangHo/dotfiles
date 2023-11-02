;;; framework-web.el --- Base support for web development

;;; Commentary:

;; This file provides base support for web development via `web-mode'.
;; Support for specific languages is provided by other `language-*.el' files by
;; modifying `auto-mode-alist' to load `web-mode'.

;;; Code:

;; Allow dynamic selection of language server
(defvar rangho/web-mode-server-program-alist
  '(nil . ("echo" "No language server available for this language"))
  "Alist of language servers for `web-mode'.")
(defun rangho/select-web-mode-server-program (_)
  "Select a language server for `web-mode'."
  (cdr (or (assoc web-mode-engine
                  rangho/web-mode-server-program-alist)
           (assoc nil rangho/web-mode-server-program-alist))))

;; Install web-mode
(use-package web-mode
  :hook (web-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(web-mode . rangho/select-web-mode-server-program)))

(provide 'framework-web)

;;; framework-web.el ends here
