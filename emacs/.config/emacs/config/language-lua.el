;;; language-lua.el --- Language support for Lua

;;; Commentary:

;;

;;; Code:

(use-package lua-mode
  :hook (lua-mode . eglot-ensure))

(provide 'language-lua)

;;; language-lua.el ends here
