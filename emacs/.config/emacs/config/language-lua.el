;;; language-lua.el --- Language support for Lua

(use-package lua-mode
  :hook (lua-mode . eglot-ensure))

(provide 'language-lua)
