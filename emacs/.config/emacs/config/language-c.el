;;; langauge-c.el --- Language support for C/C++/Objective-C/Objective-C++

;; Add commonly used C++ extensions
(add-to-list 'auto-mode-alist '("\\.hpp'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.hh'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.h++'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c++'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.hxx'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.txx'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl'" . c++-mode))

;; Use language server for C
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;; Install CMake support
(use-package cmake-mode)

(provide 'language-c)
