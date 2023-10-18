;;; langauge-cc.el --- Language support for C/C++/Objective-C/Objective-C++

;;; Commentary:

;; This configuration does the following:
;; - automatically load the correct mode for various C++ files
;; - enable LSP mode
;; - add CMake and Meson support
;; - add Clang-Format support
;; - extract C style settings from Clang-Format, if available

;;; Code:

;; Add commonly used C++ extensions
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.h++\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c++\\'" . c++-mode))

(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.txx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))

;; Use language server for C
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'objc-mode-hook 'eglot-ensure)

;; Install CMake support
(use-package cmake-mode)

;; Install meson support
(use-package meson-mode
  :hook (meson-mode . company-mode))

;; Install clang-format support
(defun rangho/clang-format-item (config-string key &optional default)
  "Extract the value of KEY from `.clang-format' file's content provided as CONFIG-STRING."
  (let ((match-result (s-match (concat "^" key ":[ \t]*\\([a-zA-Z0-9]+\\)") config-string)))
    (if match-result
        (cadr match-result)
      default)))
(use-package clang-format
  :hook ((c-mode-common . (lambda ()
                           (let* ((clang-format-string (shell-command-to-string "clang-format -dump-config"))
                                  (indent-width (rangho/clang-format-item clang-format-string "IndentWidth" "4"))
                                  (use-tab (rangho/clang-format-item clang-format-string "UseTab" "Never")))
                             (progn
                               (setq-local c-basic-offset (string-to-number indent-width))
                               (setq-local indent-tabs-mode (not (string-equal use-tab "Never")))))
                           (add-hook 'before-save-hook #'clang-format-buffer nil t)))))

(provide 'language-c)

;;; language-cc.el ends here
