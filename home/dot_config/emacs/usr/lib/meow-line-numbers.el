;;; meow-line-numbers.el --- Meow-powered line numbers display  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "26") (meow "1.5.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; `meow-line-numbers-mode' is a minor mode that extends the built-in
;; `display-line-numbers-mode' to display different types of line numbers for
;; different Meow states.  It shows relative line numbers in insert state and
;; absolute line numbers in other states.

;;; Code:

(require 'display-line-numbers)
(require 'meow)

(defun meow-line-numbers--enter-insert-state ()
  "Enable relative line numbers when entering insert state."
  (setq-local display-line-numbers 'relative))

(defun meow-line-numbers--exit-insert-state ()
  "Enable absolute line numbers when exiting insert state."
  (setq-local display-line-numbers t))

(defun meow-line-numbers--enable ()
  "Enable `meow-line-numbers-mode'."
  (setq-local display-line-numbers (if (meow-insert-mode-p) 'relative t))
  (add-hook 'meow-insert-enter-hook #'meow-line-numbers--enter-insert-state nil t)
  (add-hook 'meow-insert-exit-hook #'meow-line-numbers--exit-insert-state nil t))

(defun meow-line-numbers--disable ()
  "Disable `meow-line-numbers-mode'."
  (setq-local display-line-numbers nil)
  (remove-hook 'meow-insert-enter-hook #'meow-line-numbers--enter-insert-state t)
  (remove-hook 'meow-insert-exit-hook #'meow-line-numbers--exit-insert-state t))

;;;###autoload
(define-minor-mode meow-line-numbers-mode
  "Minor mode that displays different types of line numbers for each Meow state."
  :init-value nil
  (if meow-line-numbers-mode
      (meow-line-numbers--enable)
    (meow-line-numbers--disable)))

(provide 'meow-line-numbers)

;;; meow-line-numbers.el ends here
