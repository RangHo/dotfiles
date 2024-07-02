;;; evil-line-numbers.el --- Evil-powered line numbers display

;; Copyright (C) 2024 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "26") (evil "1.14.2"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; `evil-line-numbers-mode' is a minor mode that extends the built-in
;; `display-line-numbers-mode' to display different types of line numbers for
;; different Evil states.  It complements the Vim-style navigation by shwoing
;; relative line numbers in insert state and absolute line numbers in normal
;; state.

;;; Code:

(require 'display-line-numbers)
(require 'evil)

(defun evil-line-numbers--enter-insert-state ()
  "Enable relative line numbers when entering insert state."
  (setq-local display-line-numbers 'relative))

(defun evil-line-numbers--exit-insert-state ()
  "Enable absolute line numbers when exiting insert state."
  (setq-local display-line-numbers t))

(defun evil-line-numbers--enable ()
  "Enable `evil-line-numbers-mode'."
  (setq-local display-line-numbers (if (evil-insert-state-p) 'relative t))
  (add-hook 'evil-insert-state-entry-hook #'evil-line-numbers--enter-insert-state nil t)
  (add-hook 'evil-insert-state-exit-hook #'evil-line-numbers--exit-insert-state nil t))

(defun evil-line-numbers--disable ()
  "Disable `evil-line-numbers-mode'."
  (setq-local display-line-numbers nil)
  (remove-hook 'evil-insert-state-entry-hook #'evil-line-numbers--enter-insert-state t)
  (remove-hook 'evil-insert-state-exit-hook #'evil-line-numbers--exit-insert-state t))

;;;###autoload
(define-minor-mode evil-line-numbers-mode
  "Minor mode that displays different types of line numbers for each Evil state."
  :init-value nil
  (if evil-line-numbers-mode
      (evil-line-numbers--enable)
    (evil-line-numbers--disable)))

(provide 'evil-line-numbers)

;;; evil-line-numbers.el ends here
