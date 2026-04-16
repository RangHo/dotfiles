;;; early-init.el --- RangHo's early init file.

;; Copyright (C) 2023 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file is loaded before init.el.
;; These values impact the initialization process, so they must be set very early
;; in the init file.  Usually performance-related and bootstrapping stuff.

;;; Code:

;; Silence native compilation warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; If an `.el' file is newer than its corresponding `.elc' file, prefer the `.el' file.
(setq load-prefer-newer t)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Incrase GC threshold to reduce GC frequency
(setq gc-cons-threshold most-positive-fixnum)

(provide 'early-init)

;;; early-init.el ends here
