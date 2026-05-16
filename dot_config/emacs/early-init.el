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

;; Silence native compilation warnings.
(setq native-comp-async-report-warnings-errors 'silent)

;; If an `.el' file is newer than its corresponding `.elc' file, prefer the `.el' file.
(setq load-prefer-newer t)

;; Incrase GC threshold to reduce GC frequency.
(setq gc-cons-threshold most-positive-fixnum)

;; Set up customized no-littering directories and functions before loading the package.
(with-eval-after-load 'no-littering
  (defvar no-littering-usr-directory
    (expand-file-name (convert-standard-filename "usr/") user-emacs-directory)
    "The directory where users place their own files.
  This variable has to be set before `no-littering' is loaded.")

  (defun no-littering-expand-usr-file-name (file)
    "Expand filename FILE relative to `no-littering-usr-directory'."
    (expand-file-name (convert-standard-filename file)
                      no-littering-usr-directory))

  (defvar no-littering-lib-directory
    (expand-file-name (convert-standard-filename "lib/") user-emacs-directory)
    "The directory where standalone packages are placed.
  This variable has to be set before `no-littering' is loaded.")

  (defun no-littering-expand-lib-file-name (file)
    "Expand filename FILE relative to `no-littering-lib-directory'."
    (expand-file-name (convert-standard-filename file)
                      no-littering-lib-directory))

  (defun no-littering-theme-custom ()
    "Theme the Emacs customization feature."
    (setq custom-file            (no-littering-expand-etc-file-name "custom.el"))
    (setq custom-theme-directory (no-littering-expand-etc-file-name "themes/"))
    (when (file-exists-p custom-file)
      (load custom-file)))

  (defun no-littering-theme-eln-cache ()
    "Theme the Emacs native compilation cache."
    (when (and (fboundp 'startup-redirect-eln-cache)
               (fboundp 'native-comp-available-p)
               (native-comp-available-p))
      (startup-redirect-eln-cache
       (convert-standard-filename (no-littering-expand-var-file-name "eln-cache/")))))

  (defun no-littering-theme-elpa ()
    "Theme the Emacs package.el directory."
    (setq package-user-dir (no-littering-expand-var-file-name "elpa/")))

  (defun no-littering-theme-load-path ()
    "Theme the Emacs Lisp load path."
    (let ((default-directory no-littering-lib-directory))
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))
    (add-to-list 'load-path (no-littering-expand-usr-file-name "lib/")))

  (no-littering-theme-backups)
  (no-littering-theme-custom)
  (no-littering-theme-eln-cache)
  (no-littering-theme-elpa)
  (no-littering-theme-load-path))

;; Make Emacs stop litter things everywhere.
(require 'no-littering
         (expand-file-name "lib/no-littering/no-littering.el" user-emacs-directory))

(provide 'early-init)
;;; early-init.el ends here
