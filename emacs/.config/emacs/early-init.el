;;; early-init.el --- RangHo's early init file.

;; Copyright (C) 2023 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file is loaded before init.el.

;;; Code:

;; Emacs 26.2 apparently has a TLS bug
(if (and (<= emacs-major-version 26)
         (<= emacs-minor-version 2))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; If an `.el' file is newer than its corresponding `.elc' file, prefer the `.el' file.
(setq load-prefer-newer t)

;; Avoid garbage collection during startup
;; GCMH will be enabled anyways...
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Change default custom file
(setq custom-file (expand-file-name "etc/custom.el" user-emacs-directory))

;; Change default eln-cache directory
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

(provide 'early-init)

;;; early-init.el ends here
