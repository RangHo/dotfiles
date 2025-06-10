;;; init.el --- RangHo's Emacs Configurations

;; Copyright (C) 2019--2023 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file is a part of my personal Emacs configurations.  To reduce the
;; clutter in my `user-emacs-directory', most of the configurations now reside
;; in a separate org file.  This init file simply loads that file with
;; `org-babel-load-file'.

;;; Code:

;; Set the default encoding as UTF-8
(set-language-environment "utf-8")

;; Load configurations
(org-babel-load-file (expand-file-name "README.org" user-emacs-directory))

(provide 'init)

;;; init.el ends here
