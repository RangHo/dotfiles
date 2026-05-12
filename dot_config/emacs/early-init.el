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

;; Include local library directory and subdirectories to `load-path'.
(let ((default-directory (expand-file-name "usr/lib" user-emacs-directory)))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

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

  (defun no-littering-theme-custom ()
    "Theme the Emacs customization feature."
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
    (when (file-exists-p custom-file)
      (load custom-file)))

  (defun no-littering-theme-custom-themes ()
    "Theme the Emacs custom theme location."
    (let ((old-custom-themes (expand-file-name "themes/" user-emacs-directory))
          (new-custom-themes (no-littering-expand-etc-file-name "themes/")))
      ;; Set the new custom themes directory.
      (setq custom-theme-directory new-custom-themes)
      ;; Move the contents of the old custom themes directory to the new custom themes directory.
      (when (file-exists-p old-custom-themes)
        (unless (file-exists-p new-custom-themes)
          (make-directory new-custom-themes t))
        (dolist (file (directory-files old-custom-themes t))
          (when (file-regular-p file)
            (copy-file file (expand-file-name (file-name-nondirectory file) new-custom-themes) t)))))
    ;; Delete the old custom themes directory.
    (delete-directory old-custom-themes t))

  (defun no-littering-theme-eln-cache ()
    "Theme the Emacs native compilation cache.

    This function will move the littered eln-cache directory to the no-littering directory."
    (let ((old-eln-cache (expand-file-name "eln-cache/" user-emacs-directory))
          (new-eln-cache (no-littering-expand-var-file-name "eln-cache/")))
      ;; Set the new eln-cache directory.
      (startup-redirect-eln-cache new-eln-cache)
      ;; Move the contents of the old eln-cache directory to the new eln-cache directory.
      (when (file-exists-p old-eln-cache)
        (unless (file-exists-p new-eln-cache)
          (make-directory new-eln-cache t))
        (dolist (file (directory-files old-eln-cache t))
          (when (file-regular-p file)
            (copy-file file (expand-file-name (file-name-nondirectory file) new-eln-cache) t))))
      ;; Delete the old eln-cache directory.
      (delete-directory old-eln-cache t)))

  (defun no-littering-theme-elpa ()
    "Theme the Emacs package.el directory."
    (let ((old-elpa (expand-file-name "elpa/" user-emacs-directory))
          (new-elpa (no-littering-expand-var-file-name "elpa/")))
      ;; Set the new elpa directory.
      (setq package-user-dir new-elpa)
      ;; Move the contents of the old elpa directory to the new elpa directory.
      (when (file-exists-p old-elpa)
        (unless (file-exists-p new-elpa)
          (make-directory new-elpa t))
        (dolist (file (directory-files old-elpa t))
          (when (file-regular-p file)
            (copy-file file (expand-file-name (file-name-nondirectory file) new-elpa) t))))
      ;; Delete the old elpa directory.
      (delete-directory old-elpa t)))

  (defun no-littering-theme-treesit ()
    "Theme the Emacs tree-sitter library.

  This function will move the littered tree-sitter directory to the no-littering directory."
    (let ((old-tree-sitter (expand-file-name "tree-sitter/" user-emacs-directory))
          (new-tree-sitter (no-littering-expand-var-file-name "tree-sitter/")))
      ;; Set the new tree-sitter directory.
      (setq treesit-extra-load-path (list new-tree-sitter))
      ;; Move the contents of the old tree-sitter directory to the new tree-sitter directory.
      (when (file-exists-p old-tree-sitter)
        (unless (file-exists-p new-tree-sitter)
          (make-directory new-tree-sitter t))
        (dolist (file (directory-files old-tree-sitter t))
          (when (file-regular-p file)
            (copy-file file (expand-file-name (file-name-nondirectory file) new-tree-sitter) t))))
      ;; Delete the old tree-sitter directory.
      (delete-directory old-tree-sitter t)))

  (no-littering-theme-backups)
  (no-littering-theme-custom)
  (no-littering-theme-custom-themes)
  (no-littering-theme-eln-cache)
  (no-littering-theme-elpa)
  (no-littering-theme-treesit))

;; Make Emacs stop litter things everywhere.
(require 'no-littering)

(provide 'early-init)
;;; early-init.el ends here
