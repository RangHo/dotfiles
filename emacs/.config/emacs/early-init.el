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

;; If an `.el' file is newer than its corresponding `.elc' file, prefer the `.el' file.
(setq load-prefer-newer t)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Bootstrap elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "var/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Replace use-package with elpaca-use-package
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

(elpaca-wait)

;; Do not litter Emacs directory
(defun rangho/no-littering-theme-custom ()
  "Theme the Emacs customization feature."
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(defun rangho/no-littering-theme-eln-cache ()
  "Theme the Emacs native compilation cache.

This function will move the littered eln-cache directory to the no-littering directory."
  (let ((old-eln-cache (expand-file-name "eln-cache/" user-emacs-directory))
        (new-eln-cache (no-littering-expand-var-file-name "eln-cache/")))
    ;; Set the new eln-cache directory
    (startup-redirect-eln-cache new-eln-cache)

    ;; Move the contents of the old eln-cache directory to the new eln-cache directory
    (when (file-exists-p old-eln-cache)
      (unless (file-exists-p new-eln-cache)
        (make-directory new-eln-cache t))
      (dolist (file (directory-files old-eln-cache t))
        (when (file-regular-p file)
          (copy-file file (expand-file-name (file-name-nondirectory file) new-eln-cache) t))))

    ;; Delete the old eln-cache directory
    (delete-directory old-eln-cache 'recursive)))

(use-package no-littering
  :config
  (no-littering-theme-backups)
  (rangho/no-littering-theme-custom)
  (rangho/no-littering-theme-eln-cache))

;; Apply magical GC settings
(use-package gcmh
  :custom
  (gcmh-verbose t)
  :config
  (gcmh-mode 1))

;; Wait for the essential packages to be loaded
(elpaca-wait)

(provide 'early-init)

;;; early-init.el ends here
