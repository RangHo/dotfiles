;;; early-init.el --- RangHo's early init file.

;; Copyright (C) 2023 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file is loaded before init.el.
;; These values impact the initialization process, so they must be set very early
;; in the init file.  Usually performance-related and bootstrapping stuff.

;;; Code:

;; Change default eln-cache directory
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Emacs 26.2 apparently has a TLS bug
(if (and (<= emacs-major-version 26)
         (<= emacs-minor-version 2))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; If an `.el' file is newer than its corresponding `.elc' file, prefer the `.el' file.
(setq load-prefer-newer t)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Change default custom file
(setq custom-file (expand-file-name "etc/custom.el" user-emacs-directory))

;; Bootstrap elpaca
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "var/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
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
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

;; Do not litter Emacs directory
(use-package no-littering
  :config
  (no-littering-theme-backups)
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; Big GC threshold for big brain moments
(use-package gcmh
  :config
  (gcmh-mode 1))

;; Wait for the essential packages to be loaded
(elpaca-wait)

(provide 'early-init)

;;; early-init.el ends here
