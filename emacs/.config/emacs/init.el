;;===============================================================================
;;  _       _ _         _ 
;; (_)_ __ (_) |_   ___| |
;; | | '_ \| | __| / _ \ |
;; | | | | | | |_ |  __/ |
;; |_|_| |_|_|\__(_)___|_|
;; 
;; Emacs configuration file, created by RangHo.
;;===============================================================================

;; Emacs 26.2 apparently has a TLS bug
(if (and (<= emacs-major-version 26)
         (<= emacs-minor-version 2))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Initialize straight.el first
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Useful user directory configurations
(defconst user-config-directory
  (cond ((eq system-type 'windows-nt)
         (file-name-as-directory (getenv "AppData")))
        ((getenv "XDG_CONFIG_HOME")
         (file-name-as-directory (getenv "XDG_CONFIG_HOME")))
        (t (concat (file-name-as-directory (getenv "HOME"))
                   ".config/")))
  "Per-user configuration directory")
(defconst user-data-directory
  (cond ((eq system-type 'windows-nt)
         (file-name-as-directory (getenv "AppData")))
        ((getenv "XDG_DATA_HOME")
         (file-name-as-directory (getenv "XDG_DATA_HOME")))
        (t (concat (file-name-as-directory (getenv "HOME"))
                   ".local/share/")))
  "Per-user data directory")
(defconst user-cache-directory
  (cond ((eq system-type 'windows-nt)
         (file-name-as-directory (concat user-data-directory "Cache")))
        ((getenv "XDG_CACHE_HOME")
         (file-name-as-directory (getenv "XDG_CACHE_HOME")))
        (t (concat (file-name-as-directory (getenv "HOME"))
                   ".cache/")))
  "Per-user cache directory")

;; Replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Place custom file in its own file
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))

;; Enable Xterm mouse support if no windowing system is found
(unless (display-graphic-p)
  (xterm-mouse-mode))

;; Hydra keybinding manager
(use-package hydra)

;; View which keybinding is available
(use-package which-key
  :config (which-key-mode))

;; Undo-tree undo manager
(use-package undo-tree
  :config (global-undo-tree-mode))

;; Evil mode
(use-package evil
  :after undo-tree
  :init
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config (evil-mode))
(use-package evil-collection
  :after evil
  :init (evil-collection-init))
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode))

;; EditorConfig plugin
(use-package editorconfig
  :config (editorconfig-mode))

;; All-the-icons
(use-package all-the-icons)

;; Treemacs project explorer
(use-package treemacs
  :after hydra
  :defer t
  :config (progn (treemacs-follow-mode t)
                 (treemacs-filewatch-mode t)
                 (treemacs-fringe-indicator-mode 'always)
                 (pcase (cons (not (null (executable-find "git")))
                              (not (null treemacs-python-executable)))
                   (`(t . t)
                    (treemacs-git-mode 'deferred))
                   (`(t . _)
                    (treemacs-git-mode 'simple)))))
(use-package treemacs-evil
  :after (treemacs evil))
(use-package treemacs-magit
  :after (treemacs magit))
(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config (treemacs-load-theme "all-the-icons"))

;; Magit git repository manager
(use-package magit)

;; Ivy completion engine
(use-package ivy
  :diminish
  :config (ivy-mode))

;; Company in-buffer completion engine
(use-package company
  :config (global-company-mode))

;; Language Server Protocol support
;; Starting from Emacs 29, eglot langserver client is built-in.
(if (< emacs-major-version 29)
    (use-package eglot))

;; Appearance packages
(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-wavy-trail t)
  :config (nyan-mode))

;; Move the autosave/backup files out of the way
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Activate uim if available
(setq-default uim-candidate-display-inline t)
(when (require 'uim nil 'no-error)
  (add-hook 'prog-mode-hook (lambda () (uim-mode))))

;; Fuck tabs because reasons
(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list (number-sequence 4 120 4))

;; Display line number for programming-related modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Load non-global configurations
(let* ((config-dir (concat user-emacs-directory "config"))
       (load-user-config-file
        (lambda (filename)
          (load-file (concat (file-name-as-directory config-dir)
                             filename)))))
  (if (file-directory-p config-dir)
      (mapc load-user-config-file
            (mapcar 'concat
                    (directory-files config-dir nil "\\.el")))))

;; Apply theme without asking (coz I made it anyways)
(load-theme 'rangho t)

(provide 'init)
