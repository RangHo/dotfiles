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

;; Replace use-package with straight-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Place custom file in its own file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)

;; Hydra keybinding manager
(use-package hydra)

;; Undo-tree undo manager
(use-package undo-tree
  :config (global-undo-tree-mode))

;; Evil mode
(setq-default evil-want-keybinding nil)
(setq-default evil-want-integration t)
(use-package evil
  :after undo-tree
  :init
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
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

;; Treemacs project explorer
(use-package treemacs
  :after hydra)
(use-package treemacs-evil
  :after '(treemacs evil))

;; Magit git repository manager
(use-package magit)

;; Ivy completion engine
(use-package ivy
  :diminish
  :config (ivy-mode))

;; Language Server Protocol support
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :commands lsp)
(use-package lsp-ui
  :commands lsp-ui)
(use-package lsp-ivy
  :after ivy
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :after treemacs
  :commands lsp-treemacs-error-list)
(use-package dap-mode)

;; Move the autosave/backup files out of the way
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Fuck tabs because reasons
(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list (number-sequence 4 120 4))

;; Load language-specific configurations
(let* ((config-dir (concat user-emacs-directory "config"))
       (load-user-config-file
        (lambda (filename)
          (load-file (concat(file-name-as-directory config-dir)
			    filename)))))
  (if (file-directory-p config-dir)
      (mapc load-user-config-file
            (mapcar 'concat
                    (directory-files config-dir nil "\\.el")))))
