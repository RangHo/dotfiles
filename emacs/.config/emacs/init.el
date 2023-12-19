;;; init.el --- RangHo's Emacs configurations.

;; Copyright (C) 2019-2023 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file is a part of my personal Emacs configurations.  The init file sets
;; up the package manager, load some important packages.  Language-specific and
;; other utilities are broken down and loaded from a separate directory.  See
;; the `usr' directory for more specific settings.

;;; Code:

;;-------------------------------------------------------------------------------
;; Constants and helpers
;;
;; Useful values that are used throughout the init file.
;;-------------------------------------------------------------------------------

;; Useful user directory configurations
(defconst user-config-directory
  (cond ((eq system-type 'windows-nt)
         (file-name-as-directory (getenv "AppData")))
        ((getenv "XDG_CONFIG_HOME")
         (file-name-as-directory (getenv "XDG_CONFIG_HOME")))
        (t (concat (file-name-as-directory (getenv "HOME"))
                   ".config/")))
  "Per-user configuration directory.")
(defconst user-data-directory
  (cond ((eq system-type 'windows-nt)
         (file-name-as-directory (getenv "AppData")))
        ((getenv "XDG_DATA_HOME")
         (file-name-as-directory (getenv "XDG_DATA_HOME")))
        (t (concat (file-name-as-directory (getenv "HOME"))
                   ".local/share/")))
  "Per-user data directory.")
(defconst user-cache-directory
  (cond ((eq system-type 'windows-nt)
         (file-name-as-directory (concat user-data-directory "Cache")))
        ((getenv "XDG_CACHE_HOME")
         (file-name-as-directory (getenv "XDG_CACHE_HOME")))
        (t (concat (file-name-as-directory (getenv "HOME"))
                   ".cache/")))
  "Per-user cache directory.")

;; Long-lost Emacs string manipulation library
(use-package s)


;;-------------------------------------------------------------------------------
;; Emacs behavior modification
;;
;; Default Emacs has its own quirks. Let's fix that.
;;-------------------------------------------------------------------------------

;; Apply theme without asking (coz I made it anyways)
(load-theme 'rangho t)

;; Enable Xterm mouse support if no windowing system is found
(unless (display-graphic-p)
  (xterm-mouse-mode))

;; Ivy completion engine
;; For future me: this is for M-x completion
(use-package ivy
  :diminish
  :config
  (ivy-mode)
  (ivy-define-key ivy-minibuffer-map (kbd "<S-return>") #'ivy-immediate-done))

;; Show ElDoc documentation in a child frame
(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-at-point-mode))

;; Do not show native compilation warnings; they are annoying
(setq native-comp-async-report-warnings-errors nil)

;; y-or-n instead of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)


;;-------------------------------------------------------------------------------
;; Keybindings
;;
;; Emacs is all about using keyboard. These packages make it even more enjoyable.
;;-------------------------------------------------------------------------------

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

;; Hydra keybinding manager
(use-package hydra)

;; God speed!
(use-package god-mode)

;; View which keybinding is available
(use-package which-key
  :config (which-key-mode))


;;-------------------------------------------------------------------------------
;; Workspaces and projects
;;
;; Let's keep things nice and organized.
;;-------------------------------------------------------------------------------

;; Projectile project manager
(use-package projectile
  :ensure t
  :config (projectile-mode 1))

;; Treemacs project explorer
(use-package treemacs
  :after (hydra god-mode)
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
(use-package treemacs-projectile
  :after (treemacs projectile))

;; All-the-icons and treemacs integration
(use-package all-the-icons
  :if (or (daemonp) (display-graphic-p)))
(use-package treemacs-all-the-icons
  :if (or (daemonp) (display-graphic-p))
  :after (treemacs all-the-icons)
  :config (treemacs-load-theme "all-the-icons"))

;; Magit git repository manager
(use-package magit
  :init (if (not (boundp 'project-switch-commands))
            (setq project-switch-commands nil))
  :bind (("C-c g" . magit-file-dispatch)))


;;-------------------------------------------------------------------------------
;; Editing support
;;
;; Packages that are useful *specifically* when writing code.
;;-------------------------------------------------------------------------------

;; Fuck tabs because reasons
(setq-default indent-tabs-mode nil)
(setq-default tab-stop-list (number-sequence 4 120 4))

;; Display line number for programming-related modes
(setq-default display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Enable word wrap for text editing modes
(add-hook 'text-mode-hook 'visual-line-mode)

;; Flycheck syntax checker
(setq flycheck-emacs-lisp-load-path 'inherit)
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Language Server Protocol support
(use-package eglot)

;; Company in-buffer completion engine
;; For future me: this is for code completion
(use-package company
  :config
  (global-company-mode))
(use-package company-box
  :hook (company-mode . company-box-mode))

;; Undo-tree undo manager
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; EditorConfig plugin
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Use colorful delimiters because Lisp
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Always auto-insert matching delimiters
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Show RGB color codes what they look like
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :init
  (setq rainbow-x-colors nil))


;;-------------------------------------------------------------------------------
;; Extra configurations
;;
;; Language-specific configuration or utilities go in a separate file.
;;-------------------------------------------------------------------------------

;; Wait for core packages
(elpaca-wait)

;; Load non-global configurations
(let* ((config-dir (concat user-emacs-directory "usr"))
       (load-user-config-file
        (lambda (filename)
          (load-file (concat (file-name-as-directory config-dir)
                             filename)))))
  (if (file-directory-p config-dir)
      (mapc load-user-config-file
            (mapcar 'concat
                    (directory-files config-dir nil "\\.el")))))

;; Load custom file
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
