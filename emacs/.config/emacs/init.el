;;; init.el --- RangHo's Emacs configurations.

;; Copyright (C) 2019-2023 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file is a part of my personal Emacs configurations.  The init file sets
;; up the package manager, load some important packages.  Language-specific and
;; other utilities are broken down and loaded from a separate directory.  See
;; the `usr' directory for more specific settings.

;;; Code:

;;-------------------------------------------------------------------------------
;; Constants and environments
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

;; Use mise if available
(use-package mise
  :if (executable-find "mise")
  :ensure (:host github :repo "liuyinz/mise.el")
  :init
  (let ((mise-shims (expand-file-name "mise/shims" user-data-directory)))
    (setenv "PATH" (concat (getenv "PATH") ":" mise-shims))
    (add-to-list 'exec-path mise-shims))
  :config
  (global-mise-mode))

;; Apply theme without asking (coz I made it anyways)
(load-theme 'rangho t)

;; Show fancy dashboard on startup
(use-package dashboard
  :hook ((elpaca-after-init . dashboard-insert-startupify-lists)
         (elpaca-after-init . dashboard-initialize))
  :custom
  (initial-buffer-choice (lambda ()
                           (dashboard-refresh-buffer)
                           (get-buffer-create dashboard-buffer-name)))
  (dashboard-startup-banner (expand-file-name "usr/share/GNUEmacs.png" user-emacs-directory))
  (dashboard-image-banner-max-height 400)
  (dashboard-icon-type 'all-the-icons)
  :config
  (dashboard-setup-startup-hook))

;; Wait for packages
(elpaca-wait)


;;-------------------------------------------------------------------------------
;; Emacs behavior modification
;;
;; Default Emacs has its own quirks. Let's fix that.
;;-------------------------------------------------------------------------------

;; Enable Xterm mouse support if no windowing system is found
(unless (display-graphic-p)
  (xterm-mouse-mode))

;; Don't show warnings unless it's really important
(setq native-comp-async-report-warnings-errors nil)

;; Don't make lockfiles
(setq create-lockfiles nil)

;; y-or-n instead of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Fix calls to the deprecated cl libraries
;; Wait for packages
(elpaca-wait)


;;-------------------------------------------------------------------------------
;; Keybindings
;;
;; Emacs is all about using keyboard. These packages make it even more enjoyable.
;;-------------------------------------------------------------------------------

;; Evil mode
(use-package evil
  :custom
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-undo-system 'undo-tree)
  (evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :init
  (evil-collection-init))
(use-package evil-surround
  :config
  (global-evil-surround-mode))

;; Hydra keybinding manager
(use-package hydra)

;; God speed!
(use-package god-mode)

;; View which keybinding is available
(use-package which-key
  :config
  (which-key-mode))

;; Transient command menus
(use-package transient
  :bind (:map transient-map ("q" . transient-quit-one)
         :map transient-edit-map ("q" . transient-quit-one)
         :map transient-sticky-map ("q" . transient-quit-seq)))

;; Wait for packages
(elpaca-wait)


;;-------------------------------------------------------------------------------
;; Workspaces and projects
;;
;; Let's keep things nice and organized.
;;-------------------------------------------------------------------------------

;; Projectile project manager
(use-package projectile
  :config
  (projectile-mode 1))

;; Magit git repository manager
(use-package magit
  :bind (("C-c g" . magit-file-dispatch))
  :init
  (if (not (boundp 'project-switch-commands))
      (setq project-switch-commands nil)))

;; Treemacs project explorer
(use-package treemacs
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))
(use-package treemacs-evil)
(use-package treemacs-magit)
(use-package treemacs-projectile)
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; All-the-icons and treemacs integration
(use-package all-the-icons
  :if (or (daemonp) (display-graphic-p)))
(use-package treemacs-all-the-icons
  :if (or (daemonp) (display-graphic-p))
  :config
  (treemacs-load-theme "all-the-icons"))

;; Wait for packages
(elpaca-wait)


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

;; Hunspell is the spell checker
(setq ispell-program-name "hunspell")

;; Flycheck syntax checker
(setq flycheck-emacs-lisp-load-path 'inherit)
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Show ElDoc documentation in a child frame
(use-package eldoc
  :ensure nil)
(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-at-point-mode))

;; Company in-buffer completion engine
;; For future me: this is for code completion
(use-package company
  :config
  (global-company-mode))
(use-package company-box
  :hook (company-mode . company-box-mode))

;; Ivy completion engine
;; For future me: this is for M-x completion
(use-package ivy
  :config
  (ivy-mode)
  (ivy-define-key ivy-minibuffer-map (kbd "<S-return>") #'ivy-immediate-done))

;; Language Server Protocol support
(use-package eglot
  :ensure nil)

;; Undo-tree undo manager
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; EditorConfig plugin
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Show RGB color codes what they look like
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :init
  (setq rainbow-x-colors nil))

;; Use colorful delimiters because Lisp
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Always auto-insert matching delimiters
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Wait for packages
(elpaca-wait)


;;-------------------------------------------------------------------------------
;; Extra configurations
;;
;; Language-specific configuration or utilities go in a separate file.
;;-------------------------------------------------------------------------------

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
