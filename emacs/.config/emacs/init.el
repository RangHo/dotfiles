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

(defun rangho/detect-littered-eln-cache (location-hint)
  "Issue a warning with LOCATION-HINT as a tag if a rogue eln-cache directory was found."
  (let ((littered-eln-cache (expand-file-name "eln-cache" user-emacs-directory)))
    (when (file-exists-p littered-eln-cache)
      (warn "Found rogue eln-cache directory at around %s." littered-eln-cache))))


;;-------------------------------------------------------------------------------
;; Emacs behavior modification
;;
;; Default Emacs has its own quirks. Let's fix that.
;;-------------------------------------------------------------------------------

;; Apply theme without asking (coz I made it anyways)
(load-theme 'rangho t)
(rangho/detect-littered-eln-cache "theme")

(use-package mise
  :if (executable-find "mise")
  :ensure (:host github :repo "liuyinz/mise.el")
  :hook (after-init . global-mise-mode))

;; Enable Xterm mouse support if no windowing system is found
(unless (display-graphic-p)
  (xterm-mouse-mode))

;; Show fancy dashboard on startup
(rangho/detect-littered-eln-cache "dashboard")
(use-package dashboard
  :hook ((elpaca-after-init . dashboard-insert-startupify-lists)
         (elpaca-after-init . dashboard-initialize))
  :custom
  (dashboard-startup-banner (expand-file-name "usr/share/GNUEmacs.png" user-emacs-directory))
  (dashboard-image-banner-max-height 400)
  (dashboard-icon-type 'all-the-icons)
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda ()
                              (dashboard-refresh-buffer)
                              (get-buffer-create dashboard-buffer-name)))

;; Ivy completion engine
;; For future me: this is for M-x completion
(rangho/detect-littered-eln-cache "ivy")
(use-package ivy
  :config
  (ivy-mode)
  (ivy-define-key ivy-minibuffer-map (kbd "<S-return>") #'ivy-immediate-done))

;; Show ElDoc documentation in a child frame
(rangho/detect-littered-eln-cache "eldoc-box")
(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-at-point-mode))

;; Don't show warnings unless it's really important
(setq native-comp-async-report-warnings-errors nil)

;; Don't make lockfiles
(setq create-lockfiles nil)

;; y-or-n instead of yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Long-lost Emacs string manipulation library
(rangho/detect-littered-eln-cache "s)")
(use-package s)


;;-------------------------------------------------------------------------------
;; Keybindings
;;
;; Emacs is all about using keyboard. These packages make it even more enjoyable.
;;-------------------------------------------------------------------------------

;; Evil mode
(rangho/detect-littered-eln-cache "evil")
(use-package evil
  :after (undo-tree)
  :init
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1))
(rangho/detect-littered-eln-cache "evil-collection")
(use-package evil-collection
  :after (evil)
  :init
  (evil-collection-init))
(rangho/detect-littered-eln-cache "evil-surround")
(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode))

;; Hydra keybinding manager
(rangho/detect-littered-eln-cache "hydra)")
(use-package hydra)

;; God speed!
(rangho/detect-littered-eln-cache "god-mode)")
(use-package god-mode)

;; View which keybinding is available
(rangho/detect-littered-eln-cache "which-key")
(use-package which-key
  :config
  (which-key-mode))

;; Transient command menus
(rangho/detect-littered-eln-cache "transient")
(use-package transient
  :bind (:map transient-map ("q" . transient-quit-one)
         :map transient-edit-map ("q" . transient-quit-one)
         :map transient-sticky-map ("q" . transient-quit-seq)))


;;-------------------------------------------------------------------------------
;; Workspaces and projects
;;
;; Let's keep things nice and organized.
;;-------------------------------------------------------------------------------

;; Projectile project manager
(rangho/detect-littered-eln-cache "projectile")
(use-package projectile
  :config
  (projectile-mode 1))

;; Magit git repository manager
(rangho/detect-littered-eln-cache "magit")
(use-package magit
  :after (hydra projectile transient)
  :bind (("C-c g" . magit-file-dispatch))
  :init
  (if (not (boundp 'project-switch-commands))
      (setq project-switch-commands nil)))

;; Treemacs project explorer
(rangho/detect-littered-eln-cache "treemacs")
(use-package treemacs
  :after (hydra god-mode)
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
(rangho/detect-littered-eln-cache "treemacs-evil")
(use-package treemacs-evil
  :after (treemacs evil))
(rangho/detect-littered-eln-cache "treemacs-magit")
(use-package treemacs-magit
  :after (treemacs magit))
(rangho/detect-littered-eln-cache "treemacs-projectile")
(use-package treemacs-projectile
  :after (treemacs projectile))
(rangho/detect-littered-eln-cache "treemacs-icons-dired")
(use-package treemacs-icons-dired
  :after (treemacs dired)
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; All-the-icons and treemacs integration
(rangho/detect-littered-eln-cache "all-the-icons")
(use-package all-the-icons
  :if (or (daemonp) (display-graphic-p)))
(rangho/detect-littered-eln-cache "treemacs-all-the-icons")
(use-package treemacs-all-the-icons
  :if (or (daemonp) (display-graphic-p))
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons"))


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
(rangho/detect-littered-eln-cache "flycheck")
(use-package flycheck
  :init
  (global-flycheck-mode))

;; Language Server Protocol support
(rangho/detect-littered-eln-cache "eglot")
(use-package eglot
  :ensure nil)

;; Company in-buffer completion engine
;; For future me: this is for code completion
(rangho/detect-littered-eln-cache "company")
(use-package company
  :config
  (global-company-mode))
(rangho/detect-littered-eln-cache "company-box")
(use-package company-box
  :hook (company-mode . company-box-mode))

;; Undo-tree undo manager
(rangho/detect-littered-eln-cache "undo-tree")
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; EditorConfig plugin
(rangho/detect-littered-eln-cache "editorconfig")
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Use colorful delimiters because Lisp
(rangho/detect-littered-eln-cache "rainbow-delimiters")
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Always auto-insert matching delimiters
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Show RGB color codes what they look like
(rangho/detect-littered-eln-cache "rainbow-mode")
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
                             filename))
          (rangho/detect-littered-eln-cache filename))))
  (if (file-directory-p config-dir)
      (mapc load-user-config-file
            (mapcar 'concat
                    (directory-files config-dir nil "\\.el")))))

;; Load custom file
(when (file-exists-p custom-file)
  (load custom-file)
  (rangho/detect-littered-eln-cache custom-file))

(provide 'init)

;;; init.el ends here
