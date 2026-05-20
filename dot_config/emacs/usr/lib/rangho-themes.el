;;; rangho-themes.el --- A custom base theme based on Material Design 3 color system

;; Copyright (C) 2026 RangHo Lee

;; Author: RangHo Lee <hello@rangho.me>
;; URL: https://github.com/RangHo/dotfiles
;; Package-Requires: ((emacs "30.2") (compat "31.0.0.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'compat)
(require 'let-alist)

(defgroup rangho-themes nil
  "Custom base theme for Emacs."
  :group 'local
  :prefix "rangho-themes-")

(defconst rangho-themes-material-color-roles
  '(   primary                             secondary                               tertiary                              error
    on-primary                          on-secondary                            on-tertiary                           on-error
       primary-container                   secondary-container                     tertiary-container                    error-container
    on-primary-container                on-secondary-container                  on-tertiary-container                 on-error-container
    surface
    surface-container-lowest surface-container-low surface-container surface-container-high surface-container-highest
    on-surface on-surface-variant outline outline-variant
    inverse-surface inverse-on-surface inverse-primary
    scrim shadow)
  "List of color roles specified by Material Design 3.")

(defun rangho-themes--hex-to-rgb (color)
  "Decompose hexadecimal representation of COLOR to 3-tuple (R G B)."
  (list (/ (string-to-number (substring color 1 3) 16) 255.0)
        (/ (string-to-number (substring color 3 5) 16) 255.0)
        (/ (string-to-number (substring color 5) 16) 255.0)))

(defun rangho-themes-blend (a b &optional alpha)
  "Blend the two hexadecimal colors A and B in linear space with ALPHA."
  (let ((alpha (or alpha 0.5))
        (a-rgb (rangho-themes--hex-to-rgb a))
        (b-rgb (rangho-themes--hex-to-rgb b)))
    (apply #'color-rgb-to-hex
           (color-blend a-rgb b-rgb alpha))))

(defun rangho-themes-lighten (color alpha)
  "Lighten the hexadecimal COLOR by blending it with white using ALPHA."
  (rangho-themes-blend color "#FFFFFF" alpha))

(defun rangho-themes-darken (color alpha)
  "Darken the hexadecimal COLOR by blending it with black using ALPHA."
  (rangho-themes-blend color "#000000" alpha))

(defun rangho-themes-set-faces (name role-color-alist &rest overrides)
  "Set the faces for theme named NAME with ROLE-COLOR-ALIST, overriding faces specified in OVERRIDES."
  ;; Make sure that all required roles are defined.
  (let-alist role-color-alist
    (custom-theme-set-faces
     name

     ;; Basic faces.
     `(border                     ((t (:background ,.surface-container :foreground ,.surface-container))))
     `(cursor                     ((t (:background ,.primary))))
     `(default                    ((t (:background ,.surface :foreground ,.on-surface))))
     `(default-italic             ((t (:background ,.surface :foreground ,.on-surface :slant italic))))
     `(error                      ((t (:foreground ,.error))))
     `(fringe                     ((t (:background ,.surface :foreground ,.outline-variant))))
     `(header-line                ((t (:background ,.surface-container-highest :foreground ,.on-surface :box nil))))
     `(highlight                  ((t (:background ,.surface-container))))
     `(isearch                    ((t (:background ,.tertiary-container :foreground ,.on-tertiary-container  :weight bold))))
     `(lazy-highlight             ((t (:background ,.secondary-container   :foreground ,.on-secondary-container))))
     `(link                       ((t (:foreground ,.primary   :underline t))))
     `(link-visited               ((t (:foreground ,.tertiary  :underline t))))
     `(match                      ((t (:background ,.tertiary-container   :foreground ,.on-tertiary-container))))
     `(region                     ((t (:background ,.primary-container     :foreground ,.on-primary-container   :extend t))))
     `(secondary-selection        ((t (:background ,.secondary-container   :foreground ,.on-secondary-container  :extend t))))
     `(shadow                     ((t (:foreground ,.outline-variant))))
     `(success                    ((t (:foreground ,.tertiary))))
     `(vertical-border            ((t (:foreground ,.surface-container))))
     `(warning                    ((t (:foreground ,.secondary))))
     `(window-divider             ((t (:foreground ,.outline))))
     `(window-divider-first-pixel ((t (:foreground ,.outline-variant))))
     `(window-divider-last-pixel  ((t (:foreground ,.outline-variant))))

     ;; Font-lock faces.
     `(font-lock-builtin-face           ((t (:foreground ,.primary :weight bold))))
     `(font-lock-comment-face           ((t (:foreground ,.outline :slant italic))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,.outline-variant))))
     `(font-lock-constant-face          ((t (:foreground ,.tertiary :weight bold))))
     `(font-lock-doc-face               ((t (:foreground ,.on-surface-variant :slant italic))))
     `(font-lock-function-name-face     ((t (:foreground ,.secondary :weight bold))))
     `(font-lock-keyword-face           ((t (:foreground ,.primary :weight bold))))
     `(font-lock-string-face            ((t (:foreground ,.tertiary))))
     `(font-lock-type-face              ((t (:foreground ,.secondary))))
     `(font-lock-variable-name-face     ((t (:foreground ,.on-surface))))
     `(font-lock-warning-face           ((t (:foreground ,.error :weight bold))))
     `(font-lock-preprocessor-face      ((t (:foreground ,.primary-container))))
     `(font-lock-negation-char-face     ((t (:foreground ,.tertiary))))

     ;; Show paren.
     `(show-paren-match    ((t (:background ,.primary-container :foreground ,.on-primary-container :weight bold))))
     `(show-paren-mismatch ((t (:background ,.error-container   :foreground ,.on-error-container   :weight bold))))

     ;; Mode line.
     `(mode-line           ((t (:background ,.surface-container-highest :foreground ,.on-surface :box nil))))
     `(mode-line-inactive  ((t (:background ,.surface :foreground ,.on-surface-variant :box nil))))
     `(mode-line-buffer-id ((t (:foreground ,.primary :weight bold))))
     `(mode-line-emphasis  ((t (:foreground ,.primary :weight bold))))
     `(mode-line-highlight ((t (:foreground ,.primary :box nil))))

     ;; Org mode.
     `(org-block            ((t (:background ,.surface-container-low                               :extend t :inherit fixed-pitch))))
     `(org-block-begin-line ((t (:background ,.surface-container-low :foreground ,.primary-container :extend t :slant italic :inherit fixed-pitch))))
     `(org-block-end-line   ((t (:background ,.surface-container-low :foreground ,.primary-container :extend t :slant italic :inherit fixed-pitch))))
     `(org-code             ((t (:background ,.surface-container-low :foreground ,.tertiary          :inherit fixed-pitch))))
     `(org-verbatim         ((t (:background ,.surface-container-low :foreground ,.primary           :inherit fixed-pitch))))
     `(org-meta-line        ((t (:foreground ,.outline         :slant italic))))
     `(org-level-1          ((t (:foreground ,.primary         :weight bold :height 1.2))))
     `(org-level-2          ((t (:foreground ,.secondary       :weight bold :height 1.1))))
     `(org-level-3          ((t (:foreground ,.tertiary        :weight bold))))
     `(org-level-4          ((t (:foreground ,.primary         :weight bold))))
     `(org-level-5          ((t (:foreground ,.secondary       :weight bold))))
     `(org-level-6          ((t (:foreground ,.tertiary        :weight bold))))
     `(org-level-7          ((t (:foreground ,.primary         :weight bold))))
     `(org-level-8          ((t (:foreground ,.secondary       :weight bold))))
     `(org-document-title   ((t (:foreground ,.primary         :weight bold :height 1.3))))
     `(org-document-info    ((t (:foreground ,.primary-container))))
     `(org-todo             ((t (:foreground ,.error           :weight bold))))
     `(org-done             ((t (:foreground ,.tertiary        :weight bold))))
     `(org-headline-done    ((t (:foreground ,.on-surface-variant))))
     `(org-hide             ((t (:foreground ,.surface))))
     `(org-ellipsis         ((t (:foreground ,.tertiary        :underline nil))))
     `(org-table            ((t (:foreground ,.secondary       :inherit fixed-pitch))))
     `(org-formula          ((t (:foreground ,.tertiary        :inherit fixed-pitch))))
     `(org-checkbox         ((t (:foreground ,.primary         :weight bold :inherit fixed-pitch))))
     `(org-date             ((t (:foreground ,.secondary       :underline t))))
     `(org-special-keyword  ((t (:foreground ,.on-surface-variant :slant italic))))
     `(org-tag              ((t (:foreground ,.outline         :weight normal))))

     ;; Magit.
     `(magit-section-highlight        ((t (:background ,.surface-container-low))))
     `(magit-diff-hunk-heading        ((t (:background ,.surface-container     :foreground ,.on-surface-variant))))
     `(magit-diff-hunk-heading-highlight ((t (:background ,.surface-container-high :foreground ,.on-surface))))
     `(magit-diff-context             ((t (:foreground ,.on-surface-variant))))
     `(magit-diff-context-highlight   ((t (:background ,.surface-container-low :foreground ,.on-surface))))
     `(magit-diff-added               ((t (:background ,.tertiary-container    :foreground ,.on-tertiary-container))))
     `(magit-diff-added-highlight     ((t (:background ,.tertiary-container    :foreground ,.on-tertiary-container :weight bold))))
     `(magit-diff-removed             ((t (:background ,.error-container       :foreground ,.on-error-container))))
     `(magit-diff-removed-highlight   ((t (:background ,.error-container       :foreground ,.on-error-container   :weight bold))))
     `(magit-hash                     ((t (:foreground ,.outline))))
     `(magit-branch-local             ((t (:foreground ,.tertiary :weight bold))))
     `(magit-branch-remote            ((t (:foreground ,.primary  :weight bold))))

     ;; Company.
     `(company-tooltip                  ((t (:background ,.surface-container   :foreground ,.on-surface))))
     `(company-tooltip-selection        ((t (:background ,.primary-container   :foreground ,.on-primary-container))))
     `(company-tooltip-common           ((t (:foreground ,.primary))))
     `(company-tooltip-common-selection ((t (:foreground ,.on-primary-container :weight bold))))
     `(company-tooltip-annotation       ((t (:foreground ,.tertiary))))
     `(company-scrollbar-fg             ((t (:background ,.primary))))
     `(company-scrollbar-bg             ((t (:background ,.surface-container))))
     `(company-preview                  ((t (:foreground ,.on-surface-variant  :slant italic))))
     `(company-preview-common           ((t (:foreground ,.primary             :slant italic))))

     ;; Ido.
     `(ido-first-match ((t (:foreground ,.primary   :weight bold))))
     `(ido-only-match  ((t (:foreground ,.tertiary  :weight bold))))
     `(ido-subdir      ((t (:foreground ,.secondary))))
     `(ido-indicator   ((t (:foreground ,.error))))
     `(ido-virtual     ((t (:foreground ,.outline))))

     ;; Helm.
     `(helm-selection        ((t (:background ,.primary-container      :foreground ,.on-primary-container))))
     `(helm-match            ((t (:foreground ,.primary    :weight bold))))
     `(helm-source-header    ((t (:background ,.surface-container-high :foreground ,.primary :weight bold :height 1.1))))
     `(helm-candidate-number ((t (:foreground ,.tertiary   :weight bold))))
     `(helm-ff-directory     ((t (:foreground ,.primary    :weight bold))))
     `(helm-ff-file          ((t (:foreground ,.on-surface))))
     `(helm-ff-executable    ((t (:foreground ,.tertiary))))

     ;; Corfu.
     `(corfu-default ((t (:background ,.surface-container  :foreground ,.on-surface))))
     `(corfu-current ((t (:background ,.primary-container  :foreground ,.on-primary-container))))

     ;; Which-key.
     `(which-key-key-face                 ((t (:foreground ,.primary  :weight bold))))
     `(which-key-separator-face           ((t (:foreground ,.outline-variant))))
     `(which-key-command-description-face ((t (:foreground ,.on-surface))))
     `(which-key-group-description-face   ((t (:foreground ,.secondary))))
     `(which-key-special-key-face         ((t (:foreground ,.tertiary :weight bold))))

     ;; Line numbers.
     `(line-number              ((t (:foreground ,.outline-variant :inherit fixed-pitch))))
     `(line-number-current-line ((t (:foreground ,.primary         :weight bold :inherit fixed-pitch))))

     ;; Smartparens.
     `(sp-show-pair-match-face    ((t (:background ,.primary-container :foreground ,.on-primary-container))))
     `(sp-show-pair-mismatch-face ((t (:background ,.error-container   :foreground ,.on-error-container))))

     ;; Rainbow delimiters.
     `(rainbow-delimiters-depth-1-face    ((t (:foreground ,.primary))))
     `(rainbow-delimiters-depth-2-face    ((t (:foreground ,.secondary))))
     `(rainbow-delimiters-depth-3-face    ((t (:foreground ,.tertiary))))
     `(rainbow-delimiters-depth-4-face    ((t (:foreground ,.primary-container))))
     `(rainbow-delimiters-depth-5-face    ((t (:foreground ,.secondary-container))))
     `(rainbow-delimiters-depth-6-face    ((t (:foreground ,.tertiary-container))))
     `(rainbow-delimiters-depth-7-face    ((t (:foreground ,.surface-container-highest))))
     `(rainbow-delimiters-depth-8-face    ((t (:foreground ,.surface-container-high))))
     `(rainbow-delimiters-depth-9-face    ((t (:foreground ,.surface-container))))
     `(rainbow-delimiters-mismatched-face ((t (:foreground ,.error :weight bold))))
     `(rainbow-delimiters-unmatched-face  ((t (:foreground ,.error :weight bold))))

     ;; Dired.
     `(dired-directory ((t (:foreground ,.primary         :weight bold))))
     `(dired-ignored   ((t (:foreground ,.outline-variant))))
     `(dired-flagged   ((t (:foreground ,.error))))
     `(dired-marked    ((t (:foreground ,.tertiary        :weight bold))))
     `(dired-symlink   ((t (:foreground ,.secondary       :slant italic))))
     `(dired-header    ((t (:foreground ,.primary         :weight bold :height 1.1))))

     ;; Terminal colors.
     `(term-color-black   ((t (:foreground ,.surface :background ,.surface))))
     `(term-color-red     ((t (:foreground ,.error :background ,.error))))
     `(term-color-green   ((t (:foreground ,.tertiary :background ,.tertiary))))
     `(term-color-yellow  ((t (:foreground ,.secondary :background ,.secondary))))
     `(term-color-blue    ((t (:foreground ,.primary :background ,.primary))))
     `(term-color-magenta ((t (:foreground ,.tertiary-container :background ,.tertiary-container))))
     `(term-color-cyan    ((t (:foreground ,.secondary-container :background ,.secondary-container))))
     `(term-color-white   ((t (:foreground ,.on-surface :background ,.on-surface))))

     ;; EShell.
     `(eshell-prompt        ((t (:foreground ,.primary              :weight bold))))
     `(eshell-ls-directory  ((t (:foreground ,.primary              :weight bold))))
     `(eshell-ls-symlink    ((t (:foreground ,.secondary            :slant italic))))
     `(eshell-ls-executable ((t (:foreground ,.tertiary))))
     `(eshell-ls-archive    ((t (:foreground ,.on-tertiary-container))))
     `(eshell-ls-backup     ((t (:foreground ,.outline-variant))))
     `(eshell-ls-clutter    ((t (:foreground ,.error))))
     `(eshell-ls-missing    ((t (:foreground ,.error))))
     `(eshell-ls-product    ((t (:foreground ,.on-surface-variant))))
     `(eshell-ls-readonly   ((t (:foreground ,.on-surface-variant))))
     `(eshell-ls-special    ((t (:foreground ,.secondary))))
     `(eshell-ls-unreadable ((t (:foreground ,.outline-variant))))

     ;; Markdown.
     `(markdown-header-face      ((t (:foreground ,.primary             :weight bold))))
     `(markdown-header-face-1    ((t (:foreground ,.primary             :weight bold :height 1.2))))
     `(markdown-header-face-2    ((t (:foreground ,.primary-container   :weight bold :height 1.1))))
     `(markdown-header-face-3    ((t (:foreground ,.secondary           :weight bold))))
     `(markdown-header-face-4    ((t (:foreground ,.secondary-container :weight bold))))
     `(markdown-inline-code-face ((t (:foreground ,.tertiary :background ,.surface-container-low :inherit fixed-pitch))))
     `(markdown-code-face        ((t (:background ,.surface-container-low :extend t :inherit fixed-pitch))))
     `(markdown-pre-face         ((t (:background ,.surface-container-low :inherit fixed-pitch))))
     `(markdown-table-face       ((t (:foreground ,.secondary :inherit fixed-pitch))))

     ;; Web mode.
     `(web-mode-html-tag-face         ((t (:foreground ,.primary))))
     `(web-mode-html-tag-bracket-face ((t (:foreground ,.on-surface-variant))))
     `(web-mode-html-attr-name-face   ((t (:foreground ,.secondary))))
     `(web-mode-html-attr-value-face  ((t (:foreground ,.tertiary))))
     `(web-mode-css-selector-face     ((t (:foreground ,.primary))))
     `(web-mode-css-property-name-face ((t (:foreground ,.secondary))))
     `(web-mode-css-string-face       ((t (:foreground ,.tertiary))))

     ;; Flycheck.
     `(flycheck-error         ((t (:underline (:style wave :color ,.error)))))
     `(flycheck-warning       ((t (:underline (:style wave :color ,.secondary)))))
     `(flycheck-info          ((t (:underline (:style wave :color ,.tertiary)))))
     `(flycheck-fringe-error  ((t (:foreground ,.error))))
     `(flycheck-fringe-warning ((t (:foreground ,.secondary))))
     `(flycheck-fringe-info   ((t (:foreground ,.tertiary))))

     ;; Minibuffer.
     `(minibuffer-prompt ((t (:foreground ,.primary :weight bold))))

     ;; LSP highlight.
     `(lsp-face-highlight-textual ((t (:background ,.primary-container   :foreground ,.on-primary-container   :weight bold))))
     `(lsp-face-highlight-read    ((t (:background ,.secondary-container :foreground ,.on-secondary-container :weight bold))))
     `(lsp-face-highlight-write   ((t (:background ,.tertiary-container  :foreground ,.on-tertiary-container  :weight bold))))

     ;; Info.
     `(info-title-1    ((t (:foreground ,.primary             :weight bold :height 1.3))))
     `(info-title-2    ((t (:foreground ,.primary-container   :weight bold :height 1.2))))
     `(info-title-3    ((t (:foreground ,.secondary           :weight bold :height 1.1))))
     `(info-title-4    ((t (:foreground ,.secondary-container :weight bold))))
     `(Info-quoted     ((t (:foreground ,.tertiary))))
     `(info-menu-header ((t (:foreground ,.primary  :weight bold))))
     `(info-menu-star  ((t (:foreground ,.primary))))
     `(info-node       ((t (:foreground ,.tertiary :weight bold))))

     ;; Tab bar.
     `(tab-bar              ((t (:background ,.surface-container-high :foreground ,.on-surface         :box nil))))
     `(tab-bar-tab          ((t (:background ,.surface-container-high :foreground ,.on-surface         :weight bold :box nil))))
     `(tab-bar-tab-inactive ((t (:background ,.surface               :foreground ,.on-surface-variant :box nil))))

     ;; Tab line.
     `(tab-line              ((t (:background ,.surface-container-high    :foreground ,.on-surface         :box nil))))
     `(tab-line-tab          ((t (:background ,.surface                   :foreground ,.on-surface-variant :box nil))))
     `(tab-line-tab-current  ((t (:background ,.surface-container-high    :foreground ,.on-surface         :weight bold :box nil))))
     `(tab-line-tab-inactive ((t (:background ,.surface                   :foreground ,.on-surface-variant :box nil))))
     `(tab-line-highlight    ((t (:background ,.surface-container-highest :foreground ,.on-surface))))

     ;; Centaur tabs.
     `(centaur-tabs-default            ((t (:background ,.surface-container-high :foreground ,.on-surface))))
     `(centaur-tabs-selected           ((t (:background ,.surface-container-high :foreground ,.on-surface         :weight bold))))
     `(centaur-tabs-unselected         ((t (:background ,.surface               :foreground ,.on-surface-variant))))
     `(centaur-tabs-selected-modified  ((t (:background ,.surface-container-high :foreground ,.tertiary           :weight bold))))
     `(centaur-tabs-unselected-modified ((t (:background ,.surface              :foreground ,.tertiary))))
     `(centaur-tabs-active-bar-face    ((t (:background ,.primary)))))
    (apply #'custom-theme-set-faces name overrides)))

(provide 'rangho-themes)
;;; rangho-themes.el ends here
