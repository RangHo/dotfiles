;;; rangho-theme.el --- RangHo's Emacs theme

;;; Commentary:

;; My Emacs theme.

;;; Code:

(deftheme rangho "RangHo's day-to-day Emacs theme")


;; ==============
;; User Interface
;; ==============

;; Set default frame style
(setq default-frame-alist
      (list '(width . 115) ; 80 col of editor + 35 col of treemacs
                    '(height . 25)
                    '(left-fringe . 0)
                    '(right-fringe . 0)
                    '(horizontal-scroll-bar . nil)
                    '(vertical-scroll-bar . nil)))

;; No stupid startup stuff
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Disable unnecessary UI elements after loading
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; No ugly checkboxes
(setq widget-image-enable nil)

;; Always show matching parentheses
(show-paren-mode t)

;; Enable visual bell
(setq visual-bell t)


;; ============
;; Font-related
;; ============
;; Emacs has weird way of implementing font lookup, so the function calls below cannot be
;; tampered, or things *will* break.
;;
;; Sample sentences are provided below for reference:
;;   - Alphabet : The quick brown fox jumps over the lazy dog.
;;   - Hangul   : 다람쥐 헌 쳇바퀴에 타고파.
;;   - Kana     : いろはにほへと ちりぬるを / わかよたれそ つねならむ
;;   - Emoji(?) : I'm blue, da💨🎟 ba🐋🐟 dee 🐦👖da📘🔷 ba💙💠 dai🌊🌀
(defun rangho/set-fontset-font ()
  "Set the default font to use throughout Emacs."
  (interactive)
  (set-face-attribute 'default nil
                      :font "semteulche"
                      :height 110)
  (set-face-attribute 'variable-pitch nil
                      :font "Noto Sans CJK KR"
                      :height 110)
  (set-fontset-font t 'hangul (font-spec :name "Noto Sans Mono CJK KR"))
  (set-fontset-font t 'kana (font-spec :name "Noto Sans Mono CJK JP"))
  (set-fontset-font t 'han (font-spec :name "Noto Sans Mono CJK CN"))
  (set-fontset-font t 'unicode (font-spec :name "semteulche"))
  (set-fontset-font t
                    ;; The `emoji' charset is introduced in Emacs 28.1
                    (if (version< emacs-version "28.1")
                        '(#x1f300 . #x1fad0)
                      'emoji)
                    (font-spec :name "Noto Color Emoji")))

(defvar rangho/serif-font-ignore-face-list
  '( ;; Texts highlighted with font-lock
    font-lock-builtin-face
    font-lock-comment-face
    font-lock-comment-delimiter-face
    font-lock-constant-face
    font-lock-doc-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-preprocessor-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    font-lock-string-face
    font-lock-type-face
    font-lock-variable-name-face
    font-lock-warning-face
    font-lock-error-face)
  "List of faces to ignore when setting serif font.")

(defun rangho/toggle-serif-font ()
  "Toggle serif font for the current buffer non-code related texts."
  (interactive)
  (when (display-graphic-p)
    (let ((serif-family "Noto Serif CJK KR")
          (serif-height 120)
          (default-family (face-attribute 'default :family))
          (default-height (face-attribute 'default :height)))
      (if (not (bound-and-true-p rangho/serif-font-enabled))
          (progn
            ;; Serif is not enabled now; enable it
            (make-local-variable 'rangho/serif-font-enabled)
            (make-local-variable 'rangho/serif-font-preserved)
            (setq rangho/serif-font-enabled
                  (face-remap-add-relative 'default
                                           :family serif-family
                                           :height serif-height))
            (setq rangho/serif-font-preserved nil)

            ;; Restore the default font for code-related texts
            (dolist (face rangho/serif-font-ignore-face-list)
              (add-to-list 'rangho/serif-font-preserved
                           (face-remap-add-relative face
                                                    :family default-family
                                                    :height default-height)))
            (message "Serif font enabled in the current buffer."))
        (progn
          ;; Serif is enabled; disable it
          (face-remap-remove-relative rangho/serif-font-enabled)
          (dolist (face rangho/serif-font-preserved)
            (face-remap-remove-relative face))
          (setq rangho/serif-font-enabled nil)
          (setq rangho/serif-font-preserved nil)
          (message "Serif font disabled in the current buffer."))))))

;; There are three cases:
;;   1. daemon mode,
;;   2. graphical mode,
;;   3. terminal mode.
(cond ((daemonp) (add-hook 'after-make-frame-functions
                           (lambda (frame)
                             (select-frame frame)
                             (if (display-graphic-p frame)
                                 (rangho/set-fontset-font)))))
      ((display-graphic-p) (rangho/set-fontset-font))
      (t nil))

;; Enable font ligatures
;; Emacs 27 has a problem dealing with ligatures.
;; It's not worth trying to workaround this issue as I have to do make significant
;; changes that I'd rather compile Emacs myself.
(unless (= emacs-major-version 27)
  (use-package ligature
    :config
    (global-ligature-mode)))


;; ===========
;; Colorscheme
;; ===========

;; Load values from pywal, if available
;; If colorscheme is not available, use a sensible "grayscale" as default
(if (file-exists-p (concat user-cache-directory "wal/colors.el"))
    (require 'pywal-colors (concat user-cache-directory "wal/colors.el") 'noerror)
  (setq-default wal/foreground "#f7f7f7"
                wal/background "#101010"
                wal/cursor "#f7f7f7"
                wal/color0 "#101010"
                wal/color1 "#7c7c7c"
                wal/color2 "#5c5c5c"
                wal/color3 "#a0a0a0"
                wal/color4 "#686868"
                wal/color5 "#747474"
                wal/color6 "#868686"
                wal/color7 "#b9b9b9"
                wal/color8 "#525252"
                wal/color9 "#b0b0b0"
                wal/color10 "#8f8f8f"
                wal/color11 "#d4d4d4"
                wal/color12 "#9c9c9c"
                wal/color13 "#a6a6a6"
                wal/color14 "#bababa"
                wal/color15 "#f7f7f7"))

;; Map color values to their representative colors
;; Note: An `i' next to color name represents that
;; it is the "intense" variant of the color
(defvar rangho/color-name-alist
  `((color/fg        . ,wal/foreground)
    (color/bg        . ,wal/background)
    (color/cursor    . ,wal/cursor)
    (color/black     . ,wal/color0)
    (color/blacki    . ,wal/color8)
    (color/red       . ,wal/color1)
    (color/redi      . ,wal/color9)
    (color/green     . ,wal/color2)
    (color/greeni    . ,wal/color10)
    (color/yellow    . ,wal/color3)
    (color/yellowi   . ,wal/color11)
    (color/blue      . ,wal/color4)
    (color/bluei     . ,wal/color12)
    (color/magenta   . ,wal/color5)
    (color/magentai  . ,wal/color13)
    (color/cyan      . ,wal/color6)
    (color/cyani     . ,wal/color14)
    (color/white     . ,wal/color7)
    (color/whitei    . ,wal/color15))
  "Association list to map wal colors to friendly names.")

;; Load color utility
(require 'color)

;; List of shades to generate for each colors
(defvar rangho/color-shade-list
  (number-sequence -20 20 5)
  "List of percentage values to lighten/darken the original colors.")

;; Color manipulation utility
(defun color-hex-to-rgb (color)
  "Decompose hex representation of a COLOR to 3-tuple (r, g, b)."
  (list (/ (string-to-number (substring color 1 3) 16) 255.0)
        (/ (string-to-number (substring color 3 5) 16) 255.0)
        (/ (string-to-number (substring color 5) 16) 255.0)))

(defun color-lighten-hex (color percent)
  "Lighten or darken an RGB color string COLOR by PERCENT."
  (let* ((rgb (color-hex-to-rgb color))
         (hsl (apply 'color-rgb-to-hsl rgb))
         (hsl2 (apply 'color-lighten-hsl (nconc hsl `(,percent))))
         (rgb2 (apply 'color-hsl-to-rgb hsl2)))
    (apply 'color-rgb-to-hex (nconc rgb2 '(2)))))

;; Color shade creation utility
(defun rangho/create-color-matrix-alist (color-alist lighten-list)
  "Create a matrix of colors based on a COLOR-ALIST and LIGHTEN-LIST."
  ;; For each color association...
  (append color-alist
          (mapcan (lambda (original-color)
                    ;; And then for each lighten value...
                    (mapcar (lambda (lighten-value)
                              ;; Create a list of...
                              (cons (intern (concat (symbol-name (car original-color))
                                                    (if (>= lighten-value 0) "+" "")
                                                    (number-to-string lighten-value)))
                                    (color-lighten-hex (cdr original-color) lighten-value)))
                            lighten-list))
                  color-alist)))

;; Create a color matrix
(defvar rangho/color-matrix-alist
  (rangho/create-color-matrix-alist rangho/color-name-alist
                                    rangho/color-shade-list))

;; Apply the color theme
(require 'let-alist)
(let-alist rangho/color-matrix-alist
  (custom-theme-set-faces
   'rangho

   ;; Base faces

   ;; Basic UI elements
   `(default      ((t (:foreground ,.color/fg :background ,.color/bg))))
   `(cursor       ((t (:foreground ,.color/bg :background ,.color/cursor))))
   `(region       ((t (:foreground ,.color/bg :background ,.color/fg))))
   `(highlight    ((t (:background ,.color/blacki))))
   `(button       ((t (:underline t))))
   `(link         ((t (:foreground ,.color/bluei :underline t :weight bold))))
   `(link-visited ((t (:foreground ,.color/blue :underline t :weight normal))))
   `(success      ((t (:foreground ,.color/green :weight bold))))
   `(warning      ((t (:foreground ,.color/yellow :weight bold))))

   ;; Modeline configuration
   `(mode-line           ((t (:foreground ,.color/bg-10 :background ,.color/fg+10 :box (:line-width 3 :color ,.color/fg+10 :style nil)))))
   `(mode-line-inactive  ((t (:foreground ,.color/fg-10 :background ,.color/bg+10 :box (:line-width 3 :color ,.color/bg+10 :style nil)))))
   `(mode-line-highlight ((t (:inherit highlight))))
   `(mode-line-emphasis  ((t (:weight regular))))
   `(mode-line-buffer-id ((t (:weight regular))))
   
   ;; font-lock!
   `(font-lock-comment-face ((t (:foreground ,.color/green))))
   `(font-lock-doc-face ((t (:foreground ,.color/greeni))))
   `(font-lock-keyword-face ((t (:foreground ,.color/bluei))))
   `(font-lock-builtin-face ((t (:foreground ,.color/blue))))
   `(font-lock-type-face ((t (:foreground ,.color/cyan))))
   `(font-lock-string-face ((t (:foreground ,.color/yellowi))))
   `(font-lock-variable-name-face ((t (:foreground ,.color/yellow))))
   `(font-lock-constant-face ((t (:foreground ,.color/magentai))))
   `(font-lock-function-name-face ((t (:foreground ,.color/magenta))))))

(provide-theme 'rangho)

;;; rangho-theme.el ends here
