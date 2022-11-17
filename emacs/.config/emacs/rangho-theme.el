;;; rangho-theme.el --- RangHo's Emacs theme

(deftheme rangho "RangHo's day-to-day Emacs theme")

;; ==============
;; User Interface
;; ==============

;; Disable unnecessary UI elements after loading
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Set default frame style
(setq default-frame-alist
      (append (list '(width . 80)
                    '(height . 25))))

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
;;   - Emoji(?) : I'm blue, da💨🎟️ ba🐋🐟 dee 🐦👖da📘🔷 ba💙💠 dai🌊🌀
(set-face-attribute 'default nil
                    :font "semteulche"
                    :height 110)
(set-fontset-font t 'hangul (font-spec :name "Noto Sans Mono CJK KR"))
(set-fontset-font t 'kana (font-spec :name "Noto Sans Mono CJK JP"))
(set-fontset-font t 'han (font-spec :name "Noto Sans Mono CJK CN"))
(set-fontset-font t 'unicode (font-spec :name "semteulche"))
(set-fontset-font t
                  (if (version< emacs-version "28.1")
                      '(#x1f300 . #x1fad0)
                    'emoji)
                  (font-spec :name "Noto Color Emoji"))

;; ===========
;; Colorscheme
;; ===========

;; Load values from pywal, if available
;; If colorscheme is not available, use a sensible "grayscale" as default
(if (file-exists-p (concat user-cache-directory "/wal/colors.el"))
    (require 'pywal-colors (concat user-cache-directory "/wal/colors.el") 'noerror)
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
;; Note: A plus sign(+) next to color name represents that
;; it is the "intense" variant of the color
(let ((color/fg       wal/foreground)
      (color/bg       wal/background)
      (color/cursor   wal/cursor)
      (color/black    wal/color0)
      (color/black+   wal/color8)
      (color/red      wal/color1)
      (color/red+     wal/color9)
      (color/green    wal/color2)
      (color/green+   wal/color10)
      (color/yellow   wal/color3)
      (color/yellow+  wal/color11)
      (color/blue     wal/color4)
      (color/blue+    wal/color12)
      (color/magenta  wal/color5)
      (color/magenta+ wal/color13)
      (color/cyan     wal/color6)
      (color/cyan+    wal/color14)
      (color/white    wal/color7)
      (color/white+   wal/color15))
  (custom-theme-set-faces
   'rangho

   ;; Basic UI elements
   `(default ((t (:foreground ,color/fg :background ,color/bg))))
   `(cursor ((t (:foreground ,color/bg :background ,color/cursor))))
   `(region ((t (:foreground ,color/black :background ,color/white))))
   `(highlight ((t (:background, color/black+))))
   `(button ((t (:underline t))))
   `(link ((t (:foreground ,color/blue+ :underline t :weight bold))))
   `(link-visited ((t (:foreground ,color/blue :underline t :weight normal))))
   `(success ((t (:foreground ,color/green :weight bold))))
   `(warning ((t :foreground ,color/yellow :weight bold))))

   ;; font-lock!
   `(font-lock-comment-face ((t (:foreground ,color/green))))
   `(font-lock-doc-face ((t (:foreground ,color/green+))))
   `(font-lock-keyword-face ((t (:foreground ,color/blue+))))
   `(font-lock-builtin-face ((t (:foreground ,color/blue))))
   `(font-lock-type-face ((t (:foreground ,color/cyan))))
   `(font-lock-string-face ((t (:foreground ,color/yellow+))))
   `(font-lock-variable-name-face ((t (:foreground ,color/yellow))))
   `(font-lock-constant-face ((t (:foreground ,color/magenta+))))
   `(font-lock-function-name-face ((t (:foreground ,color/magenta))))
   ))

(provide-theme 'rangho)
