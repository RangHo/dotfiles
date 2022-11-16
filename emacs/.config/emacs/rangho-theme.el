;;; rangho-theme.el --- RangHo's Emacs theme

(deftheme rangho "RangHo's day-to-day Emacs theme")

;; Load values from pywal, if available
(if (file-exists-p (concat user-cache-directory "/wal/colors.el"))
    (require 'pywal-colors (concat user-cache-directory "/wal/colors.el") 'noerror))

;; Disable unnecessary UI elements after loading
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; Font-related configurations
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


(provide-theme 'rangho)
